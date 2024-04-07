#![feature(slice_ptr_get)]
use compiler::{
    ast::{Flag, Program, TargetArch, TypeId},
    bc::Value,
    compiler::{Compile, ExecTime},
    experiments::{bc_to_asm::BcToAsm, emit_rust::bootstrap},
    export_ffi::{get_include_std, STDLIB_PATH},
    find_std_lib,
    interp::Interp,
    load_program, log_dbg, log_err,
    logging::{init_logs, init_logs_flag, LogTag},
    outln,
    pool::StringPool,
    run_main, timestamp,
};
#[cfg(feature = "llvm")]
use llvm_backend::{verify_module, BcToLlvm};
use std::{arch::asm, env, fs, io::Write, mem::transmute, ops::DerefMut, path::PathBuf, process::exit};

// TODO: Instead of cli args, what if the arg was a string of code to run so 'franca "start_lsp()"' would concat that on some compiler_cli.txt and run it.
//       Make sure theres some prefix that lets you run/compile the next arg as a file path for shabang line.
//       Maybe that implies I should have syntax for a call that taking the rest of the line as an argument without needing a close paren.
//       Or just passs remaining cli args as args of a function.
//       Because it seems nicer if things are composable and the compiler functions don't assume they can just read the args except the top level one which you can define in userland.
// TODO: repl(). works nicely with ^ so you could experiment but then always be able to run things as a command when working with other tools
fn main() {
    // TODO: option to hardcode path
    // TODO: its a problem that someone could add one at a higher prio place maybe?
    if find_std_lib() {
        // TODO: option to silence
        eprintln!("Found installed lib at {:?}", STDLIB_PATH.lock().unwrap().as_ref().unwrap());
    } else {
        // TODO: show search locations
        eprintln!("Standard library not found.");
        exit(1);
    }
    if let Some(name) = env::args().nth(1) {
        if name == "bootstrap" {
            let (rs, fr) = bootstrap();
            fs::write("target/bootstrap_gen.rs", rs).unwrap();
            fs::write("target/aarch64_basic.gen.fr", fr).unwrap();
            return;
        }
        if name == "log_export_ffi" {
            println!("{}", get_include_std("compiler").unwrap());
            println!("{}", get_include_std("libc").unwrap());
            println!("{}", get_include_std("compiler_late").unwrap());
            return;
        }
        if name == "lsp" {
            #[cfg(feature = "lsp")]
            {
                lsp::run_lsp_blocking().unwrap();
            }
            if cfg!(not(feature = "lsp")) {
                println!("lsp feature flag disabled.")
            }
            return;
        }

        let pool = Box::leak(Box::<StringPool>::default());
        let path = PathBuf::from(format!("tests/{name}.fr"));
        if path.exists() {
            init_logs_flag(0xFFFFFFFFF);
            run_main(
                pool,
                fs::read_to_string(format!("tests/{name}.fr")).unwrap(),
                Value::I64(0),
                Value::I64(0),
                Some(&name),
                Box::new(Interp::new(pool)),
                true,
                Box::new(Interp::new(pool)),
            );
        } else {
            init_logs(&[LogTag::Scope, LogTag::ShowPrint]);
            run_main(
                pool,
                fs::read_to_string(&name).unwrap(),
                Value::I64(0),
                Value::I64(0),
                Some(&name),
                Box::new(Interp::new(pool)),
                true,
                Box::new(Interp::new(pool)),
            );
        }
    } else {
        run_tests();
    }
}

/// The normal cargo test harness combines the tests of a package into one exe and catches panics to continue after one fails.
/// It doesn't expect you do be causing segfault/illegal instruction signals, (which is generally a reasonable assumption but less so for my compiler sadly).
/// So when a test does that it kills the whole thing instead of being recorded as a failing test and continuing.
/// Instead, I start each test in its own process so no matter what happens, it can't interfear with the other tests
/// (unless they're like trying to use the write files or something, which like... don't do that then I guess).
/// My way has higher overhead per test but it feels worth it.
fn run_tests() {
    if !PathBuf::from("tests").exists() {
        eprint!("Directory 'tests' does not exist in cwd");
        eprint!("run 'franca <path>' to run a specific file instead.");
        exit(1);
    }
    let start = timestamp();
    let max_jobs = 7; // not really max cause one file can add multiple.
    let mut passed = 0;
    let mut failed = 0;
    let mut jobs = vec![];
    let mut files: Vec<_> = fs::read_dir("tests").unwrap().collect();

    while !files.is_empty() || !jobs.is_empty() {
        if jobs.len() < max_jobs && !files.is_empty() {
            let case = files.pop().unwrap().unwrap();
            let name = case.file_name();
            let name = name.to_str().unwrap().strip_suffix(".fr").unwrap();
            let src = fs::read_to_string(case.path()).unwrap();
            add_test_cases(name.to_string(), src, &mut jobs);
        } else {
            assert!(!jobs.is_empty());
            let mut status = 0i32;
            let pid = unsafe { libc::wait(&mut status) };

            let the_job = jobs.iter().position(|j| j.2 == pid).expect("wait returned unexpected pid");
            let (name, arch, _) = jobs.remove(the_job);

            if status == 0 {
                // TODO: have it return 'assertion_count' somehow so the tested program calling exit(0) doesn't get seen as a pass.

                // TODO: should have this colour stuff as a library in my language.
                //       https://en.wikipedia.org/wiki/ANSI_escape_code#24-bit
                std::io::stdout().write_all(&[27]).unwrap();
                print!("[38;2;{};{};{}m", 0, 255, 0);
                println!("[PASSED: {} {:?}]", name, arch);
                std::io::stdout().write_all(&[27]).unwrap();
                print!("[0m");
                passed += 1;
            } else {
                std::io::stdout().write_all(&[27]).unwrap();
                print!("[38;2;{};{};{}m", 255, 0, 0);
                println!("[FAILED: {} {:?}]^^^", name, arch);
                std::io::stdout().write_all(&[27]).unwrap();
                print!("[0m");
                failed += 1;
            }
        }
    }

    let end = timestamp();
    let seconds = end - start;
    println!("Passed {}/{} Tests in {} ms.", passed, passed + failed, (seconds * 1000.0) as i64);
    assert_eq!(failed, 0);
}

fn add_test_cases(name: String, src: String, jobs: &mut Vec<(String, TargetArch, i32)>) {
    let start = src.find("@test(").expect("@test in test file") + 6;
    let s = &src[start..];
    let end = s.find(')').unwrap();
    let mut passed = 0;
    let mut failed = 0;
    let assertion_count = src.split("assert_eq(").count() - 1;
    for backend in s[..end].split(", ") {
        let arch = match backend {
            "interp" => TargetArch::Interp,
            "aarch64" => TargetArch::Aarch64,
            #[cfg(not(feature = "llvm"))]
            "llvm" => continue,
            #[cfg(feature = "llvm")]
            "llvm" => TargetArch::Llvm,
            "skip" => break,
            other => panic!("Unknown backend {other}"),
        };

        match unsafe { libc::fork() } {
            -1 => panic!("Fork Failed"),
            0 => actually_run_it(name, src, assertion_count, arch),
            pid => {
                jobs.push((name.clone(), arch, pid));
            }
        }
    }
}

/// This is the thing we exec.
fn actually_run_it(name: String, src: String, assertion_count: usize, arch: TargetArch) -> ! {
    init_logs(&[LogTag::ShowPrint, LogTag::ShowErr]);
    let pool = Box::leak(Box::<StringPool>::default());
    let start = timestamp();
    let mut program = Program::new(pool, TargetArch::Interp, arch);
    let mut comp = Compile::new(pool, &mut program, Box::new(Interp::new(pool)), Box::new(Interp::new(pool)));
    let result = load_program(&mut comp, &src);
    if let Err(e) = result {
        log_err(&comp, e, None);
        exit(1);
    }
    let (_, lines) = result.unwrap();
    let f = comp.program.find_unique_func(Flag::Main.ident());
    if f.is_none() {
        println!("'fn main' NOT FOUND");
        log_dbg(&comp, None);
        exit(1);
    }
    let f = f.unwrap();
    let result = comp.compile(f, ExecTime::Runtime);
    if let Err(e) = result {
        log_err(&comp, e, None);
        exit(1);
    }

    assert_eq!(comp.program[f].finished_ret, Some(TypeId::i64()));
    assert_eq!(comp.program[f].finished_arg, Some(TypeId::i64()));
    let arg = Value::I64(982164);

    let (result, program) = match arch {
        TargetArch::Interp => {
            let res = comp.run(f, arg.into(), ExecTime::Runtime);
            (res, comp.program)
        }
        TargetArch::Aarch64 => {
            let mut interp: Interp = comp.runtime_executor.to_interp().unwrap();
            let mut asm = BcToAsm::new(&mut interp.ready, &mut program);
            asm.asm.reserve(asm.program.funcs.len());
            if let Err(e) = asm.compile(f) {
                // log_err(&comp, e, None);
                exit(1);
            }
            asm.asm.reserve(asm.program.funcs.len()); // Need to allocate for all, not just up to the one being compiled because backtrace gets the len of array from the program func count not from asm.
            asm.asm.make_exec();
            let code = asm.asm.get_fn(f).unwrap().as_ptr();

            let code: extern "C-unwind" fn(i64) -> i64 = unsafe { transmute(code) };
            let indirect_fns = asm.asm.get_dispatch();
            let a = arg.to_int().unwrap();
            unsafe {
                asm!(
                "mov x21, {fns}",
                fns = in(reg) indirect_fns,
                // I'm hoping this is how I declare that I intend to clobber the register.
                // https://doc.rust-lang.org/reference/inline-assembly.html
                // "[...] the contents of the register to be discarded at the end of the asm code"
                // I imagine that means they just don't put it anywhere, not that they zero it for spite reasons.
                out("x21") _
                );
            }
            (Ok(Value::I64(code(a)).into()), &mut program)
        }
        #[cfg(not(feature = "llvm"))]
        TargetArch::Llvm => unreachable!(),
        #[cfg(feature = "llvm")]
        TargetArch::Llvm => {
            let mut interp = comp.runtime_executor.to_interp().unwrap();
            let mut asm = BcToLlvm::new(&mut interp, &mut program);

            if let Err(e) = asm.compile(f) {
                // TODO: make logerr just take program so i can actually call it
                // log_err(&comp, e, None);
                exit(1);
            }

            if let Err(e) = verify_module(asm.llvm.module) {
                // log_err(&comp, e, None);
                exit(1);
            }

            let code = asm.llvm.get_fn_jitted(f).unwrap();
            let code: extern "C" fn(i64) -> i64 = unsafe { transmute(code) };
            let res = code(arg.to_int().unwrap());
            unsafe {
                asm.llvm.release();
            }
            (Ok(Value::I64(res).into()), &mut program)
        }
    };
    if let Err(e) = result {
        // log_err(&comp, e, None);
        exit(1);
    }
    let result = result.unwrap();
    let end = timestamp();
    let seconds = end - start;
    debug_assert_eq!(result, arg.into());

    assert_eq!(program.assertion_count, assertion_count, "vm missed assertions?");
    // std::io::stdout().write_all(&[27]).unwrap();
    // print!("[38;2;{};{};{}m", 0, 255, 0);
    // println!("[PASSED: {} {:?}] {} ms.", name, arch, (seconds * 1000.0) as i64);
    // std::io::stdout().write_all(&[27]).unwrap();
    // print!("[0m");
    exit(0);
}
