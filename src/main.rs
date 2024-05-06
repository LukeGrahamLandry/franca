#![feature(slice_ptr_get)]
#![feature(pattern)]

use franca::{
    ast::{garbage_loc, Flag, FuncId, Program, ScopeId, TargetArch},
    bc_to_asm::emit_aarch64,
    compiler::{CErr, Compile, CompileError, ExecTime},
    emit_rust::bootstrap,
    export_ffi::{get_include_std, STDLIB_PATH},
    find_std_lib,
    lex::Lexer,
    load_program, log_err,
    logging::{init_logs, LogTag},
    make_toplevel,
    parse::Parser,
    pool::StringPool,
    run_main,
    scope::ResolveScope,
    timestamp, MEM, MMAP_ARENA_START, STACK_START, STATS,
};
use std::{
    arch::asm,
    env, fs,
    io::Write,
    mem::{self, transmute},
    panic::{set_hook, take_hook},
    path::PathBuf,
    process::exit,
    str::pattern::Pattern,
};

// TODO: Instead of cli args, what if the arg was a string of code to run so 'franca "start_lsp()"' would concat that on some compiler_cli.txt and run it.
//       Make sure theres some prefix that lets you run/compile the next arg as a file path for shabang line.
//       Maybe that implies I should have syntax for a call that taking the rest of the line as an argument without needing a close paren.
//       Or just passs remaining cli args as args of a function.
//       Because it seems nicer if things are composable and the compiler functions don't assume they can just read the args except the top level one which you can define in userland.
// TODO: repl(). works nicely with ^ so you could experiment but then always be able to run things as a command when working with other tools

const SHOW_MEM_REGIONS: bool = false;

fn main() {
    let marker = 0;
    unsafe { STACK_START = &marker as *const i32 as usize };
    unsafe { MMAP_ARENA_START = MEM.get() as usize };

    // If you get random garbage that looks like a pointer, this can help you figure out where its coming from.
    if SHOW_MEM_REGIONS {
        let prev = take_hook();
        set_hook(Box::new(move |arg| {
            franca::where_am_i();
            prev(arg);
        }));
    }

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
    let mut args = env::args();
    if let Some(name) = args.nth(1) {
        if name == "--" {
            panic!("dont put double dash to seperate args");
        }
        if name == "bootstrap" {
            let (rs, fr) = bootstrap();
            fs::write("target/bootstrap_gen.rs", rs).unwrap();
            fs::write("target/aarch64_basic.gen.fr", fr).unwrap();
            return;
        }
        if name == "log_export_ffi" {
            println!("{}", get_include_std("compiler").unwrap());
            println!("{}", get_include_std("libc").unwrap());

            let pool = Box::leak(Box::<StringPool>::default());
            let program = Program::new(pool, TargetArch::Aarch64, TargetArch::Aarch64);
            println!("{}", program.ffi_definitions);
            return;
        }

        if name == "--no-fork" {
            run_tests_serial();
            return;
        }

        let pool = Box::leak(Box::<StringPool>::default());
        let path = PathBuf::from(format!("tests/{name}.fr"));
        let mut src = if name == "_" {
            "".to_string()
        } else {
            match fs::read_to_string(path) {
                Ok(src) => src,
                Err(_) => fs::read_to_string(&name).unwrap(),
            }
        };

        for a in args {
            src += &a;
        }

        run_main(pool, src, Some(&name), true);

        // println!("{:#?}", unsafe { &STATS });
    } else {
        match unsafe { libc::fork() } {
            -1 => panic!("Fork Failed"),
            0 => {
                run_tests_serial();
                exit(0);
            }
            pid => {
                let mut status = 0i32;
                let pid2 = unsafe { libc::wait(&mut status) };
                assert_eq!(pid, pid2);
                if status != 0 {
                    println!("TESTS FAILED");
                    println!("Running seperately...");
                    println!("=================================");
                    // fork for each individual one, dont print passes.
                    run_tests();
                }
            }
        }
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
        eprintln!("Directory 'tests' does not exist in cwd");
        eprintln!("run 'franca <path>' to run a specific file instead.");
        exit(1);
    }
    let start = timestamp();
    let max_jobs = 7; // not really max cause one file can add multiple.
    let mut passed = 0;
    let mut failed = 0;
    let mut jobs = vec![];
    let mut files: Vec<_> = fs::read_dir("tests").unwrap().map(|f| (f, true)).collect();

    while !files.is_empty() || !jobs.is_empty() {
        if jobs.len() < max_jobs && !files.is_empty() {
            let (case, expect_success) = files.pop().unwrap();
            let case = case.unwrap();
            let name = case.file_name();
            let name = name.to_str().unwrap();
            if !".fr".is_suffix_of(name) {
                if case.file_type().unwrap().is_dir() {
                    let expect_success = expect_success && case.file_name() != "broken";
                    let add = fs::read_dir(case.path()).unwrap().map(|f| (f, expect_success)).collect::<Vec<_>>();
                    files.extend(add)
                }
                continue;
            }
            let name = name.strip_suffix(".fr").unwrap();
            let src = fs::read_to_string(case.path()).unwrap();
            add_test_cases(name.to_string(), src, &mut jobs, expect_success);
        } else {
            assert!(!jobs.is_empty());
            let mut status = 0i32;
            let pid = unsafe { libc::wait(&mut status) };

            let the_job = jobs.iter().position(|j| j.2 == pid).expect("wait returned unexpected pid");
            let (name, arch, _, expect_success) = jobs.remove(the_job);

            // TODO: have it return 'assertion_count' somehow so the tested program calling exit(0) doesn't get seen as a pass.
            // TODO: should have this colour stuff as a library in my language.
            //       https://en.wikipedia.org/wiki/ANSI_escape_code#24-bit
            if expect_success && status == 0 {
                set_colour(0, 255, 0);
                println!("[PASSED: {} {:?}]", name, arch);
                unset_colour();
                passed += 1;
            } else if expect_success {
                set_colour(255, 0, 0);
                println!("[FAILED: {} {:?}]^^^", name, arch);
                unset_colour();
                failed += 1;
            } else if status == 0 {
                set_colour(255, 0, 255);
                println!("[FIXED : {} {:?}]", name, arch);
                unset_colour();
            } else {
                set_colour(255, 255, 0);
                println!("[BROKEN: {} {:?}]^^^", name, arch);
                unset_colour();
            }
        }
    }

    let end = timestamp();
    let seconds = end - start;
    println!("============================");
    set_colour((failed != 0) as u8 * 255, (failed == 0) as u8 * 255, 0);
    println!("Passed {}/{} Tests in {} ms.", passed, passed + failed, (seconds * 1000.0) as i64);
    unset_colour();
    assert_eq!(failed, 0);
}

// forking a bunch confuses the profiler.
// also it seems like a good idea to make sure nothing gets weird when you compile a bunch of stuff on the same compiler instance.
fn run_tests_serial() {
    let beg = MEM.get();
    if !PathBuf::from("tests").exists() {
        eprint!("Directory 'tests' does not exist in cwd");
        exit(1);
    }
    let pool = Box::leak(Box::<StringPool>::default());
    let start = timestamp();
    let mut program = Program::new(pool, TargetArch::Aarch64, TargetArch::Aarch64);
    let mut comp = Compile::new(pool, &mut program);

    let mut files: Vec<_> = fs::read_dir("tests").unwrap().collect();

    let mut test_count = 0;
    let mut assertion_count = 0;
    let mut file_count = 0;
    let mut parsed = vec![];
    while let Some(case) = files.pop() {
        let case = case.unwrap();
        let name = case.file_name();
        let name = name.to_str().unwrap();
        if !".fr".is_suffix_of(name) {
            if case.file_type().unwrap().is_dir() && case.file_name() != "broken" {
                files.extend(fs::read_dir(case.path()).unwrap().collect::<Vec<_>>())
            }
            continue;
        }
        let name = name.strip_suffix(".fr").unwrap();
        let src = fs::read_to_string(case.path()).unwrap();
        assertion_count += src.split("assert_eq(").count() - 1;
        test_count += src.split("#test").count() - 1;
        file_count += 1;

        let file = comp
            .parsing
            .codemap
            .add_file(name.to_string(), format!("#include_std(\"core.fr\");\n{src}"));
        let lex = Lexer::new(file.clone(), comp.program.pool, file.span);
        match Parser::parse_stmts(&mut comp.parsing, lex, comp.pool) {
            Ok(s) => parsed.extend(s),
            Err(e) => {
                let e = CompileError {
                    internal_loc: e.loc,
                    loc: Some(e.diagnostic[0].spans[0].span),
                    reason: CErr::Diagnostic(e.diagnostic),
                    trace: String::new(),
                };
                log_err(&comp, e);
                exit(1);
            }
        };
    }

    let mut global = make_toplevel(comp.pool, garbage_loc(), parsed);
    if let Err(e) = ResolveScope::run(&mut global, &mut comp, ScopeId::from_index(0)) {
        log_err(&comp, *e);
        exit(1);
    }

    if let Err(e) = comp.compile_top_level(global) {
        log_err(&comp, *e);
        exit(1);
    }

    // assert_eq!(test_count, comp.tests.len());
    let mut last = String::new();
    for f in comp.tests.clone() {
        run_one(&mut comp, f);

        let file = comp.parsing.codemap.look_up_span(comp.program[f].loc).file.name().to_string();

        let fname = comp.pool.get(comp.program[f].name);
        if file != last {
            println!();
            set_colour(0, 255, 0);
            print!("[{}] ", file.to_uppercase());
            unset_colour();
            last = file;
        }
        print!("{}, ", fname);
    }

    if let Some(f) = comp.program.find_unique_func(Flag::__Get_Assertions_Passed.ident()) {
        let actual: usize = comp.call_jitted(f, ExecTime::Comptime, None, ()).unwrap();
        assert_eq!(actual, assertion_count, "vm missed assertions?");
    } else {
        println!("__Get_Assertions_Passed not found. COUNT_ASSERT :: false?");
    }
    let end = timestamp();
    let seconds = end - start;
    println!("\n");
    set_colour(250, 150, 200);

    let end = MEM.get();
    println!(
        "ALL TESTS PASSED! {} files. {} tests. {} assertions. {} ms. {} KB.",
        file_count,
        test_count,
        assertion_count,
        (seconds * 1000.0) as i64,
        (end as usize - beg as usize) / 1000,
    );
    unset_colour();

    // println!("{:#?}", unsafe { &STATS });
}

fn set_colour(r: u8, g: u8, b: u8) {
    std::io::stdout().write_all(&[27]).unwrap();
    print!("[38;2;{r};{g};{b}m");
}

fn unset_colour() {
    std::io::stdout().write_all(&[27]).unwrap();
    print!("[0m");
}

fn add_test_cases(name: String, src: String, jobs: &mut Vec<(String, TargetArch, i32, bool)>, expect_success: bool) {
    let assertion_count = src.split("assert_eq(").count() - 1;
    let arch = TargetArch::Aarch64;
    // TODO: use dup2/pipe to capture stdout/err and print in a mutex so the colours don't get fucked up.
    match unsafe { libc::fork() } {
        -1 => panic!("Fork Failed"),
        0 => {
            actually_run_it(name, src, assertion_count, arch);
            exit(0);
        }
        pid => {
            jobs.push((name.clone(), arch, pid, expect_success));
        }
    }
}

/// This is the thing we exec.
fn actually_run_it(_name: String, src: String, assertion_count: usize, arch: TargetArch) {
    init_logs(&[LogTag::ShowPrint, LogTag::ShowErr]);
    // init_logs_flag(0xFFFFFFFF);
    // let save = format!("{name}_{arch:?}/");
    // let save = Some(save.as_str());

    let pool = Box::leak(Box::<StringPool>::default());
    let start = timestamp();
    let mut program = Program::new(pool, TargetArch::Aarch64, arch);
    let mut comp = Compile::new(pool, &mut program);
    let result = load_program(&mut comp, &src);

    if let Err(e) = result {
        log_err(&comp, *e);
        exit(1);
    }

    let expected_tests = src.split("#test").count() - 1;
    assert_eq!(expected_tests, comp.tests.len());

    // TODO: But how to not run the lib tests a billion times
    for f in comp.tests.clone() {
        run_one(&mut comp, f);
    }
    let end = timestamp();
    let _seconds = end - start;
    // TODO: have a call_jitted that takes name and resolves on Arg/Ret generics.
    if let Some(f) = comp.program.find_unique_func(Flag::__Get_Assertions_Passed.ident()) {
        let actual: usize = comp.call_jitted(f, ExecTime::Comptime, None, ()).unwrap();
        assert_eq!(actual, assertion_count, "vm missed assertions?");
    } else {
        println!("__Get_Assertions_Passed not found. COUNT_ASSERT :: false?");
    }
    mem::forget(comp);
    mem::forget(program);
}

fn run_one(comp: &mut Compile, f: FuncId) {
    let result = comp.compile(f, ExecTime::Runtime);
    if let Err(e) = result {
        log_err(comp, *e);
        exit(1);
    }

    let arg = 0; // HACK: rn this works for canary int or just unit because asm just treats unit as an int thats always 0.

    if let Err(e) = emit_aarch64(comp, f, ExecTime::Runtime) {
        log_err(comp, *e);
        exit(1);
    }
    comp.aarch64.reserve(comp.program.funcs.len()); // Need to allocate for all, not just up to the one being compiled because backtrace gets the len of array from the program func count not from asm.
    comp.aarch64.make_exec();
    comp.flush_cpu_instruction_cache();
    let code = comp.aarch64.get_fn(f).unwrap().as_ptr();

    let code: extern "C-unwind" fn(i64) -> i64 = unsafe { transmute(code) };
    let indirect_fns = comp.aarch64.get_dispatch();
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
    let result = code(arg);
    debug_assert_eq!(result, arg);
}

#[test]
fn run_code_tests() {
    find_std_lib();
    run_tests();
}
