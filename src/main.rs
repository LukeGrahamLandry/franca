#![feature(slice_ptr_get)]
#![feature(pattern)]

use franca::{
    ast::{garbage_loc, Flag, FuncId, Program, ScopeId, TargetArch},
    bc_to_asm::emit_aarch64,
    compiler::{Compile, ExecTime, Res},
    emit_rust::bootstrap,
    export_ffi::{get_include_std, STDLIB_PATH},
    find_std_lib,
    lex::Lexer,
    log_err, make_toplevel,
    parse::Parser,
    pool::StringPool,
    run_main,
    scope::ResolveScope,
    timestamp, MEM, MMAP_ARENA_START, STACK_START,
};
use std::{
    arch::asm,
    env,
    fs::{self, File},
    io::{Read, Write},
    mem::transmute,
    os::fd::FromRawFd,
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
        run_tests_find_faliures();
        check_broken();
    }
}

/// The normal cargo test harness combines the tests of a package into one exe and catches panics to continue after one fails.
/// It doesn't expect you do be causing segfault/illegal instruction signals, (which is generally a reasonable assumption but less so for my compiler sadly).
/// So when a test does that it kills the whole thing instead of being recorded as a failing test and continuing.
/// Instead, I start each test in its own process so no matter what happens, it can't interfear with the other tests
/// (unless they're like trying to use the write files or something, which like... don't do that then I guess).
/// My way has higher overhead per test but it feels worth it.

fn run_tests_find_faliures() {
    // If we can run all the tests without crashing, cool, we're done.
    let (success, out, err) = fork_and_catch(run_tests_serial);
    if success {
        println!("{out}");
        println!("{err}");
        return;
    }

    println!("TESTS FAILED. Running seperately...");
    println!("=================================");
    forked_swallow_passes();
}

// forking a bunch confuses the profiler.
// also it seems like a good idea to make sure nothing gets weird when you compile a bunch of stuff on the same compiler instance.
fn run_tests_serial() {
    let beg = MEM.get();
    let pool = Box::leak(Box::<StringPool>::default());
    let start = timestamp();
    let mut program = Program::new(pool, TargetArch::Aarch64, TargetArch::Aarch64);
    let mut comp = Compile::new(pool, &mut program);

    let files = collect_test_files();
    load_all_toplevel(&mut comp, &files).unwrap_or_else(|e| {
        log_err(&comp, *e);
        exit(1);
    });

    // let assertion_count = ta.iter().map(|(_, i)| i).sum();
    // let test_count: usize = ta.iter().map(|(i, _)| i).sum();

    // assert_eq!(test_count, comp.tests.len());
    let mut last = String::new();
    let test_count = comp.tests.len();
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

    let assertion_count = if let Some(f) = comp.program.find_unique_func(Flag::__Get_Assertions_Passed.ident()) {
        let actual: usize = comp.call_jitted(f, ExecTime::Comptime, None, ()).unwrap();
        // assert_eq!(actual, assertion_count, "vm missed assertions?");
        actual
    } else {
        println!("__Get_Assertions_Passed not found. COUNT_ASSERT :: false?");
        0
    };
    let end = timestamp();
    let seconds = end - start;
    println!("\n");
    set_colour(250, 150, 200);

    let end = MEM.get();
    println!(
        "ALL TESTS PASSED! {} files. {} tests. {} assertions. {} ms. {} KB.",
        files.len(),
        test_count,
        assertion_count,
        (seconds * 1000.0) as i64,
        (end as usize - beg as usize) / 1000,
    );
    unset_colour();

    // println!("{:#?}", unsafe { &STATS });
}

fn forked_swallow_passes() {
    let pool = Box::leak(Box::<StringPool>::default());
    let mut program = Program::new(pool, TargetArch::Aarch64, TargetArch::Aarch64);
    let mut comp = Compile::new(pool, &mut program);

    let files = collect_test_files();
    load_all_toplevel(&mut comp, &files).unwrap_or_else(|e| {
        log_err(&comp, *e);
        exit(1);
    });

    let tests = comp.tests.clone();
    let total = tests.len();
    let mut failed = 0;
    let mut failing: Vec<FuncId> = vec![];
    for fns in tests.chunks(10) {
        let (success, _, _) = fork_and_catch(|| {
            for f in fns {
                run_one(&mut comp, *f);
            }
        });
        if !success {
            failing.extend(fns)
        }
    }

    for f in failing {
        let file = comp.parsing.codemap.look_up_span(comp.program[f].loc).file.name().to_string();
        let fname = comp.pool.get(comp.program[f].name);
        let (success, out, err) = fork_and_catch(|| run_one(&mut comp, f));

        if !success {
            failed += 1;
            println!();
            set_colour(255, 0, 0);
            print!("[FAILED] {} {} ", file.to_uppercase(), fname);
            unset_colour();
            println!("{out}");
            println!("{err}");
        }
    }

    set_colour(250, 150, 200);
    println!("FAILED {}/{} tests.", failed, total);
    unset_colour();
}

fn check_broken() {
    let pool = Box::leak(Box::<StringPool>::default());
    let mut program = Program::new(pool, TargetArch::Aarch64, TargetArch::Aarch64);
    let mut comp = Compile::new(pool, &mut program);

    let files = collect_test_files();
    load_all_toplevel(&mut comp, &files).unwrap_or_else(|e| {
        log_err(&comp, *e);
        exit(1);
    });

    set_colour(255, 0, 0);
    print!("[KNOWN BUGS] ");
    unset_colour();
    let tests = comp.tests_broken.clone();
    for f in tests {
        let fname = comp.pool.get(comp.program[f].name);
        let (success, _, _) = fork_and_catch(|| {
            println!(" ");
            println!(" ");
            run_one(&mut comp, f);
        });
        if success {
            set_colour(255, 0, 255);
        } else {
            set_colour(255, 255, 0);
        }
        print!("{fname}, ");
        unset_colour();
    }
    println!();
}

fn collect_test_files() -> Vec<(String, String)> {
    let mut files: Vec<_> = fs::read_dir("tests").unwrap().collect();

    let mut found = vec![];
    while let Some(case) = files.pop() {
        let case = case.unwrap();
        let name = case.file_name();
        let name = name.to_str().unwrap();
        if !".fr".is_suffix_of(name) {
            // TODO: do 'broken' with different annotation
            if case.file_type().unwrap().is_dir() {
                files.extend(fs::read_dir(case.path()).unwrap().collect::<Vec<_>>())
            }
            continue;
        }
        let name = name.strip_suffix(".fr").unwrap().to_string();
        let src = fs::read_to_string(case.path()).unwrap();
        found.push((name, src));
    }
    found
}

fn load_all_toplevel<'p>(comp: &mut Compile<'_, 'p>, files: &[(String, String)]) -> Res<'p, ()> {
    if !PathBuf::from("tests").exists() {
        eprint!("Directory 'tests' does not exist in cwd");
        exit(1);
    }
    let mut parsed = vec![];
    for (name, src) in files {
        let file = comp
            .parsing
            .codemap
            .add_file(name.to_string(), format!("#include_std(\"core.fr\");\n{src}"));
        let lex = Lexer::new(file.clone(), comp.program.pool, file.span);
        parsed.extend(Parser::parse_stmts(&mut comp.parsing, lex, comp.pool)?);
    }

    let mut global = make_toplevel(comp.pool, garbage_loc(), parsed);
    ResolveScope::run(&mut global, comp, ScopeId::from_index(0))?;
    comp.compile_top_level(global)?;
    Ok(())
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

fn fork_and_catch(f: impl FnOnce()) -> (bool, String, String) {
    let mut out = [0, 0];
    let mut err = [0, 0];
    unsafe {
        libc::pipe(out.as_mut_ptr());
        libc::pipe(err.as_mut_ptr());
    }
    match unsafe { libc::fork() } {
        -1 => panic!("Fork Failed"),
        0 => {
            unsafe {
                libc::dup2(out[1], libc::STDOUT_FILENO);
                libc::dup2(err[1], libc::STDERR_FILENO);
            }
            print!(" ");
            eprint!(" "); // need to make sure there's something on both pipes or it blocks?
            f();
            exit(0);
        }
        pid => {
            fn read_all(fd: i32) -> String {
                let mut f = unsafe { File::from_raw_fd(fd) };
                // can't use read_to_string, it blocks waiting for eof?
                let mut s = vec![0; 99999];
                let len = f.read(&mut s).unwrap();
                unsafe { s.set_len(len) };
                String::from_utf8(s).unwrap()
            }

            let mut status = 0i32;
            let pid2 = unsafe { libc::wait(&mut status) };
            assert_eq!(pid, pid2);

            // TODO: how big is the pipe's buffer?
            let outs = read_all(out[0]);
            let errs = read_all(err[0]);

            unsafe {
                libc::close(out[0]);
                libc::close(err[0]);
            }

            (status == 0, outs, errs)
        }
    }
}

fn set_colour(r: u8, g: u8, b: u8) {
    std::io::stdout().write_all(&[27]).unwrap();
    print!("[38;2;{r};{g};{b}m");
}

fn unset_colour() {
    std::io::stdout().write_all(&[27]).unwrap();
    print!("[0m");
}
