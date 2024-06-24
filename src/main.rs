#![feature(slice_ptr_get)]
#![feature(pattern)]
#![feature(thread_sleep_until)]

use franca::{
    ast::{garbage_loc, FuncId, Program, ScopeId, TargetArch},
    bc::{to_values, Values},
    compiler::{Compile, ExecStyle, Res},
    export_ffi::{get_include_std, ImportVTable, IMPORT_VTABLE},
    find_std_lib, log_err,
    logging::PoolLog,
    make_toplevel,
    scope::ResolveScope,
    timestamp, MEM, MMAP_ARENA_START, STACK_START, STATS,
};
use std::{
    env,
    ffi::CString,
    fs::{self, File},
    io::Read,
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
    let start = timestamp();
    let log_time = || {
        let end = timestamp();
        let seconds = end - start;
        println!("Compilation (parse/bytecode/jit) finished in {seconds:.3} seconds;",);
    };

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
        // eprintln!("Found installed lib at {:?}", STDLIB_PATH.lock().unwrap().as_ref().unwrap());
    } else {
        // TODO: show search locations
        eprintln!("Standard library not found.");
        exit(1);
    }

    let mut no_fork = false;
    let mut stats = false;
    let mut path = None;
    let mut args = env::args();

    let mut driver_path = None;
    args.next().unwrap(); // exe path

    #[cfg(target_arch = "aarch64")]
    let mut arch = TargetArch::Aarch64;
    #[cfg(not(target_arch = "aarch64"))]
    let mut arch = TargetArch::Cranelift;

    #[cfg(all(not(target_arch = "aarch64"), not(feature = "cranelift")))]
    compile_error!("no backend available");

    while let Some(name) = args.next() {
        if name == "--" {
            break;
        }

        if let Some(name) = name.strip_prefix("--") {
            match name {
                "no-fork" => no_fork = true,
                "stats" => stats = true,
                // "anon-names" => unsafe { ANON_BODY_AS_NAME = true },
                #[cfg(feature = "cranelift")]
                "cranelift" => arch = TargetArch::Cranelift,
                #[cfg(not(feature = "cranelift"))]
                "cranelift" => panic!("recompile the compiler with cranelift feature enabled. "),
                #[cfg(target_arch = "aarch64")]
                "aarch64" => arch = TargetArch::Aarch64,
                #[cfg(not(target_arch = "aarch64"))]
                "aarch64" => {
                    panic!("TODO: direct aarch64 backend cannot cross compile because its just jit because i put the addresses in the code...")
                }
                "log_export_ffi" => {
                    println!("{}", get_include_std("compiler").unwrap());
                    println!("{}", get_include_std("libc").unwrap());
                }
                "help" => panic!("--no-fork, --64fps, --cranelift, --aarch64, --log_export_ffi, --stats"),

                "driver-dylib" => {
                    let path = args.next().expect("path to driver dylib");
                    assert!(!path.starts_with("--"), "you probably didn't mean to start a filepath with '--'?");
                    driver_path = Some(path)
                }

                _ => panic!("unknown argument --{name}"),
            }
        } else {
            assert!(path.is_none(), "please specify only one input file");
            path = Some(name);
        }
    }

    if let Some(path) = driver_path {
        unsafe {
            let mut s = path.as_bytes().to_vec();
            s.push(0);
            let s = CString::from_vec_with_nul(s).unwrap();
            let lib = libc::dlopen(s.as_ptr(), libc::RTLD_LAZY);
            assert_ne!(lib as usize, 0);

            let mut s = "driver".as_bytes().to_vec();
            s.push(0);
            let s = CString::from_vec_with_nul(s).unwrap();
            let f = libc::dlsym(lib, s.as_ptr());
            assert_ne!(f as usize, 0);
            let f: extern "C" fn(*const ImportVTable) = transmute(f);
            f(&IMPORT_VTABLE as *const ImportVTable);
            return;
        }
    }
    if no_fork {
        panic!("--no-fork is no longer built in to the compiler");
    }

    if let Some(name) = path {
        let path = PathBuf::from(format!("tests/{name}.fr"));
        let src = match fs::read_to_string(path) {
            Ok(src) => src,
            Err(_) => fs::read_to_string(&name).unwrap(),
        };
        // TODO
        // for a in args {
        //     src += &a;
        // }

        let mut program = Program::new(arch);
        let mut comp = Compile::new(&mut program);

        load_all_toplevel(&mut comp, &[(name, src)]).unwrap_or_else(|e| {
            log_err(&comp, *e);
            exit(1);
        });

        if let Some(f) = comp.program.find_unique_func(comp.program.pool.intern("driver")) {
            let val = to_values(comp.program, &IMPORT_VTABLE as *const ImportVTable as i64).unwrap();
            if let franca::export_ffi::BigResult::Err(e) = comp.compile(f, ExecStyle::Jit) {
                log_err(&comp, *e);
                exit(1);
            }
            if let franca::export_ffi::BigResult::Err(e) = comp.run(f, val) {
                log_err(&comp, *e);
                exit(1);
            }
            return;
        }

        for f in comp.tests.clone() {
            println!("{}();", comp.program.pool.get(comp.program[f].name));
            run_one(&mut comp, f);
        }

        if comp.tests.is_empty() {
            let f = comp.program.find_unique_func(comp.program.pool.intern("main")).expect("fn main");
            let result = comp.compile(f, ExecStyle::Jit);
            if let franca::export_ffi::BigResult::Err(e) = result {
                log_err(&comp, *e);
                exit(1);
            }

            log_time();
            run_one(&mut comp, f);
        }
        if stats {
            println!("{:#?}", unsafe { STATS.clone() });
        }
    } else {
        assert!(!stats, "run with --no-fork to show --stats");
        if !PathBuf::from("tests").exists() {
            eprint!("Directory 'tests' does not exist in cwd");
            exit(1);
        }
        forked_swallow_passes(arch);
    }
}

fn forked_swallow_passes(arch: TargetArch) {
    let mut program = Program::new(arch);
    let mut comp = Compile::new(&mut program);

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
        // let (success, _, _) = fork_and_catch(|| {
        for f in fns {
            run_one(&mut comp, *f);
        }
        // });
        let success = true;
        if success {
            let names = fns
                .iter()
                .map(|fid| {
                    let name = comp.program[*fid].name;
                    comp.program.pool.get(name)
                })
                .collect::<Vec<_>>()
                .join(", ");

            set_colour(0, 255, 0);
            print!("[PASSED] ");
            unset_colour();
            println!("{names}",);
        } else {
            failing.extend(fns)
        }
    }

    // for f in failing {
    //     let fname = comp.program.pool.get(comp.program[f].name);
    //     let (success, out, err) = fork_and_catch(|| run_one(&mut comp, f));

    //     if !success {
    //         failed += 1;
    //         println!();
    //         set_colour(255, 0, 0);
    //         print!("[FAILED] {} ", fname);
    //         unset_colour();
    //         println!("{out}");
    //         println!("{err}");
    //     }
    // }

    set_colour(250, 150, 200);
    if failed == 0 {
        println!("ALL TESTS PASSED! {} tests", total);

        for (ty, size) in comp.program.ffi_sizes.clone() {
            let expect = comp.program.get_info(ty).stride_bytes as usize;
            if size != expect {
                println!(
                    "{:?}: {}: fr={} vs rs={}",
                    comp.program.inferred_type_names[ty.as_index()].map(|n| comp.program.pool.get(n)),
                    comp.program.log_type(ty),
                    expect,
                    size
                )
            }
        }
        // println!("{}", comp.program.ffi_definitions);
    } else {
        println!("FAILED {}/{} tests.", failed, total);
    }
    unset_colour();
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
    let mut parsed = vec![];
    for (name, src) in files {
        let span = comp.program.pool.add_file(name.to_string(), format!("#include_std(\"core.fr\");\n{src}"));

        let id = comp.program.pool.add_task(false, span);
        let s = comp.program.pool.wait_for_stmts(id)?;
        for s in &s {
            use franca::logging::PoolLog;
            // println!("{}", s.log(comp.program.pool));
        }
        parsed.extend(s);
    }

    let mut global = make_toplevel(comp.program.pool, garbage_loc(), parsed);
    ResolveScope::run(&mut global, comp, ScopeId::from_index(0))?;
    comp.compile_top_level(global)?;
    franca::export_ffi::BigResult::Ok(())
}

fn run_one(comp: &mut Compile, f: FuncId) {
    let result = comp.compile(f, ExecStyle::Jit);
    if let franca::export_ffi::BigResult::Err(e) = comp.tag_err(result) {
        log_err(comp, *e);
        exit(1);
    }

    // HACK: rn this works for canary int or just unit because asm just treats unit as an int thats always 0.
    comp.run(f, Values::many(vec![0, 0, 0, 0, 0, 0, 0, 0])).unwrap();
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
            println!(" ");
            eprintln!(" "); // need to make sure there's something on both pipes or it blocks?
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
    print!("\x1B[38;2;{r};{g};{b}m");
}

fn unset_colour() {
    print!("\x1B[0m");
}
