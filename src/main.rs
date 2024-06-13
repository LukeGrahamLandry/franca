#![feature(slice_ptr_get)]
#![feature(pattern)]
#![feature(thread_sleep_until)]

use franca::{
    ast::{garbage_loc, FuncId, Program, ScopeId, TargetArch},
    bc::{to_values, Values},
    compiler::{Compile, ExecStyle, Res},
    export_ffi::{end_raw, get_include_std, start_raw, ImportVTable, IMPORT_VTABLE},
    find_std_lib,
    lex::Lexer,
    log_err, make_toplevel,
    parse::{Parser, ANON_BODY_AS_NAME},
    pool::StringPool,
    scope::ResolveScope,
    timestamp, MEM, MMAP_ARENA_START, STACK_START, STATS,
};
use std::{
    env,
    ffi::CString,
    fs::{self, File},
    io::Read,
    mem::{self, transmute},
    os::fd::FromRawFd,
    panic::{set_hook, take_hook},
    path::PathBuf,
    process::{exit, Command},
    str::pattern::Pattern,
    thread::sleep_until,
    time::{Duration, Instant},
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
    let mut do_60fps_ = false;
    let mut path = None;
    let mut args = env::args();
    let mut exe_path = None;
    let mut c = false;
    let mut run_with_clang = false;
    let mut sanitize = false;
    let mut test_c = false;
    let mut clang_o2 = false;
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
            panic!("dont put double dash to seperate args");
        }

        if let Some(name) = name.strip_prefix("--") {
            match name {
                "no-fork" => no_fork = true,
                "stats" => stats = true,
                "60fps" => do_60fps_ = true,
                "anon-names" => unsafe { ANON_BODY_AS_NAME = true },
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
                "help" => panic!("--no-fork, --64fps, --cranelift, --aarch64, --log_export_ffi, --stats, --c, --run-clang"),
                "exe" => exe_path = Some(args.next().expect("--exe <output_filepath>")),
                // TODO: need to have a -o flag so you can seperate logging of compile time execution from output c source code.
                "c" => {
                    c = true;
                    assert!(cfg!(feature = "c-backend"));
                }
                "run-clang" => {
                    run_with_clang = true;
                    assert!(cfg!(feature = "c-backend"));
                }
                "san" => {
                    sanitize = true;
                    assert!(cfg!(feature = "c-backend"));
                }
                "test-c" => {
                    test_c = true;
                    assert!(cfg!(feature = "c-backend"));
                }
                "optimize=fast" => {
                    clang_o2 = true;
                }
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

    if c && !run_with_clang {
        print!("/*");
    }
    if sanitize {
        assert!(run_with_clang);
    }

    if no_fork {
        panic!("--no-fork is no longer built in to the compiler");
    }
    if do_60fps_ {
        do_60fps(arch);
        return;
    }

    #[cfg(feature = "c-backend")]
    if test_c {
        run_c_tests(arch, sanitize);
        return;
    }

    if let Some(name) = path {
        let pool = Box::leak(Box::<StringPool>::default());
        let path = PathBuf::from(format!("tests/{name}.fr"));
        let src = match fs::read_to_string(path) {
            Ok(src) => src,
            Err(_) => fs::read_to_string(&name).unwrap(),
        };
        // TODO
        // for a in args {
        //     src += &a;
        // }

        let mut program = Program::new(pool, arch);
        let mut comp = Compile::new(pool, &mut program);

        load_all_toplevel(&mut comp, &[(name, src)]).unwrap_or_else(|e| {
            log_err(&comp, *e);
            exit(1);
        });

        #[cfg(feature = "c-backend")]
        if c || run_with_clang {
            let (fns, test_runner) = if comp.export.is_empty() {
                let name = comp.program.pool.intern("main");
                if let Some(f) = comp.program.find_unique_func(name) {
                    (vec![f], false)
                } else {
                    (comp.tests.clone(), true)
                }
            } else {
                (comp.export.clone(), false)
            };
            match franca::c::emit_c(&mut comp, fns, test_runner) {
                franca::export_ffi::BigResult::Ok(s) => {
                    log_time();
                    if run_with_clang {
                        fs::write("./target/temp.c", s).unwrap();
                        run_clang_on_temp_c(sanitize, clang_o2);
                    } else {
                        println!("*/{}", s);
                    }

                    if stats {
                        println!("/*{:#?}*/", unsafe { STATS.clone() });
                    }
                    exit(0);
                }
                franca::export_ffi::BigResult::Err(e) => {
                    log_err(&comp, *e);
                    eprintln!("failed");
                    exit(1);
                }
            }
        }

        if let Some(f) = comp.program.find_unique_func(comp.pool.intern("driver")) {
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
            let f = comp.program.find_unique_func(comp.pool.intern("main")).expect("fn main");
            let result = comp.compile(f, ExecStyle::Jit);
            if let franca::export_ffi::BigResult::Err(e) = result {
                log_err(&comp, *e);
                exit(1);
            }
            #[cfg(feature = "cranelift")]
            if let Some(output) = exe_path {
                let obj = franca::cranelift::emit_cl_exe(&mut comp, f).unwrap();
                log_time();

                let bytes = obj.emit().unwrap();
                fs::write(output, bytes).unwrap();
            } else {
                log_time();
                run_one(&mut comp, f);
            }

            #[cfg(not(feature = "cranelift"))]
            {
                log_time();
                run_one(&mut comp, f);
            }
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
        check_broken(arch);
    }
}

fn run_clang_on_temp_c(sanitize: bool, o2: bool) {
    let start = timestamp();
    let mut cmd = Command::new("clang");
    let mut cmd = cmd.args([
        "target/temp.c",
        "-Wno-int-to-void-pointer-cast",
        "-Wno-void-pointer-to-int-cast",
        "-o",
        "target/a.out",
        "-Wno-incompatible-library-redeclaration",
        "-Wno-int-conversion",
        "-Wno-pointer-sign",
        "-Wno-return-type", // TODO: this one would be helpful! but need to emit _Noreturn for functions returning Never
        "-Wno-incompatible-function-pointer-types", // TODO: this ones probably often helpful, should fix it in the generated code. also UB san complains.
    ]);

    if o2 {
        cmd = cmd.args(["-O2"]);
    }
    if sanitize {
        cmd = cmd.args(["-fsanitize=undefined", "-fsanitize=address", "-g"]);
    }
    let res = cmd.status().unwrap();
    let end = timestamp();
    let seconds = end - start;
    println!("clang finished in {seconds:.3} seconds.");
    assert!(res.success());
    let res = Command::new("./target/a.out").status().unwrap();
    assert!(res.success());
}

#[cfg(feature = "c-backend")]
fn run_c_tests(arch: TargetArch, sanitize: bool) {
    let pool = Box::leak(Box::<StringPool>::default());
    let mut program = Program::new(pool, arch);
    let mut comp = Compile::new(pool, &mut program);

    let files = collect_test_files();
    load_all_toplevel(&mut comp, &files).unwrap_or_else(|e| {
        log_err(&comp, *e);
        exit(1);
    });

    let tests = comp.tests.clone();
    let mut failed = 0;
    for f in &tests {
        // comp.compile(*f, ExecStyle::Jit).unwrap_or_else(|e| {
        //     log_err(&comp, *e);
        //     exit(1);
        // });
        let (pass, _, _) = fork_and_catch(|| {
            let src = franca::c::emit_c(&mut comp, vec![*f], true).unwrap_or_else(|e| {
                log_err(&comp, *e);
                exit(1);
            });
            fs::write("target/temp.c", src).unwrap();
            run_clang_on_temp_c(sanitize, false);
        });
        if pass {
            set_colour(0, 255, 0);
        } else {
            failed += 1;
            set_colour(255, 0, 0);
        }
        println!("{}", comp.pool.get(comp.program[*f].name));
        unset_colour();
    }
    if failed == 0 {
        println!("passed all {} tests.", tests.len());
    } else {
        println!("failed {}/{} tests.", failed, tests.len());
    }

    mem::forget(comp);
    mem::forget(program);

    // println!("{:#?}", unsafe { &STATS });
}

fn forked_swallow_passes(arch: TargetArch) {
    let pool = Box::leak(Box::<StringPool>::default());
    let mut program = Program::new(pool, arch);
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
        if success {
            let names = fns
                .iter()
                .map(|fid| {
                    let name = comp.program[*fid].name;
                    comp.pool.get(name)
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
    if failed == 0 {
        println!("ALL TESTS PASSED! {} tests", total);

        for (ty, size) in comp.program.ffi_sizes.clone() {
            let expect = comp.program.get_info(ty).stride_bytes as usize;
            if size != expect {
                println!(
                    "{:?}: {}: fr={} vs rs={}",
                    comp.program.inferred_type_names[ty.as_index()].map(|n| comp.pool.get(n)),
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

fn check_broken(arch: TargetArch) {
    let pool = Box::leak(Box::<StringPool>::default());
    let mut program = Program::new(pool, arch);
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

fn do_60fps(arch: TargetArch) {
    let name = "examples/mandelbrot.fr";
    let src = fs::read_to_string(name).unwrap();
    let mut src_parts = src.split("// @InsertConfig");
    let src1 = src_parts.next().unwrap().to_string();
    let src2 = src_parts.next().unwrap().to_string();

    println!("\x1B[?1049h");
    start_raw(0);
    let base = MEM.get();

    let start = timestamp();
    let mut frames = 0;
    let mut x = -1.5f32;
    let mut y = -1.0f32;
    loop {
        let frame_end = Instant::now().checked_add(Duration::from_millis(18)).unwrap();
        let mut src = String::with_capacity(src1.len() + src2.len() + 100);
        // TODO: cant do float calls in constants now without libffi
        src.push_str(&src1);
        if x >= 0.0 {
            src.push_str(&format!("x_start = {x:.2};"));
        } else {
            src.push_str(&format!("x_start = 0.0.sub({:.2});", x.abs()));
        }
        if y >= 0.0 {
            src.push_str(&format!("y_start = {y:.2};"));
        } else {
            src.push_str(&format!("y_start = 0.0.sub({:.2});", y.abs()));
        }
        src.push_str(&src2);

        let pool = Box::leak(Box::<StringPool>::default());
        let mut program = Program::new(pool, arch);
        let mut comp = Compile::new(pool, &mut program);

        let file = comp
            .parsing
            .codemap
            .add_file(name.to_string(), format!("#include_std(\"core.fr\");\n{src}"));
        let lex = Lexer::new(file.clone(), comp.program.pool, file.span);
        let parsed = Parser::parse_stmts(&mut comp.parsing, lex, comp.pool).unwrap();

        let mut global = make_toplevel(comp.pool, garbage_loc(), parsed);
        ResolveScope::run(&mut global, &mut comp, ScopeId::from_index(0)).unwrap();
        comp.compile_top_level(global).unwrap();
        let f = comp.program.find_unique_func(comp.pool.intern("main")).unwrap();
        comp.compile(f, ExecStyle::Jit).unwrap();

        println!("\x1B[2J");
        run_one(&mut comp, f);

        frames += 1;
        sleep_until(frame_end);
        if let Some(c) = get_input() {
            match c as char {
                'q' => break,
                'a' => x += 0.1,
                'd' => x -= 0.1,
                's' => y -= 0.1,
                'w' => y += 0.1,
                _ => {}
            }
        }

        // let mut temp = memmap2::MmapOptions::new().len(0).map_anon().unwrap().make_exec().unwrap();
        // comp.aarch64.make_exec(); // if we were using cranelift backend, it might be write the first time.
        // mem::swap(comp.aarch64.map_exec.as_mut().unwrap(), &mut temp);
        // TODO: unmap constant data in pool.rs

        // TODO
        // #[cfg(feature = "cranelift")]
        // {
        //     unsafe {
        //         comp.cranelift.module.free_memory();
        //     }
        // }

        mem::forget(comp);
        mem::forget(program);
        // Reset the arena
        MEM.set(base);

        todo!();
    }
    let end = timestamp();
    let s = end - start;
    end_raw(0);
    println!("\x1B[?1049l");
    println!("{frames} frames in {:.0} ms: {:.0}fps", s * 1000.0, frames as f64 / s);

    fn get_input() -> Option<u8> {
        let mut c = 0;
        let len = unsafe { libc::read(0, &mut c as *mut u8 as *mut libc::c_void, 1) };
        if len > 0 {
            Some(c)
        } else {
            None
        }
    }
}
