use franca::export_ffi::get_special_functions;
use franca::{
    bc::Value,
    experiments::emit_rust::bootstrap,
    interp::Interp,
    logging::{init_logs, init_logs_flag, LogTag},
    pool::StringPool,
    run_main,
};
use std::{env, fs, path::PathBuf};

// TODO: Instead of cli args, what if the arg was a string of code to run so 'franca "start_lsp()"' would concat that on some compiler_cli.txt and run it.
//       Make sure theres some prefix that lets you run/compile the next arg as a file path for shabang line.
//       Maybe that implies I should have syntax for a call that taking the rest of the line as an argument without needing a close paren.
//       Or just passs remaining cli args as args of a function.
//       Because it seems nicer if things are composable and the compiler functions don't assume they can just read the args except the top level one which you can define in userland.
// TODO: repl(). works nicely with ^ so you could experiment but then always be able to run things as a command when working with other tools
fn main() {
    if let Some(name) = env::args().nth(1) {
        if name == "bootstrap" {
            let (rs, fr) = bootstrap();
            fs::write("target/bootstrap_gen.rs", rs).unwrap();
            fs::write("target/aarch64_basic.gen.fr", fr).unwrap();
            return;
        }
        if name == "log_export_ffi" {
            println!("{}", get_special_functions());
            return;
        }
        if name == "lsp" {
            #[cfg(feature = "lsp")]
            {
                franca::lsp::run_lsp_blocking().unwrap();
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
                Interp::new(pool),
            );
        } else {
            init_logs(&[LogTag::Scope, LogTag::ShowPrint]);
            run_main(
                pool,
                fs::read_to_string(&name).unwrap(),
                Value::I64(0),
                Value::I64(0),
                Some(&name),
                Interp::new(pool),
            );
        }
    } else {
        let mut passed = true;
        for case in fs::read_dir("tests").unwrap() {
            init_logs_flag(0xFFFFFFFFF);
            let pool = Box::leak(Box::<StringPool>::default());
            let case = case.unwrap();
            println!("TEST: {}", case.file_name().to_str().unwrap());
            passed &= run_main(
                pool,
                fs::read_to_string(case.path()).unwrap(),
                Value::I64(0),
                Value::I64(0),
                Some(case.file_name().to_str().unwrap().strip_suffix(".fr").unwrap()),
                Interp::new(pool),
            );
        }
        assert!(passed);
    }
}
