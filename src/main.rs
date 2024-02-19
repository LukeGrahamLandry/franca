use franca::{
    bc::Value,
    logging::{init_logs, init_logs_flag, LogTag},
    pool::StringPool,
    run_main,
};
use std::{env, fs, path::PathBuf};

fn main() {
    if let Some(name) = env::args().nth(1) {
        let pool = Box::leak(Box::<StringPool>::default());
        let path = PathBuf::from(format!("tests/{name}.txt"));
        if path.exists() {
            init_logs_flag(0xFFFFFFFFF);
            run_main(
                pool,
                fs::read_to_string(format!("tests/{name}.txt")).unwrap(),
                Value::I64(0),
                Value::I64(0),
                Some(&name),
            );
        } else {
            init_logs(&[LogTag::Scope, LogTag::ShowPrint]);
            run_main(
                pool,
                fs::read_to_string(&name).unwrap(),
                Value::I64(0),
                Value::I64(0),
                Some(&name),
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
                Some(
                    case.file_name()
                        .to_str()
                        .unwrap()
                        .strip_suffix(".txt")
                        .unwrap(),
                ),
            );
        }
        assert!(passed);
    }
}
