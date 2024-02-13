#![allow(unused)]

use std::{env, fs, io::read_to_string, path::PathBuf, time::Instant};

use codemap::CodeMap;
use codemap_diagnostic::{ColorConfig, Diagnostic, Emitter, Level, SpanLabel, SpanStyle};
use franca::{interp::Value, pool::StringPool, run_main};

fn main() {
    let pool = Box::leak(Box::<StringPool>::default());
    if let Some(name) = env::args().nth(1) {
        run_main(
            pool,
            fs::read_to_string(format!("tests/{name}.txt")).unwrap(),
            Value::I64(0),
            Value::I64(0),
            Some(&name),
        );
        return;
    }

    for case in fs::read_dir("tests").unwrap() {
        let case = case.unwrap();
        println!("TEST: {}", case.file_name().to_str().unwrap());
        run_main(
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
}
