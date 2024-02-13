#![allow(unused)]

use std::{env, fs, io::read_to_string, path::PathBuf, time::Instant};

use codemap::CodeMap;
use codemap_diagnostic::{ColorConfig, Diagnostic, Emitter, Level, SpanLabel, SpanStyle};
use franca::{interp::Value, pool::StringPool, run_main, run_tests};

fn main() {
    if let Some(name) = env::args().nth(1) {
        let pool = Box::leak(Box::<StringPool>::default());
        run_main(
            pool,
            fs::read_to_string(format!("tests/{name}.txt")).unwrap(),
            Value::I64(0),
            Value::I64(0),
            Some(&name),
        );
        return;
    }

    run_tests();
}
