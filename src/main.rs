use franca::{bc::Value, pool::StringPool, run_main};
use std::{env, fs};

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
    } else {
        println!("No file selected.");
    }
}
