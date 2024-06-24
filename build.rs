use std::{fs, path::PathBuf};

fn main() {
    let path = "compiler/target/libfranca.a";
    if !PathBuf::from(path).exists() {
        let _ = fs::create_dir("compiler/target"); // dont care if already exists
        fs::copy("boot/libfranca.a", path).unwrap();
    }
    println!("cargo:rerun-if-changed={}", path);
    println!("cargo:rustc-link-search=native=compiler/target/");
    println!("cargo:rustc-link-lib=static=franca");
}
