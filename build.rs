use std::{fs, path::PathBuf};

fn main() {
    let triple = std::env::var("TARGET").unwrap();
    let path = format!("compiler/target/{triple}/libfranca.a");
    if !PathBuf::from(&path).exists() {
        let _ = fs::create_dir("compiler/target/{triple}"); // dont care if already exists
        fs::copy(format!("boot/{triple}/libfranca.a"), &path).expect("/boot has old version");
    }
    println!("cargo:rerun-if-changed={}", path);
    println!("cargo:rustc-link-search=native=compiler/target/{triple}/");
    println!("cargo:rustc-link-lib=static=franca");
}
