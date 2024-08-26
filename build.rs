use std::{fs, path::PathBuf};

fn main() {
    let triple = std::env::var("TARGET").unwrap();
    let path = format!("target/franca/{triple}/libselffranca.a");
    if !PathBuf::from(&path).exists() {
        let _ = fs::create_dir_all(format!("target/franca/{triple}")); // dont care if already exists
        fs::copy(format!("boot/{triple}/libselffranca.a"), &path).expect("/boot has old version");
    }
    println!("cargo:rerun-if-changed={}", path);
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rustc-link-search=native=target/franca/{triple}/");
    println!("cargo:rustc-link-lib=static=selffranca");
}
