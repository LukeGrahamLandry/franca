use std::{
    env,
    path::{Path, PathBuf},
    process::Command,
};

fn main() {
    let out = env::var("OUT_DIR").unwrap();
    let mir_repo = format!("{out}/mir");
    let mir_repo = Path::new(&mir_repo);
    if !mir_repo.exists() {
        assert!(Command::new("git")
            .current_dir(out)
            .args(["clone", "https://github.com/vnmakarov/mir.git"])
            .status()
            .unwrap()
            .success())
    }

    let bindings = bindgen::Builder::default()
        .clang_arg(format!("-I{}", mir_repo.to_str().unwrap()))
        .header(mir_repo.join("mir.h").to_str().unwrap())
        .header(mir_repo.join("c2mir/c2mir.h").to_str().unwrap())
        .allowlist_var(r#"(\w*MIR\w*)"#)
        .allowlist_type(r#"(\w*MIR\w*)"#)
        .allowlist_function(r#"(\w*MIR\w*)"#)
        .allowlist_var(r#"(\w*c2mir\w*)"#)
        .allowlist_type(r#"(\w*c2mir\w*)"#)
        .allowlist_function(r#"(\w*c2mir\w*)"#)
        .opaque_type("FILE")
        .generate()
        .expect("Unable to generate bindings");

    // Write the bindings to the $OUT_DIR/bindings.rs file.
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings.write_to_file(out_path.join("bindings.rs")).expect("Couldn't write bindings!");

    let mut build = cc::Build::new();

    build
        .include(mir_repo)
        .file(mir_repo.join("mir.c"))
        .file(mir_repo.join("mir-gen.c"))
        .file(mir_repo.join("c2mir/c2mir.c"))
        .compile("mir");
}
