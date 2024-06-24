fn main() {
    // TODO: auto switch to boot/libfranca.a the first time.
    let path = "compiler/target/libfranca.a";
    println!("cargo:rerun-if-changed={}", path);
    println!("cargo:rustc-link-search=native=compiler/target/");
    println!("cargo:rustc-link-lib=static=franca");
}
