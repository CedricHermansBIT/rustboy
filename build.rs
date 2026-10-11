fn main() {
    // The opt-in browser JIT installs compiled WASM functions in Rust's table.
    if std::env::var("CARGO_CFG_TARGET_ARCH").as_deref() == Ok("wasm32")
        && std::env::var_os("CARGO_FEATURE_GBA_JIT").is_some()
    {
        println!("cargo:rustc-link-arg=--growable-table");
    }
}
