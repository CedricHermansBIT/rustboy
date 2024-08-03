# Rustboy

This is a Gameboy emulator written in Rust. It is a work in progress and is not yet complete.

## Building

To build the project, you will need to have Rust installed. You can install Rust by following the instructions on the [official website](https://www.rust-lang.org/tools/install).

You will also need to install `wasm-pack` and `wasm-bindgen`:

```sh
cargo install wasm-pack
cargo install wasm-bindgen-cli
```

Once you have Rust and wasm installed, you can build the project by running the following command:

```sh
wasm-pack build --target web && wasm-bindgen --target web --out-dir ./out ./target/wasm32-unknown-unknown/release/rustboy.wasm
```
