---
layout: post
title: Calling Assembly Routines from Rust
date: 2025-04-23
comments: false
categories: [ rust assembly asm intel ]
---

# Introduction

Sometimes you just want the raw power of assembly, but still enjoy the ergonomics of Rust. In this article, we'll 
walk through how to call routines in an external `.s` assembly file from your Rust project — the right way, using `build.rs`.

## Project Layout

Your directory structure will look like this:

```
my_asm_rust/
├── Cargo.toml
├── build.rs
├── test.s
├── src/
│   └── main.rs
```

`build.rs` will manage our custom build steps that we'll need. `test.s` houses our assembly routines. The rest you can 
probably figure out!


## Assembly routines

Create `test.s` at the root:

```asm
.intel_syntax noprefix
.text

.global return_zero
return_zero:
    xor rax, rax
    ret

.global add_numbers
add_numbers:
    ; rdi = a, rsi = b
    mov rax, rdi
    add rax, rsi
    ret
```

Two basic functions here. One simply returns the value `0` to the caller, while the other adds two input values 
passed via registers.

Marking these functions as `.global` makes their symbols available to be picked up at link time, so it's key that you 
do this.

## Calling from Rust

In `src/main.rs`:

```rust
extern "C" {
    fn return_zero() -> usize;
    fn add_numbers(a: usize, b: usize) -> usize;
}

fn main() {
    unsafe {
        let zero = return_zero();
        println!("Zero: {}", zero);

        let result = add_numbers(42, 58);
        println!("42 + 58 = {}", result);
    }
}
```

The functions we've defined in the assembly module need to be marked as `extern`. We do this at the top via `extern "C"` 
with `"C"` indicating that we're using the [C calling convention](https://en.wikipedia.org/wiki/X86_calling_conventions) 
- which is the standard way functions pass arguments and return values on most platforms.

{% include callout.html type="warning" title="Note:" text="These functions need to be called in unsafe blocks as the Rust compiler can not guarantee the treatment of resources when they're executing." %}


## Set up a project

```toml
[package]
name = "my_asm_rust"
version = "0.1.0"
edition = "2021"
build = "build.rs"
```

The key here is the `build` entry, which tells Cargo to run our custom build script.

## `build.rs`

Why do we need `build.rs`?

Rust’s build system (Cargo) doesn’t natively compile `.s` files or link in `.o` files unless you explicitly tell it 
to. That’s where `build.rs` comes in — it's a custom build script executed **before** compilation.

Here's what ours looks like:

```rust
use std::process::Command;

fn main() {
    // Compile test.s into test.o
    let status = Command::new("as")
        .args(["test.s", "-o", "test.o"])
        .status()
        .expect("Failed to assemble test.s");

    if !status.success() {
        panic!("Assembly failed");
    }

    // Link the object file
    println!("cargo:rustc-link-search=.");
    println!("cargo:rustc-link-arg=test.o");

    // Rebuild if test.s changes
    println!("cargo:rerun-if-changed=test.s");
}
```

We're invoking `as` to compile the assembly, then passing the resulting object file to the Rust linker.

## Build and Run

```bash
cargo run
```

Expected output:

```
Zero: 0
42 + 58 = 100
```

## Conclusion

You’ve just learned how to:

- Write standalone x86_64 assembly and link it with Rust
- Use `build.rs` to compile and link external object files
- Safely call assembly functions using Rust’s FFI

This is a powerful setup for performance-critical code, hardware interfacing, or even educational tools. You can take 
this further by compiling C code too, or adding multiple `.s` modules for more complex logic.

Happy hacking! 
