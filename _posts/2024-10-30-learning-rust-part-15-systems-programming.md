---
layout: post
title: Learning Rust Part 15 - Systems Programming
date: 2024-10-30
comments: false
categories: [ "rust" ]
---

# Introduction

Rust’s combination of low-level control, memory safety, and performance makes it an excellent choice for systems 
programming. Rust supports direct memory management, OS interfacing, and embedded programming while minimizing undefined 
behavior. In this post, we’ll explore essential systems programming topics, including memory management, device driver 
development, and embedded systems programming.

# Low-level Memory Management

Rust’s memory management model enforces safe practices without a garbage collector. Tools like `Box`, `Rc`, `Arc`, and 
`unsafe` allow for direct memory management.

## Using Box for Heap Allocation

`Box<T>` is used for heap allocation, ideal for large data structures that may not fit on the stack. By default, Rust 
allocates on the stack, but `Box` moves data to the heap.

{% highlight rust %}
fn main() {
    let boxed_value = Box::new(10);
    println!("Boxed value: {}", boxed_value);
}
{% endhighlight %}

## Unsafe Rust for Manual Memory Management

Rust ensures safety by default, but `unsafe` blocks enable direct memory access, pointer manipulation, and interfacing 
with other languages, useful for hardware interactions or optimizing critical code paths.

{% highlight rust %}
fn unsafe_memory() {
    let x = 10;
    let r = &x as *const i32;

    unsafe {
        println!("Unsafe pointer dereference: {}", *r);
    }
}
{% endhighlight %}

# Interfacing with Operating System APIs

Rust’s `std::os` and `libc` crates provide access to OS-specific APIs, enabling low-level system calls, process 
management, and file descriptor handling.

## Working with Files and File Descriptors

While `std::fs` handles files at a high level, `std::os::unix` and `std::os::windows` provide OS-specific functionality 
for working with raw file descriptors.

{% highlight rust %}
use std::os::unix::io::{RawFd, AsRawFd};
use std::fs::File;

fn main() -> std::io::Result<()> {
    let file = File::open("example.txt")?;
    let raw_fd: RawFd = file.as_raw_fd();
    println!("Raw file descriptor: {}", raw_fd);
    Ok(())
}
{% endhighlight %}

## Calling OS Functions with libc

The `libc` crate allows calling C library functions directly, giving access to various POSIX functions for low-level 
system programming.

{% highlight rust %}
extern crate libc;
use libc::{getpid, c_int};

fn main() {
    let pid: c_int = unsafe { getpid() };
    println!("Process ID: {}", pid);
}
{% endhighlight %}

# Writing Device Drivers

Rust is increasingly popular for device drivers because of its safety guarantees. While driver development requires 
`unsafe` code to interact directly with hardware, Rust’s borrow checker reduces common errors.

## Example: Writing a Basic Character Device Driver

Creating an actual device driver requires interacting with kernel space. Below is a basic structure that mimics a 
character device driver.

{% highlight rust %}
#![no_std]
#![no_main]

extern crate embedded_hal as hal;
use hal::blocking::serial::Write;
use core::fmt::Write as FmtWrite;

struct Serial;

impl Write<u8> for Serial {
    type Error = ();

    fn bwrite_all(&mut self, buffer: &[u8]) -> Result<(), Self::Error> {
        for &byte in buffer {
            unsafe { core::ptr::write_volatile(0x4000_0000 as *mut u8, byte) };
        }
        Ok(())
    }
}
{% endhighlight %}

This sample initializes a `Serial` struct to write directly to a memory-mapped I/O address.

# Embedded Systems with no_std

Rust’s `no_std` environment enables development without the standard library, essential for embedded systems where 
resources are limited. In `no_std` projects, libraries like `embedded-hal` provide low-level functionalities for 
microcontrollers.

## Creating a no_std Embedded Project

To work in an embedded environment, first disable the standard library by specifying `#![no_std]`. Libraries like 
`cortex-m` and `embedded-hal` provide core functionalities for microcontrollers.

{% highlight rust %}
#![no_std]
#![no_main]

extern crate cortex_m_rt as rt;
use rt::entry;

#[entry]
fn main() -> ! {
    // Your embedded code here
    loop {}
}
{% endhighlight %}

The `#[entry]` macro designates the entry point, while `#![no_std]` removes the dependency on the standard library.

# Building Kernels and Operating Systems

Rust is becoming popular for experimental operating systems and kernel development due to its safety and performance. 
Kernel development in Rust uses `no_std`, allowing low-level hardware control.

## Example Structure for a Basic OS Kernel

To create a basic OS kernel, use `#![no_std]` and `#![no_main]` with a custom entry point, typically `_start`. Since the 
standard library is unavailable, you handle everything directly with low-level code.

{% highlight rust %}
#![no_std]
#![no_main]

use core::panic::PanicInfo;

#[no_mangle]
pub extern "C" fn _start() -> ! {
    loop {}
}

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}
{% endhighlight %}

This code provides a minimal structure for a Rust-based OS kernel, with `_start` as the entry point and a custom 
`panic_handler`.

# Performance Optimizations and Profiling

Rust offers various tools for profiling and optimizing performance, including compiler flags, profiling tools, and 
benchmarking libraries like `criterion`.

## Compiler Flags for Optimization

Using `cargo build --release` enables optimizations, significantly improving performance by enabling Rust’s optimization 
passes.

{% highlight bash %}
cargo build --release
{% endhighlight %}

## Profiling with `perf`

For detailed profiling, Rust projects can use `perf` on Linux to gain insights into CPU usage and performance 
bottlenecks.

### Compile with Release Mode

{% highlight bash %}
cargo build --release
{% endhighlight %}

### Run with perf

{% highlight bash %}
perf record ./target/release/your_binary
perf report
{% endhighlight %}

## Criterion for Benchmarking

`criterion` is a Rust library for benchmarking, providing reliable and statistically sound measurements for performance 
testing.

{% highlight rust %}
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn fibonacci(n: u64) -> u64 {
    match n {
        0 => 1,
        1 => 1,
        n => fibonacci(n - 1) + fibonacci(n - 2),
    }
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("fib 20", |b| b.iter(|| fibonacci(black_box(20))));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
{% endhighlight %}

Run with `cargo bench` to get detailed performance data.

# Summary

Rust’s systems programming capabilities make it an exceptional tool for low-level development. With control over memory, 
access to OS APIs, support for embedded systems, and tools for profiling and optimization, Rust combines safety and 
performance, enabling a wide range of system-level applications.
