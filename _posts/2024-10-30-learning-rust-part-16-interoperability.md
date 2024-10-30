---
layout: post
title: Learning Rust Part 16 - Interoperability
date: 2024-10-30
comments: false
categories: [ "rust" ]
---

# Introduction

Rust’s FFI (Foreign Function Interface) capabilities and rich library support enable it to integrate seamlessly with 
other languages like C, C++, and Python. Rust can also produce shared libraries and handle various data interchange 
formats such as JSON, Protobuf, and MsgPack, making it a great choice for cross-language applications and APIs. This 
post covers essential tools and techniques for interfacing Rust with other languages.

# FFI with C and C++

Rust’s FFI makes it possible to interact directly with C libraries, letting Rust leverage existing C code or integrate 
with languages like C++. The `extern` keyword and the `libc` crate facilitate this interoperability.

## Calling C Functions from Rust

To call a C function, define an `extern` block and use `#[link]` to specify the library. Here’s an example with the C 
`sqrt` function from the math library:

{% highlight rust %}
extern "C" {
    fn sqrt(x: f64) -> f64;
}

fn main() {
    let x = 25.0;
    unsafe {
        println!("sqrt({}) = {}", x, sqrt(x));
    }
}
{% endhighlight %}

## Exposing Rust Functions to C

To expose Rust functions for use in C, use `#[no_mangle]` and declare the function as `extern "C"`. This prevents Rust 
from altering the function name.

{% highlight rust %}
#[no_mangle]
pub extern "C" fn rust_add(a: i32, b: i32) -> i32 {
    a + b
}
{% endhighlight %}

## Interfacing with C++ using the `cxx` crate

The `cxx` crate provides an interface for calling C++ code from Rust and vice versa, handling C++ types like 
`std::string` and `std::vector`.

Add `cxx` to `Cargo.toml` and define a C++ bridge file (`bridge.rs`):

{% highlight rust %}
#[cxx::bridge]
mod ffi {
    extern "C++" {
        include!("example.h");
        fn cpp_function(x: i32) -> i32;
    }
}

fn main() {
    let result = ffi::cpp_function(42);
    println!("Result from C++: {}", result);
}
{% endhighlight %}

# Rust and Python Interfacing with `pyo3`

The `pyo3` crate allows Rust to execute Python code, call Python functions, and even create Python modules directly from 
Rust.

## Calling Python Code from Rust

Use `pyo3` to execute Python code within Rust. First, add `pyo3` to `Cargo.toml`:

{% highlight toml %}
[dependencies]
pyo3 = { version = "0.15", features = ["extension-module"] }
{% endhighlight %}

Then, write a Rust function that interacts with Python:

{% highlight rust %}
use pyo3::prelude::*;

fn main() -> PyResult<()> {
    Python::with_gil(|py| {
        let sys = py.import("sys")?;
        let version: String = sys.get("version")?.extract()?;
        println!("Python version: {}", version);
        Ok(())
    })
}
{% endhighlight %}

## Building a Python Module in Rust

Rust can also create native Python modules. Annotate functions with `#[pyfunction]` and use `#[pymodule]` to define the 
module.

{% highlight rust %}
use pyo3::prelude::*;

#[pyfunction]
fn sum_as_string(a: i64, b: i64) -> PyResult<String> {
    Ok((a + b).to_string())
}

#[pymodule]
fn my_rust_module(py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(sum_as_string, m)?)?;
    Ok(())
}
{% endhighlight %}

Build this as a shared library, and it can be imported into Python just like a native module.

# Building Shared Libraries

Rust can produce shared libraries (e.g., `.dll` on Windows, `.so` on Linux, and `.dylib` on macOS), making it easy to 
share Rust code across multiple languages.

## Compiling Rust to a Shared Library

To build a Rust project as a shared library, set the `crate-type` in `Cargo.toml`:

{% highlight toml %}
[lib]
crate-type = ["cdylib"]
{% endhighlight %}

Then build the library with:

{% highlight bash %}
cargo build --release
{% endhighlight %}

This generates a `.dll`, `.so`, or `.dylib` file, depending on your operating system, which other languages can link to 
and use.

## Using the Shared Library

From another language, import the shared library and call its functions. For instance, in Python, you can use `ctypes` 
to load and call functions from the Rust shared library:

{% highlight plain %}
import ctypes

lib = ctypes.CDLL('./target/release/libmy_rust_lib.so')
result = lib.rust_add(10, 20)
print(f"Result from Rust: {result}")
{% endhighlight %}

# Using Rust with Other Languages

Rust can interface with languages like JavaScript, Ruby, and Go by using FFI or compiling Rust to WebAssembly or shared 
libraries.

## WebAssembly (Wasm) for JavaScript Interoperability

WebAssembly allows Rust code to run in the browser or JavaScript environments. Using `wasm-bindgen`, Rust functions can 
be exposed to JavaScript.

Add `wasm-bindgen` to `Cargo.toml`:

{% highlight rust %}
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn greet(name: &str) -> String {
    format!("Hello, {}!", name)
}
{% endhighlight %}

Build the Rust code as WebAssembly and import it in JavaScript, making Rust interoperable with frontend applications.

# Data Interchange Formats (JSON, Protobuf, MsgPack)

Rust supports serialization formats that allow data interchange with other systems and languages.

## JSON with `serde_json`

The `serde_json` crate is the standard for JSON serialization and deserialization in Rust.

{% highlight rust %}
use serde::{Serialize, Deserialize};
use serde_json;

#[derive(Serialize, Deserialize)]
struct User {
    id: u32,
    name: String,
}

fn main() -> serde_json::Result<()> {
    let user = User { id: 1, name: "Alice".to_string() };
    let json = serde_json::to_string(&user)?;
    println!("Serialized JSON: {}", json);

    let deserialized: User = serde_json::from_str(&json)?;
    println!("Deserialized: {:?}", deserialized);
    Ok(())
}
{% endhighlight %}

## Protobuf with `prost`

Google’s Protocol Buffers (Protobuf) is a fast, language-agnostic format used for efficient data serialization. Rust’s 
`prost` crate generates Rust types from `.proto` files.

Define a `.proto` file for your data structures and use `prost` to generate Rust types.

{% highlight rust %}
use prost::Message;

#[derive(Message)]
struct User {
    #[prost(uint32, tag = "1")]
    pub id: u32,
    #[prost(string, tag = "2")]
    pub name: String,
}
{% endhighlight %}

## MsgPack with `rmp-serde`

MsgPack is a compact, binary format for data serialization, providing efficiency for high-performance applications. 
`rmp-serde` allows Rust to serialize and deserialize MsgPack data using `serde`.

{% highlight rust %}
use serde::{Serialize, Deserialize};
use rmp_serde::{to_vec, from_slice};

#[derive(Serialize, Deserialize, Debug)]
struct User {
    id: u32,
    name: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let user = User { id: 1, name: "Alice".to_string() };
    let msgpack = to_vec(&user)?; // Serialize to MsgPack

    let deserialized: User = from_slice(&msgpack)?; // Deserialize
    println!("Deserialized: {:?}", deserialized);
    Ok(())
}
{% endhighlight %}

## Summary

Rust’s interoperability capabilities make it ideal for building cross-language applications. Whether through FFI, shared 
libraries, or data interchange formats like JSON and Protobuf, Rust can integrate seamlessly with various ecosystems, 
enabling it to act as a high-performance backend or computational layer in multi-language projects.
