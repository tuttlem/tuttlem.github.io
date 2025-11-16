---
layout: post
title: Loading dynamic libraries in Rust
date: 2025-11-15
comments: false
categories: [ rust, ffi, dynamic-linking ]
---

Today’s post is going to be a quick demonstration of loading dynamic libraries at runtime in Rust.

In my [earlier article]({% post_url 2016-11-21-loading-dynamic-libraries-in-c %}), I showed how to use Glibc’s 
[dlopen](https://man7.org/linux/man-pages/man3/dlopen.3.html)/[dlsym](https://man7.org/linux/man-pages/man3/dlsym.3.html)/[dlclose](https://man7.org/linux/man-pages/man3/dlclose.3p.html) 
APIs from C to load a shared object off disk and call a function in it. Rust can do the same thing – with a bit more 
type safety – using:

- a Rust dynamic library compiled as a [cdylib](https://doc.rust-lang.org/reference/linkage.html#r-link.cdylib), and
- the [libloading](https://docs.rs/libloading/latest/libloading/) crate as a safe-ish wrapper around dlopen/dlsym on Unix and [LoadLibrary](https://learn.microsoft.com/en-us/windows/win32/api/libloaderapi/nf-libloaderapi-loadlibrarya)/[GetProcAddress](https://learn.microsoft.com/en-us/windows/win32/api/libloaderapi/nf-libloaderapi-getprocaddress) on Windows.

This is not meant to be a full plugin framework, just a minimal “host loads a tiny library and calls one function” 
example, similar in spirit to the original C version.

## A tiny library in Rust

We’ll start with a tiny dynamic library that exports one function, greet, which returns a C-style string:

{% highlight bash %}
cargo new --lib rust_greeter
cd rust_greeter
{% endhighlight %}

Edit Cargo.toml so that the library is built as a cdylib:

{% highlight rust %}
[package]
name = "rust_greeter"
version = "0.1.0"
edition = "2021"

[lib]
name = "test"                
crate-type = ["cdylib"]      
{% endhighlight %}

Now the library code in src/lib.rs:

{% highlight rust %}
use std::os::raw::c_char;

#[unsafe(no_mangle)]
pub extern "C" fn greet() -> *const c_char {
    static GREETING: &str = "Hello from Rust!\0";
    GREETING.as_ptr().cast()
}
{% endhighlight %}

The `#[unsafe(no_mangle)]` form marks the item (the function) as unsafe to call, and also forwards the nested 
`no_mangle` attribute exactly as written. This avoids needing unsafe fn syntax and keeps ABI-exported functions more 
visually consistent. It’s a small but nice modernisation that fits well when exposing C-compatible symbols from Rust.

Build:

{% highlight rust %}
cargo build --release
{% endhighlight %}

You’ll get:

`target/release/libtest.so`

## Host program: loading the library with libloading

Create a new binary crate:

{% highlight bash %}
cargo new rust_host
cd rust_host
{% endhighlight %}

Add libloading to Cargo.toml:

{% highlight toml %}
[package]
name = "rust_host"
version = "0.1.0"
edition = "2021"

[dependencies]
libloading = "0.8"
{% endhighlight %}

And `src/main.rs`:

{% highlight rust %}
use std::error::Error;
use std::ffi::CStr;
use std::os::raw::c_char;

use libloading::{Library, Symbol};

type GreetFn = unsafe extern "C" fn() -> *const c_char;

fn main() -> Result<(), Box<dyn Error>> {
    unsafe {
        let lib = Library::new("./libtest.so")?;
        let greet: Symbol<GreetFn> = lib.get(b"greet\0")?;

        let raw = greet();
        let c_str = CStr::from_ptr(raw);
        let message = c_str.to_str()?;

        println!("{message}");
    }
    Ok(())
}
{% endhighlight %}

Before we can run any of this, we need to make sure the library is available to the host program. In order to do this, 
we simply copy over the library:

{% highlight bash %}
cp ../rust_greeter/target/release/libtest.so .
{% endhighlight %}

Just copy the `so` over to the host program folder.

Running cargo run prints:

{% highlight bash %}
$ cargo run                                     
   Finished `dev` profile [unoptimized + debuginfo] target(s) in 0.01s
    Running `target/debug/rust_host`
Hello from Rust!
{% endhighlight %}

# Mapping back to the C version

When you look at this code, you can see that `Library::new("./libtest.so")` now takes the place of `dlopen()`.

We can get to the symbol that we want to call with `lib.get(b"greet\0")` rather than `dlsym()`, and we clean everything 
up now by just dropping the library.

# Platform notes

Keep in mind that I've written this code on my linux machine, so you'll have different targets depending on the 
platform that you work from.

| Platform | Output |
|----------|----------------------|
| Linux | `libtest.so` |  
| macOS | `libtest.dylib` |  
| Windows | `test.dll` |  

`cdylib` produces the correct format automatically.

# Conclusion

We:

- built a tiny Rust cdylib exporting a C-ABI function,
- loaded it at runtime with libloading,
- looked up a symbol by name, and
- invoked it through a typed function pointer.

I guess this was just a modern update to an existing article.

Just like in the C post, this is a deliberately minimal skeleton — but enough to grow into a proper plugin architecture 
once you define a stable API between host and library.
