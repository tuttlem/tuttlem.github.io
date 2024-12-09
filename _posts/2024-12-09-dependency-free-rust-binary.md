---
layout: post
title: Dependency Free Rust Binary
date: 2024-12-09
comments: false
categories: [ "rust" ]
---

# Introduction

In some situations, you may need to build yourself a [bare machine](https://en.wikipedia.org/wiki/Bare_machine) binary 
file. Some embedded applications can require this, as well as systems programming where you might be building for 
scenarios where you don't have libraries available to you.

In today's post, we'll go through building one of these binaries.

# Getting Started

Let's create a standard binary project to start with.

{% highlight shell %}
cargo new depfree
{% endhighlight %}

This will produce a project that will have the following structure:

{% highlight plain %}
.
├── Cargo.toml
└── src
    └── main.rs
{% endhighlight %}

Your application should have no dependencies:

{% highlight toml %}
[package]
name = "depfree"
version = "0.1.0"
edition = "2021"

[dependencies]
{% endhighlight %}

and, you shouldn't have much in the way of code:

{% highlight rust %}
fn main() {
    println!("Hello, world!");
}
{% endhighlight %}

We build and run this, we should see the very familiar message:

{% highlight plain %}
➜ cargo build
   Compiling depfree v0.1.0 (/home/michael/src/tmp/depfree)
    Finished `dev` profile [unoptimized + debuginfo] target(s) in 0.92s
➜ cargo run  
    Finished `dev` profile [unoptimized + debuginfo] target(s) in 0.02s
     Running `target/debug/depfree`
Hello, world!
{% endhighlight %}

This is already a pretty minimal program. Now our job starts!

# Standard Library

When you build an application, by default all Rust crates will link to the [standard library](https://doc.rust-lang.org/std/).

We can get rid of this by using the `no_std` attribute like so:

{% highlight rust %}
#![no_std]
fn main() {
    println!("Hello, world!");
}
{% endhighlight %}

After a quick re-build, we quickly run into some issues.

{% highlight plain %}
error: cannot find macro `println` in this scope
 --> src/main.rs:3:5
  |
3 |     println!("Hello, world!");
  |     ^^^^^^^

error: `#[panic_handler]` function required, but not found

error: unwinding panics are not supported without std
{% endhighlight %}

Clearly, `println` is no longer available to us, so we'll ditch that line. 

{% highlight rust %}
#![no_std]
fn main() {
}
{% endhighlight %}

We also need to do some extra work around handling our own panics.

# Handling Panics

Without the `no_std` attribute, Rust will setup a panic handler for you. When you have `no_std` specified, this 
implementation no longer exists. We can use the `panic_handler` attribute to nominate a function that will handle our 
panics.

{% highlight rust %}
#![no_std]

use core::panic::PanicInfo;

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop { }
}

fn main() {
}
{% endhighlight %}

Now we've defined a panic handler (called `panic`) that will do nothing more than just spin-loop forever. The 
return type of `!` means that the function won't ever return.

We're also being told that unwinding panics are not supported when we're not using the standard library. To simplify 
this, we can just force panics to abort. We can control this in our `Cargo.toml`:

{% highlight toml %}
[package]
name = "depfree"
version = "0.1.0"
edition = "2021"

[profile.release]
panic = "abort"

[profile.dev]
panic = "abort"

[dependencies]
{% endhighlight %}

We've just disabled unwinding panics in our programs.

If we give this another rebuild now, we get the following:

{% highlight plain %}
error: using `fn main` requires the standard library
  |
  = help: use `#![no_main]` to bypass the Rust generated entrypoint and declare a platform specific entrypoint yourself, usually with `#[no_mangle]`
{% endhighlight %}

This is progress, but it looks like we can't hold onto our `main` function anymore.

# Entry Point

We need to define a new entry point. By using the `no_main` attribute, we are free to *no longer* define a `main` 
function in our program:

{% highlight rust %}
#![no_std]
#![no_main]

use core::panic::PanicInfo;

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop { }
}
{% endhighlight %}

We really have no entry point now. Building this will give you a big horrible error and basically boils down to a 
linker error:

{% highlight plain %}
(.text+0x1b): undefined reference to `main'
/usr/bin/ld: (.text+0x21): undefined reference to `__libc_start_main'
{% endhighlight %}

Fair enough. Our linker is taking exception to the fact that we don't have a `_start` function which is what the 
underlying runtime is going to want to call to start up. The linker will look for this function by default.

So, we can fix that by defining a `_start` function.

{% highlight rust %}
#![no_std]
#![no_main]

use core::panic::PanicInfo;

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop { }
}

#[no_mangle]
pub extern "C" fn _start() -> ! {
    loop { }
}
{% endhighlight %}

The `no_mangle` attribute makes sure that the `_start` function maintains its name, otherwise the compiler will 
use its own creativity and generate a name for you. When it does this, it mangles the name so bad that the linker 
can no longer find it. 

The `extern "C"` is as you'd expect, giving this function C calling conventions.

# The C Runtime

After defining our own `_start` entrypoint, we can give this another build. 

You should see a horrific linker error.

The program that the compiler and linker is trying to produce (for my system here at least) is trying to do so using 
the C runtime. As we're trying to get dependency-free, we need to tell the build chain that we don't want to use this.

In order to do that, we need to build our program for a bare metal target.

You can see all of the targets available for you to install with the following:

{% highlight shell %}
rustc --print=target-list
{% endhighlight %}

You need to find one of those many targets that doesn't have any underlying dependencies. In this example, I've found 
`x86_64-unknown-none`.

Install this runtime:

{% highlight shell %}
rustup target add x86_64-unknown-none
{% endhighlight %}

Let's build!

{% highlight plain %}
➜ cargo build --target x86_64-unknown-none  
   Compiling depfree v0.1.0 (/home/michael/src/tmp/depfree)
    Finished `dev` profile [unoptimized + debuginfo] target(s) in 0.14s
{% endhighlight %}

We've got a build!

# Output

Now we can inspect the binary that we've just produced. `objdump` tells us that we've at least made an elf64:

{% highlight plain %}
target/x86_64-unknown-none/debug/depfree:     file format elf64-x86-64
{% endhighlight %}

Taking a look at our `_start` entrypoint:

{% highlight plain %}
Disassembly of section .text:

0000000000001210 <_start>:
    1210:       eb 00                   jmp    1212 <_start+0x2>
    1212:       eb fe                   jmp    1212 <_start+0x2>
{% endhighlight %}

There's our infinite loop.

# Running, and more

Did you try running that thing?

As expected, the application just stares at you doing nothing. Excellent. It's working.

Let's add some stuff back in. We can start writing a little inline assembly language easy enough to start to do some 
things.

We can import `asm` from the `core::arch` crate:

{% highlight rust %}
use core::arch::asm;

pub unsafe fn exit(code: i32) -> ! {
    let syscall_number: u64 = 60;

    asm!(
        "syscall",
        in("rax") syscall_number,
        in("rdi") code,
        options(noreturn)
    );
}
{% endhighlight %}

The [syscall at 60](https://filippo.io/linux-syscall-table/) is `sys_exit`. In 64-bit style, we load it up in `rax` and 
put the exit code in `rdi`.

We can relax in `_start` point now that it's unsafe:

{% highlight rust %}
#[no_mangle]
pub unsafe fn _start() {
    exit(0);
}
{% endhighlight %}

We can now build this one:

{% highlight shell %}
➜ cargo build --target x86_64-unknown-none
   Compiling depfree v0.1.0 (/home/michael/src/tmp/depfree)
    Finished `dev` profile [unoptimized + debuginfo] target(s) in 0.24s
{% endhighlight %}

We can crack this one open now, and take a look at the underlying implementation.

{% highlight plain %}
Disassembly of section .text:

0000000000001210 <_ZN7depfree4exit17h5d41f4f9db19d099E>:
    1210:       48 83 ec 18             sub    $0x18,%rsp
    1214:       48 c7 44 24 08 3c 00    movq   $0x3c,0x8(%rsp)
    121b:       00 00 
    121d:       89 7c 24 14             mov    %edi,0x14(%rsp)
    1221:       b8 3c 00 00 00          mov    $0x3c,%eax
    1226:       0f 05                   syscall
    1228:       0f 0b                   ud2
    122a:       cc                      int3
    122b:       cc                      int3
    122c:       cc                      int3
    122d:       cc                      int3
    122e:       cc                      int3
    122f:       cc                      int3

0000000000001230 <_start>:
    1230:       50                      push   %rax
    1231:       31 ff                   xor    %edi,%edi
    1233:       e8 d8 ff ff ff          call   1210 <_ZN7depfree4exit17h5d41f4f9db19d099E>
{% endhighlight %}

Unsurprisingly, we're calling our exit implementation which *has been* mangled - you'll notice.

Let's give it a run.

{% highlight plain %}
➜ ./depfree           
➜ echo $?
0
{% endhighlight %}

# Conclusion

Success - we've made some very bare-bones software using Rust and are ready to move onto other embedded and/or 
operating system style applications.
