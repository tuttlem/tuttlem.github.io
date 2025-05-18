---
layout: post
title: Building a Minimal JIT Compiler in Rust with Cranelift
date: 2025-05-18
comments: false
categories: [ rust, jit, compilers, cranelift, codegen ]
---

# Introduction

Most of the time, we think of programs as static — we write code, compile it, and run it. But what if our programs 
could generate and execute **new code at runtime**?

This technique, called **dynamic code generation**, underpins technologies like:

- High-performance JavaScript engines (V8, SpiderMonkey)
- Regex engines (like RE2's code generation)
- AI compilers like TVM or MLIR-based systems
- Game scripting engines
- Emulators and binary translators

In this post, we’ll explore the idea of **just-in-time compilation (JIT)** using Rust and a powerful but approachable 
backend called **Cranelift**.

Rather than building a full language or VM, we'll create a simple JIT compiler that can dynamically compile a function 
like:

{% highlight rust %}
fn add(a: i32, b: i32) -> i32 {
  a + b
}
{% endhighlight %}

And run it — at runtime.

Let’s break this down step by step.

# What is Cranelift?

[Cranelift](https://github.com/bytecodealliance/wasmtime/tree/main/cranelift) is a low-level code generation framework 
built by the Bytecode Alliance. It's designed for:

- **Speed**: It compiles fast, making it ideal for JIT scenarios.
- **Portability**: It works across platforms and architectures.
- **Safety**: It's written in Rust, and integrates well with Rust codebases.

Unlike LLVM, which is a powerful but heavyweight compiler infrastructure, Cranelift is laser-focused on 
**emitting machine code** with minimal overhead.

# Dependencies

First up, we have some dependencies that we need to install into the project.

{% highlight toml %}
[dependencies]
cranelift-jit = "0.119"
cranelift-module = "0.119"
cranelift-codegen = "0.119"
cranelift-frontend = "0.119"
{% endhighlight %}

# The Cdde

## Context Setup

We begin by creating a JIT context using Cranelift’s `JITBuilder` and `JITModule`:

{% highlight rust %}
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
let mut builder = JITBuilder::new(cranelift_module::default_libcall_names())?;
let mut module = JITModule::new(builder);

    // ...
    Ok(())
}
{% endhighlight %}

This sets up a dynamic environment where we can define and compile functions on the fly.

## The Function Signature

Next, we define the function signature for our `add(i32, i32) -> i32` function:

{% highlight rust %}
use cranelift_codegen::ir::{types, AbiParam};

let mut sig = module.make_signature();
sig.params.push(AbiParam::new(types::I32));
sig.params.push(AbiParam::new(types::I32));
sig.returns.push(AbiParam::new(types::I32));
{% endhighlight %}

This tells Cranelift the number and type of arguments and the return value.

## Declaring the Function

We now declare this function in the module:

{% highlight rust %}
let func_id = module.declare_function("add", Linkage::Export, &sig)?;
{% endhighlight %}

This returns a `FuncId` we’ll use to reference and later finalize the function.

Now we build out the fuction body.

This is where we emit Cranelift IR using `FunctionBuilder`.

{% highlight rust %}
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_codegen::ir::InstBuilder;

let mut ctx = module.make_context();
ctx.func.signature = sig;

let mut builder_ctx = FunctionBuilderContext::new();
let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_ctx);

let block = builder.create_block();
builder.append_block_params_for_function_params(block);
builder.switch_to_block(block);
builder.seal_block(block);

// Extract arguments
let a = builder.block_params(block)[0];
let b = builder.block_params(block)[1];

// Perform addition and return
let sum = builder.ins().iadd(a, b);
builder.ins().return_(&[sum]);

builder.finalize();
{% endhighlight %}

This constructs a Cranelift function that takes two `i32`s, adds them, and returns the result.

#3 Compiling and Executing

Once the IR is built, we compile and retrieve a function pointer:

{% highlight rust %}
module.define_function(func_id, &mut ctx)?;
module.clear_context(&mut ctx);
module.finalize_definitions();

let code_ptr = module.get_finalized_function(func_id);
let func = unsafe { std::mem::transmute::<_, fn(i32, i32) -> i32>(code_ptr) };

let result = func(7, 35);
println!("7 + 35 = {}", result);
{% endhighlight %}

Because we’re turning a raw pointer into a typed function, this step is `unsafe`. We promise the runtime that we’ve 
constructed a valid function that respects the signature we declared.

# Final Result

When run, the output is:

{% highlight text %}
7 + 35 = 42
{% endhighlight %}

We dynamically constructed a function, compiled it, and executed it — **at runtime**, without ever writing that 
function directly in Rust!

# Where to Go From Here

This is just the beginning. Cranelift opens the door to:

- Building interpreters with optional JIT acceleration
- Creating domain-specific languages (DSLs)
- Writing high-performance dynamic pipelines (e.g. for graphics, audio, AI)
- Implementing interactive REPLs with on-the-fly function definitions

You could expand this project by:

- Parsing arithmetic expressions and generating IR
- Adding conditionals or loops
- Exposing external functions (e.g. math or I/O)
- Dumping Cranelift IR for inspection

{% highlight rust %}
println!("{}", ctx.func.display());
{% endhighlight %}

# Conclusion

Dynamic code generation feels like magic — and Cranelift makes it approachable, fast, and safe.

In a world where flexibility, speed, and composability matter, being able to build and run code at runtime is a 
**superpower**. Whether you're building a toy language, optimizing a runtime path, or experimenting with compiler 
design, Cranelift is a fantastic tool to keep in your Rust toolbox.

If this post helped you peek behind the curtain of JIT compilers, I’d love to hear from you. Let me know if you'd 
like to see this example expanded into a real toy language!
