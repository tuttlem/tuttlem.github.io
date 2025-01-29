---
layout: post
title: Getting Started with WebAssembly
date: 2025-01-29
comments: false
categories: [ "" ]
---

# Introduction

Previously, we've explored [WASM in rust]({% post_url 2024-12-02-wasm-in-rust %}) as well as some more advanced 
concepts with [Pixel Buffer Rendering]({% post_url 2024-12-07-pixel-buffer-rendering-in-wasm-with-rust %}) again from 
Rust. In today's article, we'll go through WebAssembly from a more fundamental perspective.

WebAssembly (Wasm) is a powerful technology that enables high-performance execution in web browsers and beyond. If 
you're just getting started, this guide will walk you through writing a simple WebAssembly program from scratch, 
running it in a browser using JavaScript.

# What is WebAssembly?

WebAssembly is a low-level binary instruction format that runs at near-native speed. It provides a sandboxed execution 
environment, making it secure and highly portable. While it was initially designed for the web, Wasm is now expanding 
into cloud computing, serverless, and embedded systems.

Unlike JavaScript, Wasm allows near-native performance, making it ideal for gaming, video processing, and even AI in 
the browser.

# First program

Before we start, we need to make sure all of the tools are available on your system. Make sure you have 
[wabt](https://github.com/WebAssembly/wabt) installed on your system:

{% highlight shell %}
sudo pacman -S wabt
{% endhighlight %}

## WAT

We'll start by writing a WebAssembly module using the [WebAssembly Text Format (WAT)](https://webassembly.github.io/spec/core/text/index.html). 
Create a file called add.wat with the following code:

{% highlight text %}
(module
  (func $add (param $a i32) (param $b i32) (result i32)
    local.get $a
    local.get $b
    i32.add
  )
  (export "add" (func $add))
)
{% endhighlight %}

This module defines a function `$add` that takes two 32-bit integers (`i32`) as parameters and returns their sum. 

* `local.get` retrieves the parameters.
* `i32.add` performs the addition.
* The function is exported as `"add"`, making it accessible from JavaScript

## wat2wasm

To convert our `add.wat` file into a `.wasm` binary, we'll use a tool called `wat2wasm` from the 
WebAssembly Binary Toolkit (`wabt`) that we installed earlier:

{% highlight shell %}
wat2wasm add.wat -o add.wasm
{% endhighlight %}

This produces a binary `add.wasm` file, ready for execution.

## Running WebAssembly from Javascript

Now, let's create a JavaScript file (`index.js`) to load and execute our Wasm module:

{% highlight javascript %}
async function runWasm() {
    // Fetch and compile the Wasm module
    const response = await fetch("add.wasm");
    const buffer = await response.arrayBuffer();
    const wasmModule = await WebAssembly.instantiate(buffer);

    // Get the exported add function
    const add = wasmModule.instance.exports.add;

    // Call the function
    console.log("5 + 7 = ", add(5, 7));
}

runWasm();
{% endhighlight %}

We can execute this javascript by referencing it from a html file, and running this in a browser.

Create `index.html` with the following:

{% highlight html %}
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>WebAssembly Example</title>
</head>
<body>
    <h1>WebAssembly Add Example</h1>
    <script src="index.js"></script>
</body>
</html>
{% endhighlight %}

# Testing

Now we can serve our mini-website via python's http server.

{% highlight shell %}
python3 -m http.server 8080
{% endhighlight %}

When you hit [http://localhost:8080](http://localhost:8080) in your browser, pop open your javascript console and you 
should see the following text:

{% highlight text %}
5 + 7 = 12
{% endhighlight %}

# Next

Ok, it's not the most exciting but it is the start of what you can achieve with these technologies.
