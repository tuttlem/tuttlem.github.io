---
layout: post
title: WASM in Rust
date: 2024-12-02
comments: false
categories: [ "rust", "wasm" ]
---

# Introduction

WebAssembly (WASM) is a binary instruction format designed for fast execution in web browsers and other environments. 
It enables developers to write code in languages like C, C++, or Rust, compile it to a highly efficient binary format, 
and execute it directly in the browser. This makes WASM an exciting technology for building high-performance 
applications that run alongside JavaScript.

Rust, with its emphasis on safety, performance, and WebAssembly support, has become a popular choice for developers 
working with WASM. In this tutorial, we'll explore how to use Rust to produce and interact with WASM modules, showcasing 
its ease of integration with JavaScript.

# Setup

To get started, we’ll use Rust’s `nightly` version, which provides access to experimental features. You can install it 
via `rustup`:

{% highlight shell %}
rustup install nightly
{% endhighlight %}

Next, install [wasm-pack](https://github.com/rustwasm/wasm-pack). 

> This tool seeks to be a one-stop shop for building and working with rust- generated WebAssembly that you would like to interop with JavaScript, in the browser or with Node.js.

{% highlight shell %}
cargo install wasm-pack
{% endhighlight %}

Now we’re ready to set up our project. Create a new WASM project using `wasm-pack`:

{% highlight shell %}
wasm-pack new hello-wasm
{% endhighlight %}

This will generate a new project in a folder named `hello-wasm`.

# Project Structure

Once the project is created, you’ll see the following directory structure:

{% highlight plain %}
.
├── Cargo.toml
├── LICENSE_APACHE
├── LICENSE_MIT
├── README.md
├── src
│   ├── lib.rs
│   └── utils.rs
└── tests
    └── web.rs

3 directories, 7 files
{% endhighlight %}

To ensure the project uses the nightly version of Rust, set an override for the project directory:

{% highlight shell %}
rustup override set nightly
{% endhighlight %}

This tells Rust tools to use the nightly toolchain whenever you work within this directory.

# The Code

Let’s take a look at the code generated in `./src/lib.rs`:

{% highlight rust %}
mod utils;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    fn alert(s: &str);
}

#[wasm_bindgen]
pub fn greet() {
    alert("Hello, hello-wasm!");
}
{% endhighlight %}

This code introduces WebAssembly bindings using the `wasm-bindgen` crate. It defines an external JavaScript function, 
`alert`, and creates a public Rust function, `greet`, which calls this alert. This demonstrates how Rust code can 
interact seamlessly with JavaScript.

# Building the WASM Module

To compile the project into a WASM module, run the following command:

{% highlight shell %}
wasm-pack build --target web
{% endhighlight %}

After a successful build, you’ll see a `pkg` folder containing the WASM file (`hello_wasm_bg.wasm`) and JavaScript 
bindings (`hello_wasm.js`).

# Hosting and Running the Module

To test the WASM module in the browser, we need an HTML file to load and initialize it. Create a new `index.html` file 
in your project root:

{% highlight html %}
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>WASM Example</title>
</head>
<body>
    <script type="module">
        import init, { greet } from "./pkg/hello_wasm.js";

        // Initialize the WASM module and call the function
        (async () => {
            await init();
            greet();
        })();
    </script>
</body>
</html>
{% endhighlight %}

This script:
1. Imports the `init` function and the `greet` function from the WASM module.
2. Initializes the WASM module using `init`.
3. Calls `greet`, which triggers the JavaScript `alert`.

To serve the project locally, start a simple HTTP server:

{% highlight shell %}
python -m http.server
{% endhighlight %}

Visit [http://localhost:8000](http://localhost:8000) in your browser. You should see a JavaScript alert box with the 
message `"Hello, hello-wasm!"`.

# Conclusion

WebAssembly, combined with Rust, opens up exciting possibilities for writing high-performance web applications. In this 
guide, we walked through the process of setting up a Rust project, writing a WASM module, and interacting with it in the 
browser. With tools like `wasm-pack` and `wasm-bindgen`, Rust provides a seamless developer experience for building 
cross-language applications.

Whether you’re adding computationally intensive features to your web app or exploring the power of WebAssembly, Rust is 
an excellent choice for the journey.
