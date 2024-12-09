---
layout: post
title: Pixel Buffer Rendering in WASM with Rust
date: 2024-12-07
comments: false
categories: [ "rust", "wasm", "graphics" ]
---

# Introduction

In our [previous post]({% post_url 2024-12-02-wasm-in-rust %}), we introduced writing WebAssembly (WASM) programs using Rust. This time, we’ll dive into pixel 
buffer rendering, a technique that allows direct manipulation of image data for dynamic graphics. This method, inspired 
by old-school demo effects, is perfect for understanding low-level rendering concepts and building your first custom 
graphics renderer.

By the end of this tutorial, you’ll have a working Rust-WASM project that renders graphics to a `<canvas>` element in a 
web browser.

# Setting Up

Start by creating a new Rust project. 

{% highlight shell %}
wasm-pack new randypix
{% endhighlight %}

Ensure that your `Cargo.toml` is configured for WASM development:

{% highlight toml %}
[package]
name = "randypix"
version = "0.1.0"
edition = "2021"

[lib]
crate-type = ["cdylib", "rlib"]

[dependencies]
wasm-bindgen = "0.2"
web-sys = { version = "0.3", features = ["Window", "Document", "HtmlCanvasElement", "CanvasRenderingContext2d", "ImageData"] }
js-sys = "0.3"

[dev-dependencies]
wasm-bindgen-cli = "0.2"
{% endhighlight %}

# Writing the Code

The heart of our implementation is the `lib.rs` file, which handles all interactions between Rust, WebAssembly, and 
the browser. 

Here’s the complete code:

{% highlight rust %}
use wasm_bindgen::prelude::*;
use wasm_bindgen::Clamped;
use wasm_bindgen::JsCast;
use web_sys::{CanvasRenderingContext2d, HtmlCanvasElement, ImageData};

#[wasm_bindgen(start)]
pub fn start() -> Result<(), JsValue> {
    // Access the document and canvas
    let document = web_sys::window().unwrap().document().unwrap();
    let canvas = document
        .get_element_by_id("demo-canvas")
        .unwrap()
        .dyn_into::<HtmlCanvasElement>()
        .unwrap();

    let context = canvas
        .get_context("2d")?
        .unwrap()
        .dyn_into::<CanvasRenderingContext2d>()
        .unwrap();

    let width = canvas.width() as usize;
    let height = canvas.height() as usize;

    // Create a backbuffer with RGBA pixels
    let mut backbuffer = vec![0u8; width * height * 4];

    // Fill backbuffer with a simple effect (e.g., gradient)
    for y in 0..height {
        for x in 0..width {
            let offset = (y * width + x) * 4;
            backbuffer[offset] = (x % 256) as u8;        // Red
            backbuffer[offset + 1] = (y % 256) as u8;    // Green
            backbuffer[offset + 2] = 128;               // Blue
            backbuffer[offset + 3] = 255;               // Alpha
        }
    }

    // Create ImageData from the backbuffer
    let image_data = ImageData::new_with_u8_clamped_array_and_sh(
        Clamped(&backbuffer), // Wrap the slice with Clamped
        width as u32,
        height as u32,
    )?;

    // Draw the ImageData to the canvas
    context.put_image_data(&image_data, 0.0, 0.0)?;

    Ok(())
}
{% endhighlight %}

### Explanation:
1. **Canvas Access**:
   - The `HtmlCanvasElement` is retrieved from the DOM using `web_sys`.
   - The 2D rendering context (`CanvasRenderingContext2d`) is obtained for drawing.

2. **Backbuffer Initialization**:
   - A `Vec<u8>` is used to represent the RGBA pixel buffer for the canvas.

3. **Filling the Buffer**:
   - A simple nested loop calculates pixel colors to create a gradient effect.

4. **Drawing the Buffer**:
   - The pixel data is wrapped with `Clamped`, converted to `ImageData`, and drawn onto the canvas with `put_image_data`.

# Setting Up the Frontend

The frontend consists of a single `index.html` file, which hosts the canvas and loads the WASM module:

{% highlight html %}
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Rust WebAssembly Demo</title>
</head>
<body>
<canvas id="demo-canvas" width="800" height="600"></canvas>
<script type="module">
    import init from './pkg/randypix.js';
    init();
</script>
</body>
</html>
{% endhighlight %}

# Building and Running the Project

Follow these steps to build and run your project:

1. **Build the WASM Module**:
   Use `wasm-pack` to compile your Rust project into a WASM package:
   {% highlight shell %}
   wasm-pack build --target web
   {% endhighlight %}

2. **Serve the Project**:
   Use a simple HTTP server to serve the `index.html` and the generated `pkg` folder:
   {% highlight shell %}
   python -m http.server
   {% endhighlight %}

3. **Open in Browser**:
   Navigate to [http://localhost:8000](http://localhost:8000) in your browser. You should see a gradient rendered on the canvas.

# Conclusion

In this tutorial, we demonstrated how to create and render a pixel buffer to a canvas using Rust and WebAssembly. By 
leveraging `wasm-bindgen` and `web-sys`, we seamlessly integrated Rust with web APIs, showcasing its potential for 
high-performance graphics programming in the browser.

This example serves as a foundation for more advanced rendering techniques, such as animations, interactive effects, or 
even game engines. Experiment with the backbuffer logic to create unique visuals or introduce dynamic updates for an 
animated experience!
