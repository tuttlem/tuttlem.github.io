---
layout: post
title: Basic Animation in WASM with Rust
date: 2024-12-14
comments: false
categories: [ "wasm", "rust" ]
---

# Introduction

In a [previous post]({% post_url 2024-12-07-pixel-buffer-rendering-in-wasm-with-rust %}) we covered the basic setup on 
drawing to a `<canvas>` object via WebAssembly (WASM). In today's article, we'll create animated graphics directly on a 
HTML5 canvas.

We’ll break down the provided code into digestible segments and walk through each part to understand how it works. By 
the end of this article, you’ll have a clear picture of how to:

1. Set up an HTML5 canvas and interact with it using Rust and WebAssembly.
2. Generate random visual effects with Rust’s `rand` crate.
3. Build an animation loop with `requestAnimationFrame`.
4. Use shared, mutable state with `Rc` and `RefCell` in Rust.

Let's get started.

# Walkthrough

I won't cover the project setup and basics here. The [previous post]({% post_url 2024-12-07-pixel-buffer-rendering-in-wasm-with-rust %}) 
has all of that information for you. I will cover some dependencies that you need for your project here:

{% highlight toml %}
[dependencies]
wasm-bindgen = "0.2"
web-sys = { version = "0.3", features = ["Window", "Document", "HtmlCanvasElement", "CanvasRenderingContext2d", "ImageData"] }
js-sys = "0.3"
rand = { version = "0.8" }
getrandom = { version = "0.2", features = ["js"] }

[dev-dependencies]
wasm-bindgen-cli = "0.2"
{% endhighlight %}

There's a number of `features` in use there from `web-sys`. These will become clearer as we go through the code. The 
`getrandom` dependency has [web assembly](https://docs.rs/getrandom/latest/getrandom/#webassembly-support) support so 
we can use this to make our animations slightly generative. 

## Getting Browser Access

First thing we'll do is to define some helper functions that will try and acquire different features in the browser.

We need to be able to access the browser's `window` object.

{% highlight rust %}
fn window() -> web_sys::Window {
    web_sys::window().expect("no global `window` exists")
}
{% endhighlight %}

This function requests the common `window` object from the Javascript environment. The `expect` will give us an error 
context if it fails, telling us that no window exists.

We use this function to get access to [requestAnimationFrame](https://developer.mozilla.org/en-US/docs/Web/API/Window/requestAnimationFrame) 
from the browser.

{% highlight rust %}
fn request_animation_frame(f: &Closure<dyn FnMut()>) {
    window()
        .request_animation_frame(f.as_ref().unchecked_ref())
        .expect("should register `requestAnimationFrame` OK");
}
{% endhighlight %}

The function being requested here is documented as the `callback`. 

> The `window.requestAnimationFrame()` method tells the browser you wish to perform an animation. It requests the browser to call a user-supplied callback function before the next repaint.

This will come in handy to do our repaints.

Now, in our `run` function, we can start to access parts of the HTML document that we'll need references for. Sitting in
our HTML template, we have the `<canvas>` tag that we want access to:

{% highlight html %}
<canvas id="demo-canvas" width="800" height="600"></canvas>
{% endhighlight %}

We can get a handle to this `<canvas>` element, along with the 2d drawing context with the following:

{% highlight rust %}
let canvas = crate::document()
    .get_element_by_id("demo-canvas")
    .unwrap()
    .dyn_into::<HtmlCanvasElement>()
    .unwrap();

let context = canvas
    .get_context("2d")?
    .unwrap()
    .dyn_into::<CanvasRenderingContext2d>()
    .unwrap();
{% endhighlight %}

## Create our Double-Buffer

When we double-buffer graphics, we need to allocate the block of memory that will act as our "virtual screen". We draw 
to that virtual screen, and then "flip" or "blit" that virtual screen (piece of memory) onto video memory to give the 
graphics movement.

{% highlight rust %}
let width = canvas.width() as usize;
let height = canvas.height() as usize;
let mut backbuffer = vec![0u8; width * height * 4];
{% endhighlight %}

The size of our buffer will be `width * height * number_of_bytes_per_pixel`. With a red, green, blue, and alpha channel 
that makes 4 bytes.

## Animation Loop

We can now setup our animation loop. 

This approach allows the closure to reference itself so it can schedule the next frame, solving Rust's strict ownership 
and borrowing constraints.

{% highlight rust %}
let f = Rc::new(RefCell::new(None));
let g = f.clone();

*g.borrow_mut() = Some(Closure::new(move || {
    // do the animation code here

    // queue up another re-draw request
    request_animation_frame(f.borrow().as_ref().unwrap());
});

// queue up the first re-draw request, to start animation
request_animation_frame(g.borrow().as_ref().unwrap());
{% endhighlight %}

This pattern is common in Rust for managing shared, mutable state when working with closures in scenarios where you need 
to reference a value multiple times or recursively, such as with event loops or callback-based systems. Let me break it 
down step-by-step:

### The Components

1. `Rc` **(Reference Counted Pointer)**:
   * `Rc` allows multiple ownership of the same data by creating a reference-counted pointer. When the last reference to the data is dropped, the data is cleaned up.
   * In this case, it enables both `f` and `g` to share ownership of the same `RefCell`.
   
2. `RefCell` **(Interior Mutability)**:
   * `RefCell` allows mutable access to data even when it is inside an immutable container like `Rc`.
   * This is crucial because `Rc` itself does not allow mutable access to its contents by design (to prevent race conditions in a single-threaded context).
   
3. `Closure`:
   * A closure in Rust is a function-like construct that can capture variables from its surrounding scope.
   * In the given code, a `Closure` is being stored in the `RefCell` for later use.

### What's Happening Here?

1. **Shared Ownership**:
   * `Rc` is used to allow multiple references (`f` and `g`) to the same underlying `RefCell`. This is required because the closure may need to reference `f` while being stored in it, which is impossible without shared ownership.
   
2. **Mutation with RefCell**:
   * `RefCell` enables modifying the underlying data (`None` → `Some(Closure)`) despite `Rc` being immutable.
   
3. Setting the Closure:
   * The closure is created and stored in the `RefCell` via `*g.borrow_mut()`.
   * This closure may reference `f` for recursive or repeated access.

We follow this particular pattern here because the closure needs access to itself in order to recursively schedule calls 
to `requestAnimationFrame`. By storing the closure in the `RefCell`, the closure can call itself indirectly.

If we didn't use this pattern, we'd have some lifetime/ownership issues. Referencing the closure while defining it 
would create a circular reference problem that Rust wouldn't allow.

## Drawing

We're going to find a random point on our virtual screen to draw, and we're going to pick a random shade of grey. We're 
going to need a random number generator:

{% highlight rust %}
let mut rng = rand::thread_rng();
{% endhighlight %}

`rng` is now a [thread-local generator](https://rust-random.github.io/rand/rand/fn.thread_rng.html) of random numbers.

We get a random location in our virtual screen, and calculate the offset `o` to draw at using those values.

{% highlight rust %}
let rx = (rng.gen::<f32>() * width as f32) as i32;
let ry = (rng.gen::<f32>() * height as f32) as i32;
let o = ((rx + (ry * width as i32)) * 4) as usize;
{% endhighlight %}

Now, it's as simple as setting 4 bytes from that location:

{% highlight rust %}
backbuffer[o] = red;
backbuffer[o + 1] = green;
backbuffer[o + 2] = blue;
backbuffer[o + 3] = alpha;
{% endhighlight %}

## Blitting

Blitting refers to copying pixel data from the backbuffer to the canvas in a single operation. This ensures the displayed 
image updates smoothly

Now we need to blit that back buffer onto our canvas. We need to create an `ImageData` object in order to do this. 
Passing in our `backbuffer` object, we can create one with the following:

{% highlight rust %}
let image_data = ImageData::new_with_u8_clamped_array_and_sh(
    Clamped(&backbuffer), // Wrap the slice with Clamped
    width as u32,
    height as u32,
).unwrap();
{% endhighlight %}

We then use our 2d context to simply draw the image:

{% highlight rust %}
context.put_image_data(&image_data, 0.0, 0.0).unwrap();
{% endhighlight %}

# Conclusion

And there you have it—a complete walkthrough of creating dynamic canvas animations with Rust and WebAssembly! We covered 
how to:

* Set up the canvas and prepare a backbuffer for pixel manipulation.
* Use Rust’s `rand` crate to generate random visual effects.
* Manage mutable state with `Rc` and `RefCell` for animation loops.
* Leverage `requestAnimationFrame` to achieve smooth, frame-based updates.

This approach combines Rust’s strengths with the accessibility of modern web technologies, allowing you to build fast, 
interactive graphics directly in the browser.

A [gist of the full code](https://gist.github.com/tuttlem/d148302620287649aad27b1c5391f3b0) is also available.