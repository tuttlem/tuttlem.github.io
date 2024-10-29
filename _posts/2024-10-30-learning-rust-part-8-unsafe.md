---
layout: post
title: Learning Rust Part 8 - Unsafe
date: 2024-10-30
comments: false
categories: [ "rust" ]
---

# Introduction

Rust is known for its strong safety guarantees, particularly around memory safety, achieved through strict ownership and 
borrowing rules. However, certain low-level operations—like raw pointer manipulation and foreign function 
interfaces—require bypassing these safety checks. 

Rust’s **unsafe** code capabilities provide the necessary control for these cases, but they come with potential risks. 
In this post, we’ll explore unsafe Rust’s capabilities, including raw pointers, unsafe blocks, and FFI (Foreign 
Function Interface), and offer best practices for safe usage.

# Raw Pointers

In Rust, **raw pointers** (`*const T` for immutable and `*mut T` for mutable) enable low-level memory manipulation 
similar to pointers in C. Unlike Rust references (`&` and `&mut`), raw pointers:
 
* Don’t enforce Rust’s borrowing rules.
* Can be null or dangling.
* Are not automatically dereferenced.

## Creating Raw Pointers

Raw pointers are created using the `as` keyword for casting or by using `Box::into_raw` for heap allocations.

{% highlight rust %}
fn main() {
    let x = 42;
    let r1 = &x as *const i32; // Immutable raw pointer
    let r2 = &x as *mut i32;   // Mutable raw pointer
}
{% endhighlight %}

# Dereferencing and Pointer Arithmetic

With raw pointers, you can dereference or manipulate memory addresses directly. However, Rust requires an 
**unsafe block** to perform these actions due to the inherent risks.

## Dereferencing Raw Pointers

Dereferencing a raw pointer retrieves the value it points to. This operation is only allowed in an unsafe block, as 
dereferencing invalid pointers can lead to crashes or undefined behavior.

{% highlight rust %}
fn main() {
    let x = 42;
    let r = &x as *const i32;

    unsafe {
        println!("Value at pointer: {}", *r); // Unsafe dereference
    }
}
{% endhighlight %}

## Pointer Arithmetic

Raw pointers also support pointer arithmetic, allowing you to manually navigate memory addresses. This is especially 
useful for low-level data manipulation.

{% highlight rust %}
fn main() {
    let arr = [10, 20, 30];
    let ptr = arr.as_ptr();

    unsafe {
        println!("Second element: {}", *ptr.add(1)); // Accesses arr[1]
    }
}
{% endhighlight %}

# Unsafe Blocks

Rust confines certain risky operations to **unsafe blocks** to help isolate potentially unsafe code from safe parts of 
the program. This provides flexibility while containing risks.

## Operations Allowed in Unsafe Blocks

Only the following operations are allowed in unsafe blocks:

* Dereferencing raw pointers.
* Calling unsafe functions.
* Accessing or modifying mutable static variables.
* Implementing unsafe traits.
* Accessing union fields.

{% highlight rust %}
unsafe fn dangerous() {
    println!("This function is unsafe to call.");
}

fn main() {
    unsafe {
        dangerous();
    }
}
{% endhighlight %}

# FFI (Foreign Function Interface)

Rust’s **Foreign Function Interface (FFI)** lets you call functions from other languages, such as C, making it valuable 
for systems programming or integrating existing C libraries.

## Declaring an External Function

To call a C function from Rust, use `extern "C"`, which specifies the C calling convention. Here’s an example of calling 
C’s `abs` function to find the absolute value.

{% highlight rust %}
extern "C" {
    fn abs(input: i32) -> i32;
}

fn main() {
    unsafe {
        println!("Absolute value: {}", abs(-5));
    }
}
{% endhighlight %}

## Defining Functions for C

You can also use `extern "C"` to make Rust functions callable from C by adding the `#[no_mangle]` attribute, which 
prevents Rust from renaming the function during compilation.

{% highlight rust %}
#[no_mangle]
pub extern "C" fn my_function() {
    println!("Called from C code!");
}
{% endhighlight %}

# Working with C Libraries

To use external libraries, add the `#[link]` attribute, which specifies the library’s name. For example, here’s how to 
link to the math library (`libm`) for advanced mathematical functions.

## Using C Libraries (Linking and Calling)

The following example demonstrates calling `sqrt` from the math library.

{% highlight rust %}
#[link(name = "m")]
extern "C" {
    fn sqrt(x: f64) -> f64;
}

fn main() {
    unsafe {
        println!("Square root of 9.0 is {}", sqrt(9.0));
    }
}
{% endhighlight %}

> **Note**: You may need to configure linking in your `Cargo.toml` to include the library during compilation.

# Undefined Behavior and Safety

Unsafe Rust allows for operations that, if misused, can lead to **undefined behavior**. Common causes of undefined 
behavior include:

* Dereferencing null or dangling pointers.
* Breaking Rust’s aliasing rules (e.g., multiple mutable references).
* Accessing memory out of bounds.
* Using uninitialized data.

## Safety Tips for Using Unsafe Code

To minimize risks when using unsafe code, follow these best practices:

* **Limit unsafe code to small, well-defined sections** to make it easier to review and understand.
* **Wrap unsafe code in safe abstractions** to prevent direct access to risky operations.
* **Thoroughly review unsafe code**, especially around pointer dereferencing and FFI calls.

By isolating and encapsulating unsafe operations within safe APIs, you can maintain Rust’s safety guarantees while still 
taking advantage of low-level control when necessary.

# Summary

Unsafe Rust provides tools like raw pointers, unsafe blocks, and FFI to extend Rust’s capabilities in low-level 
programming, where direct memory access and foreign function calls are required. While these features offer powerful 
flexibility, they should be used sparingly and with caution to avoid undefined behavior. With proper handling, unsafe 
code can be an invaluable tool, enabling Rust developers to push the boundaries of Rust’s memory safety model without 
sacrificing control or performance.
