---
layout: post
title: Learning Rust Part 1 - Language Basics
date: 2024-10-29
comments: false
categories: [ "rust" ]
---

# Introduction

Welcome to our series on the Rust programming language! Rust has been gaining a lot of attention in the programming 
community thanks to its focus on performance, safety, and concurrency. Originally developed by Mozilla, Rust is designed 
to eliminate many common programming errors at compile time, particularly around memory safety and data races, making it 
an appealing choice for systems programming and applications requiring high reliability.

In this series, we'll start with Rust basics, gradually diving into its unique features and core language concepts. 
Whether you’re coming from a background in languages like C++, Python, or JavaScript, or completely new to programming, 
this series will help you build a strong foundation in Rust. We’ll look at its syntax and semantics, explore how 
ownership works, and understand the lifetimes of data—key concepts that make Rust unique.

This first post will guide you through the language essentials, laying the groundwork for deeper topics in future posts. 

We’ll cover the following language basics:

* **Syntax and Semantics**
We’ll start with an overview of Rust’s syntax and how it differs from other languages. You’ll learn about basic 
expressions, code structure, and how Rust’s strict compiler enforces code quality.

* **Variables and Mutability**
Rust’s approach to variables and mutability is unique among languages, emphasizing safety by making all variables 
immutable by default. We’ll explain why this is and how to work with mutable variables when needed.

* **Data Types**
Rust is a statically typed language, which means the type of each variable must be known at compile time. We’ll explore 
Rust’s basic data types and how they’re used in programs.

* **Primitive Types**
Rust offers a range of primitive types, including integers, floating-point numbers, booleans, and characters. 
Understanding these types and how to work with them is crucial as you start writing Rust code.

* **Constants and Static Variables**
Constants and static variables are essential for defining fixed values in Rust. We’ll explain the differences between 
them, as well as when and why to use each.

* **Control Structures**
Control structures are the basic building blocks for controlling the flow of execution in your programs. We'll show you 
how to use the familiar keywords `if`, `loop`, `while`, and `for`.

* **Pattern Matching**
Pattern matching is a powerful feature in Rust, providing expressive syntax for conditional branching. We’ll show you 
how to use the `match` statement and other forms of pattern matching effectively.

* **Functions and Closures**
Finally, we’ll cover functions and closures. Rust’s functions are straightforward, but closures (anonymous functions) 
bring flexibility to Rust’s syntax, especially for functional programming patterns.

Each section in this post is designed to build on the last, creating a comprehensive introduction to the Rust language’s 
basics. By the end, you’ll have a solid understanding of Rust's core language features and a foundation to explore more 
advanced concepts in subsequent posts.

# Syntax and Semantics

## Basic Program Structure

Every Rust program begins execution in a function named `main`. Unlike some languages where a main function is optional, 
Rust requires `fn main()` as an entry point.

{% highlight rust %}
fn main() {
    println!("Hello, world!");
}
{% endhighlight %}

**Breaking It Down**
* `fn` defines a function, followed by the function name `main`.
* `()` indicates that main takes no parameters in this example.
* Curly braces `{}` are used to define the function's scope.
* `println!` is a macro that prints text to the console, with a `!` indicating it’s a macro rather than a function. Rust macros are powerful, but for now, think of `println!` as equivalent to print or printf in other languages.

## Expressions and Statements

Rust is an _expression-based_ language, which means that many parts of the code return a value. For example, the final 
line of a block (without a semicolon) can act as a return value:

{% highlight rust %}
fn add_one(x: i32) -> i32 {
    x + 1 // No semicolon, so this returns the value of `x + 1`
}
{% endhighlight %}

* **Expressions** (like `x + 1` above) return a value and don’t end in a semicolon.
* **Statements** perform actions but don’t return a value, often ending with a semicolon.

Rust’s expression-based nature allows for concise and functional-looking code, as shown below:

{% highlight rust %}
let result = if x > 0 { x } else { -x }; // Inline expression in an `if` statement
{% endhighlight %}

## Enforced Code Quality: Compiler Strictness

Rust’s compiler is notoriously strict, which is a feature, not a bug! This strictness catches common mistakes and 
enforces safe memory practices. Here’s how it affects code structure and quality:

**Unused Variables**: The compiler warns about unused variables, nudging you to write clean, intentional code.

{% highlight rust %}
let x = 42; // Warning if `x` is unused
{% endhighlight %}

You can silence these warnings by prefixing variables with an underscore:

{% highlight rust %}
let _x = 42; 
{% endhighlight %}

**Immutable by Default**: Variables are immutable unless explicitly marked with mut, encouraging safer programming 
patterns.

{% highlight rust %}
let mut counter = 0; // `counter` can now be modified
counter += 1;
{% endhighlight %}

**Type Inference with Explicit Typing Encouragement**: Rust’s compiler can infer types, but you can (and sometimes 
should) specify them for clarity and error prevention.

{% highlight rust %}
let count: i32 = 10; // Explicit type annotation for clarity
{% endhighlight %}

## Error Messages: Rust’s Friendly Compiler

Rust’s compiler is known for its friendly and informative error messages. When your code doesn’t compile, Rust will 
often give suggestions or hints on how to fix it. For example, a typo in a variable name might prompt an error message 
with suggestions for the correct spelling.

{% highlight rust %}
fn main() {
    let x = 10;
    println!("Value of x: {}", y); 
}
{% endhighlight %}

The code above will have the compiler emitting messages like this:

{% highlight plain %}
-> src/main.rs:5:32
  |
5 |     println!("Value of x: {}", y); 
  |                                ^ help: a local variable with a similar name exists: `x`
{% endhighlight %}

Rust’s insistence on safe code often means dealing with the compiler more than in other languages. However, this leads 
to fewer runtime errors and safer, more reliable programs.

## Comments in Rust
Comments in Rust are straightforward and follow conventions you might know from other languages.

* Single-line comments use `//`.

{% highlight plain %}
// This is a single-line comment
{% endhighlight %}

* Multi-line comments use /* */.

{% highlight plain %}
/* This is a
   multi-line comment */
{% endhighlight %}

Rust also has documentation comments that generate HTML documentation for code, using `///` before functions or modules.

{% highlight plain %}
/// This function adds one to the input
fn add_one(x: i32) -> i32 {
    x + 1
}
{% endhighlight %}

# Data Types

Rust has a rich type system designed to prevent errors and ensure safety. Every variable in Rust has a type, either 
assigned explicitly or inferred by the compiler.

## Scalar Types

* **Integer Types**: `i8`, `i16`, `i32`, `i64`, `i128`, `isize` (signed); `u8`, `u16`, `u32`, `u64`, `u128`, `usize` (unsigned).

{% highlight rust %}
let x: i32 = -10; // 32-bit signed integer
let y: u8 = 255;  // 8-bit unsigned integer
{% endhighlight %}

* **Floating Point Types**: `f32` (single-precision), `f64` (double-precision).

{% highlight rust %}
let a: f64 = 3.1415;
let b: f32 = 2.5;
{% endhighlight %}

* **Boolean Type**: `bool`, which has two values, `true` and `false`.

{% highlight rust %}
let is_active: bool = true;
{% endhighlight %}

* **Character Type**: `char`, representing a single Unicode scalar value.

{% highlight rust %}
let letter: char = 'A';
let emoji: char = '😊';
{% endhighlight %}

## Compound Types

* **Tuples**: Group multiple values of potentially different types

{% highlight rust %}
let person: (&str, i32) = ("Alice", 30);
{% endhighlight %}

* **Arrays**: Fixed-size lists of values of a single type.

{% highlight rust %}
let numbers: [i32; 3] = [1, 2, 3];
{% endhighlight %}

# Constants and Static Variables

## Constants

Constants are immutable values defined with `const` and are global within the scope they’re declared in. Constants must 
have explicit types and are evaluated at compile time.

{% highlight rust %}
const PI: f64 = 3.14159;
{% endhighlight %}

## Static Variables

Static variables are similar to constants but have a fixed memory address. They can be mutable (with `static mut`), 
though this is unsafe.

{% highlight rust %}
static VERSION: &str = "1.0";
{% endhighlight %}

# Control Structures

Rust has similar control structures to C and C++, but with a few distinct Rust-specific behaviors and syntax nuances. 
Here’s a quick rundown:

* `if`: Works similarly to C/C++ but must have a boolean condition (no implicit integer-to-boolean conversions).
{% highlight rust %}
let number = 5;
if number > 0 {
    println!("Positive");
} else if number < 0 {
    println!("Negative");
} else {
    println!("Zero");
}
{% endhighlight %}

* `loop`: Rust's equivalent to `while(true)`. It’s an infinite loop but can return values using the `break` keyword.
{% highlight rust %}
let mut count = 0;
let result = loop {
    count += 1;
    if count == 10 {
        break count * 2;
    }
};
println!("Result: {}", result);
{% endhighlight %}

* `while`: Standard while loop as in C.
{% highlight rust %}
let mut x = 0;
while x < 5 {
    println!("x is: {}", x);
    x += 1;
}
{% endhighlight %}

* `for`: Rust’s `for` loop is typically used with ranges or iterators (no traditional C-style `for` loop).
{% highlight rust %}
for i in 0..5 {
    println!("i is: {}", i);
}
{% endhighlight %}

The `0..5` syntax creates a range from 0 to 4. You can also use `0..=5` for an inclusive range from 0 to 5.

# Pattern Matching

Rust’s match statement is a powerful control-flow construct that can deconstruct enums, arrays, and tuples.

## Using Match with Integers

{% highlight rust %}
let number = 7;

match number {
    1 => println!("One"),
    2 | 3 | 5 | 7 => println!("Prime"),
    _ => println!("Other"),
}
{% endhighlight %}

## Matching with Enums

Pattern matching is particularly useful with enums, as it enables exhaustive handling of each variant.

{% highlight rust %}
enum Color {
    Red,
    Green,
    Blue,
}

fn print_color(color: Color) {
    match color {
        Color::Red => println!("Red"),
        Color::Green => println!("Green"),
        Color::Blue => println!("Blue"),
    }
}
{% endhighlight %}

## Destructuring in Patterns

Rust allows destructuring in `match` expressions to work with complex data types.

{% highlight rust %}
let pair = (1, 2);

match pair {
    (0, _) => println!("First is zero"),
    (_, 0) => println!("Second is zero"),
    _ => println!("No zeros"),
}
{% endhighlight %}

# Functions and Closures

Functions and closures are both core components of Rust’s programming model.

## Functions

Functions are defined with `fn` and require explicit types for all parameters. Optionally, a function can return a 
value.

{% highlight rust %}
fn add(x: i32, y: i32) -> i32 {
    x + y
}
{% endhighlight %}

## Closures

Closures are anonymous functions that can capture their environment, and they are commonly used for iterators and 
callback functions.

{% highlight rust %}
let add = |a: i32, b: i32| a + b;
println!("Result: {}", add(5, 10));
{% endhighlight %}

Closures infer parameter and return types, but they can also be explicitly typed if needed.

# Summary

Rust’s syntax is familiar yet refined, with an expression-oriented structure that keeps code concise. Rust’s strict 
compiler catches potential issues early, helping you write robust code from the beginning. With these basics, you’ll be 
ready to dive deeper into Rust’s core features, like variables, mutability, and ownership.