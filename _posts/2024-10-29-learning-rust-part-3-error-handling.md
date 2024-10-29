---
layout: post
title: Learning Rust Part 3 - Error Handling
date: 2024-10-29
comments: false
categories: [ "rust" ]
---

# Introduction

Rust’s error handling model focuses on safety and reliability, providing structured patterns that allow developers to 
manage recoverable and unrecoverable errors without exceptions. This post explains Rust's key error-handling tools, 
including `Result` and `Option` types, the `?` operator, and custom error types.

# `Result` and `Option` Types

In Rust, the `Result` and `Option` types help manage possible errors at compile time, providing clear patterns for 
handling expected and unexpected outcomes.

* `Result<T, E>`: Used for functions that may succeed or fail. The `Result` type holds two variants: `Ok(T)` for success and `Err(E)` for error.
    {% highlight rust %}
    fn divide(a: f64, b: f64) -> Result<f64, String> {
        if b == 0.0 {
            Err(String::from("Cannot divide by zero"))
        } else {
            Ok(a / b)
        }
    }
    {% endhighlight %}

* `Option<T>`: Indicates the possibility of a missing value. It has two variants: `Some(T)` for a value and `None` for absence.
    {% highlight rust %}
    fn get_item(index: usize) -> Option<&'static str> {
        let items = vec!["apple", "banana", "cherry"];
        items.get(index)
    }
    {% endhighlight %}

# Unwrapping and Safe Patterns

While `unwrap` can retrieve a value from `Result` or `Option`, it will panic if the value is `Err` or `None`. Safer 
handling patterns are preferred for production code to avoid panics.

## Using `match` with `Result`

Using `match` allows us to handle both the success and error cases.

{% highlight rust %}
match divide(10.0, 0.0) {
    Ok(value) => println!("Result: {}", value),
    Err(e) => println!("Error: {}", e),
}
{% endhighlight %}

## Using `if let` with `Option`

With `if let`, we can easily check for the presence of a value.

{% highlight rust %}
if let Some(item) = get_item(1) {
    println!("Found item: {}", item);
} else {
    println!("Item not found");
}
{% endhighlight %}

## Providing Default Values with `unwrap_or`

The `unwrap_or` and `unwrap_or_else` methods allow a fallback value for `Err` or `None`.

{% highlight rust %}
let value = divide(10.0, 0.0).unwrap_or(0.0);
{% endhighlight %}

# Error Propagation with the `?` Operator

Rust’s `?` operator simplifies error propagation in functions that return `Result` or `Option`. If an error occurs, `?` 
will return it immediately to the caller, enabling cleaner code with fewer explicit `match` or `unwrap` blocks.

{% highlight rust %}
fn calculate(a: f64, b: f64) -> Result<f64, String> {
    let result = divide(a, b)?; // Error is propagated if `divide` returns `Err`
    Ok(result + 10.0)
}
{% endhighlight %}

## Rules for Using `?`

The `?` operator is only available in functions that return `Result` or `Option`. If an error occurs, it will be 
converted into the return type of the function, allowing for elegant chaining of potentially failing operations.

# Panic and Recoverable Errors

Rust differentiates between **recoverable** errors (handled with `Result` or `Option`) and **unrecoverable** errors 
(handled with `panic!`). While `panic!` stops execution in the case of a critical error, Rust recommends using it 
sparingly.

## Using `panic!` Wisely

The `panic!` macro is best reserved for unrecoverable errors that require the program to halt, whereas most errors 
should be handled with `Result` or `Option`.

{% highlight rust %}
fn risky_function() {
    panic!("An unrecoverable error occurred");
}
{% endhighlight %}

# Custom Error Types

For complex applications, custom error types allow fine-grained error handling and more expressive error messages. 
Custom error types in Rust are usually implemented with the `std::fmt::Display` and `std::error::Error` traits.

## Defining a Custom Error Type

Creating a custom error type can help differentiate between various error scenarios in a Rust application.

{% highlight rust %}
use std::fmt;

#[derive(Debug)]
enum MathError {
    DivisionByZero,
    NegativeRoot,
}

impl fmt::Display for MathError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MathError::DivisionByZero => write!(f, "Cannot divide by zero"),
            MathError::NegativeRoot => write!(f, "Cannot compute the square root of a negative number"),
        }
    }
}

fn divide(a: f64, b: f64) -> Result<f64, MathError> {
    if b == 0.0 {
        Err(MathError::DivisionByZero)
    } else {
        Ok(a / b)
    }
}
{% endhighlight %}

Custom error types support the `?` operator, allowing more readable and maintainable error handling across complex 
codebases.

# Logging and Debugging Techniques

Logging is crucial for tracking and debugging errors. Rust provides multiple logging options:

## `println!` for Basic Logging

Simple logging with `println!` is useful for quick debugging.

{% highlight rust %}
println!("This is a basic log message");
{% endhighlight %}

## Using the `log` Crate

For more structured logging, the `log` crate provides multi-level logging capabilities and works with backends like 
`env_logger` or `log4rs`.

{% highlight rust %}
use log::{info, warn, error};

fn main() {
    info!("Starting application");
    warn!("This is a warning");
    error!("This is an error message");
}
{% endhighlight %}

## Debugging with `dbg!`

The `dbg!` macro prints a debug message with the file and line number, ideal for inspecting variable values.

{% highlight rust %}
let x = 5;
dbg!(x * 2); // Outputs: [src/main.rs:4] x * 2 = 10
{% endhighlight %}

## Additional Debugging Tools

- **Compiler Error Messages**: Rust’s detailed compiler errors help identify issues early.
- **Cargo Check**: Quickly identifies syntax errors without a full compile using `cargo check`.
- **Cargo Test**: Run `cargo test` to validate the application and capture edge cases.

# Conclusion

Rust’s error handling model promotes safe, reliable code by providing structured tools like `Result`, `Option`, and the 
`?` operator for managing recoverable and unrecoverable errors. With custom error types and logging options, Rust 
empowers developers to write robust, maintainable applications. By enforcing careful error handling, Rust encourages a 
proactive approach to managing failures, making it ideal for building reliable systems.
