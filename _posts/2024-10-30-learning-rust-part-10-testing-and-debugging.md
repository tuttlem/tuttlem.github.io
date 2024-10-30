---
layout: post
title: Learning Rust Part 10 - Testing and Debugging
date: 2024-10-30
comments: false
categories: [ "rust" ]
---

# Introduction

Rust’s testing and debugging tools make it simple to verify code behavior, measure performance, and catch errors early. 
The `cargo test` command provides a built-in testing framework, while `println!` and `dbg!` help with debugging during 
development. This post explores Rust’s testing capabilities, including unit and integration tests, benchmarks, 
assertions, and effective debugging techniques.

# Unit Tests

Unit tests focus on verifying individual functions or components in isolation, ensuring each part of a program functions 
as expected. In Rust, unit tests are written inline in the same module as the code they test, using the `#[test]` 
attribute.

## Writing Unit Tests

To define a unit test, apply the `#[test]` attribute to a function. Rust’s built-in macros `assert!`, `assert_eq!`, 
and `assert_ne!` allow for assertions to confirm that the test’s outcome is correct.

{% highlight rust %}
fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        assert_eq!(add(2, 3), 5);
    }
}
{% endhighlight %}

## Running Tests

Run unit tests with `cargo test`. All functions marked with `#[test]` will execute, and results are displayed in the 
terminal.

{% highlight bash %}
cargo test
{% endhighlight %}

# Integration Tests

Integration tests verify the interactions between multiple modules, ensuring that different components of a codebase 
work as intended. Rust’s integration tests are placed in a `tests` directory at the project’s root.

## Creating an Integration Test

Each file in the `tests` directory acts as a separate integration test. These tests can access any public functions or 
types within the library crate.

{% highlight rust %}
// tests/integration_test.rs

use my_project::add;

#[test]
fn test_add() {
    assert_eq!(add(5, 5), 10);
}
{% endhighlight %}

# Benchmarks and Performance Testing

Rust’s `test` crate includes benchmarking features for performance testing, which can be run with `cargo bench` using 
Rust’s nightly version.

{% highlight bash %}
cargo +nightly bench
{% endhighlight %}

## Writing a Benchmark

The `test` crate allows you to measure the execution time of specific functions, helping identify areas for 
optimization.

{% highlight rust %}
#![feature(test)]
extern crate test;

use test::Bencher;

fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[bench]
fn bench_add(b: &mut Bencher) {
    b.iter(|| add(10, 20));
}
{% endhighlight %}

Run benchmarks using the nightly compiler with `cargo +nightly bench`, which provides performance insights into each 
function marked with `#[bench]`.

{% highlight bash %}
cargo +nightly bench
{% endhighlight %}

# Assertions and Custom Testing Utilities

Assertions are key to ensuring code correctness, verifying that conditions hold true. Rust provides several built-in 
macros for making assertions:

* `assert!`: Checks if a condition is true.
* `assert_eq!` and `assert_ne!`: Verify equality and inequality, displaying both values if the assertion fails.
* `assert!(condition, "message")`: Adds a custom message if the assertion fails.

{% highlight rust %}
#[test]
fn test_condition() {
    let value = 5;
    assert!(value > 2, "Value should be greater than 2");
    assert_eq!(value, 5, "Value should be equal to 5");
}
{% endhighlight %}

## Custom Assertion Functions

Custom assertion functions can make tests more readable and reusable.

{% highlight rust %}
fn is_even(n: i32) -> bool {
    n % 2 == 0
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_even(n: i32) {
        assert!(is_even(n), "{} is not even", n);
    }

    #[test]
    fn test_assert_even() {
        assert_even(4);
    }
}
{% endhighlight %}

# Documentation Testing

Rust’s documentation testing verifies examples in documentation comments to ensure they stay accurate as the code 
evolves. These tests are written in doc comments (`///`) and are run with `cargo test`.

## Writing Documentation Tests

Code examples can be embedded within doc comments using triple backticks (```). Rust will automatically test these 
examples.

{% highlight rust %}
/// Adds two numbers together.
///
/// # Examples
///
/// ```
/// let result = my_crate::add(2, 3);
/// assert_eq!(result, 5);
/// ```
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}
{% endhighlight %}

Documentation tests provide helpful examples for users and ensure the code continues to function as expected.

# Debugging with `println!` and `dbg!`

Rust’s `println!` macro is widely used to inspect values or track program flow during development. The `dbg!` macro, 
however, offers more context, displaying both the expression and its result along with file and line information.

## Using `println!` for Debugging

The `println!` macro outputs information to the console, allowing you to monitor variables and messages.

{% highlight rust %}
fn calculate(a: i32) -> i32 {
    println!("Calculating for a = {}", a);
    a * 2
}
{% endhighlight %}

## Using `dbg!` for Enhanced Debugging

The `dbg!` macro shows both the expression and its result, making it useful for evaluating complex expressions. 
`dbg!` outputs to standard error, keeping it separate from normal program output.

{% highlight rust %}
fn main() {
    let value = 10;
    let doubled = dbg!(value * 2); // Prints: [src/main.rs:4] value * 2 = 20
}
{% endhighlight %}

# Summary

Rust’s testing and debugging tools simplify the process of validating and refining code, from unit and integration tests 
to benchmarks and custom assertions. With `println!` and `dbg!` macros for debugging, plus documentation testing to keep 
examples up-to-date, Rust equips developers with the tools needed to build reliable, high-performance applications.