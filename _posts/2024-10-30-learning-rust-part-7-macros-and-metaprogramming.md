---
layout: post
title: Learning Rust Part 7 - Macros and Metaprogramming
date: 2024-10-30
comments: false
categories: [ "rust" ]
---

# Introduction

Rust’s macros offer powerful metaprogramming tools, enabling code generation, compile-time optimizations, and even 
domain-specific languages (DSLs). Unlike functions, macros operate at compile time, which makes them flexible and 
efficient but also requires careful usage. Rust’s macro system includes two primary types: declarative macros and 
procedural macros. In this post, we’ll explore both types and look at some practical examples.

# Declarative Macros (`macro_rules!`)

Declarative macros, created with `macro_rules!`, use pattern matching to expand code at compile time. These macros are 
ideal for handling repetitive code patterns and for defining custom DSLs.

## Defining a Declarative Macro

Here’s an example of a logging macro that can handle multiple arguments. The macro uses pattern matching to determine 
how to expand the code.

{% highlight rust %}
macro_rules! log {
    ($msg:expr) => {
        println!("[LOG]: {}", $msg);
    };
    ($fmt:expr, $($arg:tt)*) => {
        println!("[LOG]: {}", format!($fmt, $($arg)*));
    };
}

fn main() {
    log!("Starting application");
    log!("Hello, {}", "world");
}
{% endhighlight %}

This `log!` macro can be called with either a single expression or a format string with additional arguments.

## Repeaters (`$()*`)

Macros can use repeaters like `$(...)*` to handle variable numbers of arguments. Here’s a macro that generates a 
`Vec<String>` from a list of string literals:

{% highlight rust %}
macro_rules! vec_of_strings {
    ($($x:expr),*) => {
        vec![$(String::from($x)),*]
    };
}

fn main() {
    let v = vec_of_strings!["apple", "banana", "cherry"];
    println!("{:?}", v);
}
{% endhighlight %}

This macro makes it easy to create a vector of strings without repeating `String::from` for each element.

# Metavariables

In Rust macros, metavariables specify the kinds of expressions a macro can match. Here’s a look at the most commonly 
used types, with examples to help clarify each one.

## `expr`: Expressions

The `expr` metavariable type represents any valid Rust expression. This includes literals, function calls, arithmetic 
operations, and more.

{% highlight rust %}
macro_rules! log {
    ($msg:expr) => { 
        println!("[LOG]: {}", $msg); 
    };
}

fn main() {
    log!(42);                // 42 is a literal expression
    log!(5 + 3);             // 5 + 3 is an arithmetic expression
    log!("Hello, world!");   // "Hello, world!" is a string literal expression
}
{% endhighlight %}

## `tt`: Token Tree

The `tt` metavariable stands for **token tree** and is the most flexible type, accepting any valid Rust token or group 
of tokens. This includes literals, expressions, blocks, or even entire function bodies. tt is often used for parameters 
with variable length, as in `$($arg:tt)*`.

In the example,` ($($arg:tt)*)` allows the macro to accept a variable number of arguments, each matching the `tt` 
pattern.

{% highlight rust %}
macro_rules! log {
    ($fmt:expr, $($arg:tt)*) => {
        println!("[LOG]: {}", format!($fmt, $($arg)*));
    };
}

fn main() {
    log!("Hello, {}", "world");    // "Hello, {}" is matched as $fmt, "world" as $arg
    log!("Values: {} and {}", 1, 2); // Two arguments matched as $arg
}
{% endhighlight %}

In this case, `($fmt:expr, $($arg:tt)*)`:

* `$fmt:expr` matches a single format string.
* `$($arg:tt)*` matches a sequence of additional arguments, like `"world"` or `1`, `2`.

## Other Common Metavariable Types

Rust macros support additional metavariable types, each providing a different kind of flexibility. Here are some other 
commonly used types:

* `ident`: Matches an identifier (variable, function, or type name).
{% highlight rust %}
macro_rules! make_var {
    ($name:ident) => {
        let $name = 10;
    };
}

fn main() {
    make_var!(x); // Expands to: let x = 10;
    println!("{}", x);
}
{% endhighlight %}

* `ty`: Matches a type (like `i32` or `String`).
{% highlight rust %}
macro_rules! make_vec {
    ($type:ty) => {
        Vec::<$type>::new()
    };
}

fn main() {
    let v: Vec<i32> = make_vec!(i32); // Expands to Vec::<i32>::new()
}
{% endhighlight %}

* `pat`: Matches a pattern, often used in `match` arms.
{% highlight rust %}
macro_rules! match_num {
    ($num:pat) => {
        match $num {
            1 => println!("One"),
            _ => println!("Not one"),
        }
    };
}

fn main() {
    match_num!(1);
}
{% endhighlight %}

* `literal`: Matches literal values like numbers, characters, or strings. Useful when you need to capture only literal 
values.
{% highlight rust %}
macro_rules! print_literal {
    ($x:literal) => {
        println!("Literal: {}", $x);
    };
}

fn main() {
    print_literal!(42);       // Works, as 42 is a literal
    // print_literal!(5 + 5); // Error: 5 + 5 is not a literal
}
{% endhighlight %}

## Metavariable types

Here’s a quick reference of metavariable types commonly used in Rust macros:

| Metavariable | 	Matches                     | 	Example                      |
|--------------|------------------------------|-------------------------------|
| `expr`       | 	Any valid Rust expression   | 	`5 + 3`, `hello`, `foo()`    |
| `tt`         | 	Any token tree              | 	`1`, `{ 1 + 2 }`, `foo`, `bar` |
| `ident`      | 	Identifiers                 | 	`my_var`, `TypeName`         |
| `ty`         | 	Types                       | 	`i32`, `String`              |
| `pat`        | 	Patterns                    | 	`_`, `Some(x)`, `1..=10`     |
| `literal`    | 	Literals                    | 	`42`, `'a'`, `"text"`        |

# Procedural Macros

Procedural macros allow more advanced metaprogramming by directly manipulating Rust’s syntax. They operate on tokens 
(the syntactic elements of code) rather than strings, offering greater control over code generation. Procedural macros 
are defined as separate functions, usually in a dedicated crate.

## Types of Procedural Macros

Rust supports three main types of procedural macros:

- **Function-like macros**: Called like functions but with macro-level flexibility.
- **Attribute macros**: Add custom behavior to items like functions and structs.
- **Derive macros**: Automatically implement traits for structs or enums.

## Creating a Function-like Macro

A function-like macro uses the `proc_macro` crate to manipulate tokens directly. Here’s an example that generates a 
function called `hello` that prints a greeting:

{% highlight rust %}
use proc_macro;

#[proc_macro]
pub fn hello_macro(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input_str = input.to_string();
    format!("fn hello() {{ println!(\"Hello, {}!\"); }}", input_str).parse().unwrap()
}
{% endhighlight %}

This macro generates a `hello` function that prints a customized message. It would typically be used by adding 
`hello_macro!("Rust");` to the main code, and would output `Hello, Rust!`.

# Attribute Macros

Attribute macros attach custom attributes to items, making them useful for adding behaviors to functions, structs, or 
enums. For instance, an attribute macro can automatically log messages when entering and exiting a function.

{% highlight rust %}
use proc_macro::TokenStream;

#[proc_macro_attribute]
pub fn log(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = item.to_string();
    let output = format!(
        "fn main() {{
            println!(\"Entering function\");
            {}
            println!(\"Exiting function\");
        }}", input
    );
    output.parse().unwrap()
}
{% endhighlight %}

When applied to `main`, this macro logs messages before and after function execution, helping with function-level 
tracing.

# Derive Macros

Derive macros are a powerful feature in Rust, enabling automatic trait implementation for custom data types. Commonly 
used for traits like `Debug`, `Clone`, and `PartialEq`, derive macros simplify code by eliminating the need for manual 
trait implementation.

## Implementing a Derive Macro

Suppose we want to implement a custom `Hello` trait that prints a greeting. We can create a derive macro to 
automatically implement `Hello` for any struct annotated with `#[derive(Hello)]`.

First, define the `Hello` trait:

{% highlight rust %}
pub trait Hello {
    fn say_hello(&self);
}
{% endhighlight %}

Then, implement the derive macro in a procedural macro crate:

{% highlight rust %}
use proc_macro::TokenStream;
use quote::quote;

#[proc_macro_derive(Hello)]
pub fn hello_derive(input: TokenStream) -> TokenStream {
    let ast: syn::DeriveInput = syn::parse(input).unwrap();
    let name = &ast.ident;

    let gen = quote! {
        impl Hello for #name {
            fn say_hello(&self) {
                println!("Hello from {}", stringify!(#name));
            }
        }
    };
    gen.into()
}
{% endhighlight %}

Now, any struct tagged with `#[derive(Hello)]` will automatically implement the `Hello` trait, making the code more 
modular and concise.

# Domain-Specific Languages (DSLs) with Macros

Rust’s macros can be used to create DSLs, enabling specialized, readable syntax for specific tasks.

## Example: Creating a Simple DSL

Here’s an example of a DSL for building SQL-like queries. The `query!` macro translates the input syntax into a 
formatted SQL query string.

{% highlight rust %}
macro_rules! query {
    ($table:expr => $($col:expr),*) => {
        format!("SELECT {} FROM {}", stringify!($($col),*), $table)
    };
}

fn main() {
    let sql = query!("users" => "id", "name", "email");
    println!("{}", sql); // Outputs: SELECT id, name, email FROM users
}
{% endhighlight %}

This example uses `macro_rules!` to create a custom query builder, transforming macro input into SQL syntax in a natural 
format.

# Summary

Rust’s macros and metaprogramming features provide versatile tools for code generation, manipulation, and optimization. 
With declarative macros for straightforward pattern matching, procedural macros for syntax manipulation, and derive 
macros for auto-implementing traits, Rust enables developers to write efficient, flexible, and concise code. Macros can 
help create DSLs or extend functionality in powerful ways, making Rust an excellent choice for both performance and code 
expressiveness.