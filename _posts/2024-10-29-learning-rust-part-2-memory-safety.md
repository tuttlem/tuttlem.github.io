---
layout: post
title: Learning Rust Part 2 - Memory Safety
date: 2024-10-29
comments: false
categories: [ "rust" ]
---

# Introduction

Rust's approach to memory safety is one of the language’s core features, allowing developers to write efficient, 
low-level code without a garbage collector. This post will dive into Rust’s memory safety model, explaining the 
ownership system, borrowing and lifetimes, garbage collection alternatives, and how Rust leverages RAII 
(Resource Acquisition Is Initialization) to ensure safe memory handling.

# Ownership Model

Rust’s **ownership model** is central to its memory safety guarantees. In Rust, every value has a unique owner, and 
when this owner goes out of scope, Rust automatically cleans up the associated memory. This system avoids many common 
bugs found in other languages, such as use-after-free and double-free errors.

## Key Rules of Ownership

* **Ownership**: Each value in Rust has a unique owner.
* **Move Semantics**: When an owner variable is assigned to another variable, the original owner loses access.

Here's a basic example that shows ownership transfer in Rust:

{% highlight rust %}
fn main() {
    let s1 = String::from("hello");
    let s2 = s1; // `s1` is moved to `s2`, `s1` is now invalid
    println!("{}", s2); // Valid
    // println!("{}", s1); // Error: `s1` is invalidated
}
{% endhighlight %}

By enforcing ownership rules, Rust guarantees memory safety without the need for a garbage collector.

# References and Borrowing Rules

Rust’s borrowing system works alongside ownership, allowing references to data so that functions can access values 
without taking ownership. Rust enforces strict rules to prevent data races, ensuring safe concurrent programming.

## Borrowing Rules

* **Borrowing**: Allows functions to temporarily access data without taking ownership.
* **Immutable Borrowing**: `&` allows read-only access, multiple are allowed simultaneously.
* **Mutable Borrowing**: `&mut` allows read-and-write access, but only one mutable reference can exist at a time.
* **References**: must always be valid.

Here’s how Rust handles immutable and mutable references:

{% highlight rust %}
fn main() {
    let mut data = String::from("hello");

    // Immutable borrow
    let r1 = &data;
    let r2 = &data;

    println!("r1: {}, r2: {}", r1, r2);

    // Mutable borrow
    let r3 = &mut data;
    r3.push_str(", world!");
    println!("r3: {}", r3);
}
{% endhighlight %}

The borrowing rules prevent data races by allowing either multiple immutable references or a single mutable reference, 
but never both simultaneously.

# Lifetimes and Scope

To further promote memory safety, Rust uses **lifetimes** to ensure that references do not outlive the data they point 
to, avoiding dangling references.

## Lifetime Annotations

Rust infers lifetimes in many cases, but explicit lifetime annotations are sometimes necessary, particularly in 
functions. Here’s an example:

{% highlight rust %}
fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() { x } else { y }
}
{% endhighlight %}

The `'a` annotation ensures that the returned reference will live as long as the input references, guaranteeing that the 
reference is valid.

## Lifetimes in Structs

Lifetimes are also useful in structs, helping ensure that struct members don’t outlive the data they refer to.

{% highlight rust %}
struct Important<'a> {
    text: &'a str,
}

fn main() {
    let message = String::from("Hello, world!");
    let important = Important { text: &message };
}
{% endhighlight %}

# Garbage Collection Alternatives

Rust’s ownership and borrowing rules function as a compile-time garbage collector, eliminating the need for runtime 
garbage collection. This model provides several benefits:

- **Predictable Performance**: No garbage collection pauses.
- **Lower Memory Overhead**: Efficient stack and heap memory usage.
- **Reduced Runtime Errors**: Compile-time checks prevent many common runtime crashes.

# Memory Leaks and Handling

While Rust’s ownership and borrowing rules prevent most memory leaks, they can still occur in cases involving reference 
cycles in data structures. For example, using `Rc` (Reference Counted) pointers can lead to memory leaks if cycles are 
not broken with `Weak` references.

Using `Weak` references prevents cyclic dependencies between nodes in complex data structures, such as trees or graphs.

{% highlight rust %}
use std::rc::{Rc, Weak};

struct Node {
    parent: Option<Weak<Node>>,
    children: Vec<Rc<Node>>,
}
{% endhighlight %}

In this example, `Weak` references are used to ensure that the parent node doesn’t keep a strong reference to its 
children, breaking any potential reference cycle.

# Drop Trait and RAII (Resource Acquisition Is Initialization)

Rust follows the **RAII** principle, where resources are automatically released when they go out of scope. The `Drop` 
trait allows developers to define custom clean-up behavior for resources, ensuring they’re properly managed.

## Implementing Drop

The `Drop` trait provides a `drop` method that runs automatically when an object is no longer needed.

{% highlight rust %}
struct Resource {
    name: String,
}

impl Drop for Resource {
    fn drop(&mut self) {
        println!("Releasing resource: {}", self.name);
    }
}

fn main() {
    let _res = Resource { name: String::from("file.txt") };
} // `_res` goes out of scope here, calling `drop`
{% endhighlight %}

## RAII in Rust

With RAII, resources like files and network connections are closed as soon as they’re no longer used. This minimizes the 
chance of resource leaks, and many standard library types in Rust implement `Drop` to handle resource 
deallocation automatically.

## Conclusion

Rust's approach to memory safety stands out for its compile-time checks, which enforce safe memory handling through 
ownership, borrowing, and lifetimes. By relying on these principles instead of a runtime garbage collector, Rust enables 
developers to write efficient, high-performance applications with minimal risk of memory-related errors. For developers 
looking to harness both power and safety, Rust offers a comprehensive memory management model that is well worth the 
investment.
