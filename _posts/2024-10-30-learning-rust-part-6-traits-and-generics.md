---
layout: post
title: Learning Rust Part 6 - Traits and Generics
date: 2024-10-30
comments: false
categories: [ "rust" ]
---

# Introduction

Rust’s traits and generics offer powerful tools for creating reusable, flexible, and type-safe code. Traits define 
shared behaviors, while generics allow code to work with multiple types. Combined, they make Rust’s type system 
expressive and robust, enabling high-performance applications with minimal redundancy. In this post, we’ll explore how 
traits and generics work in Rust and how they enhance code reusability.

# Trait Definition and Implementation

Traits in Rust are similar to interfaces in other languages, defining a set of methods that a type must implement. 
Traits allow different types to share behavior in a type-safe manner.

## Defining and Implementing Traits

To define a trait, use the `trait` keyword. Traits can include method signatures without implementations or with default 
implementations, which can be overridden by specific types.

{% highlight rust %}
trait Describe {
    fn describe(&self) -> String; // Required method

    fn greeting(&self) -> String { // Optional with a default implementation
        String::from("Hello!")
    }
}

struct Person {
    name: String,
}

impl Describe for Person {
    fn describe(&self) -> String {
        format!("My name is {}", self.name)
    }
}
{% endhighlight %}

In this example, the `Person` struct implements the `Describe` trait, providing a specific implementation for 
`describe`.

# Trait Objects (Dynamic Dispatch)

Rust supports dynamic dispatch with trait objects, allowing runtime polymorphism. This is useful when a function or 
collection must handle multiple types implementing the same trait.

## Using Trait Objects with `dyn`

Trait objects are created by specifying `dyn` before the trait name. The trait must be object-safe, meaning it doesn’t 
use generic parameters.

{% highlight rust %}
fn print_description(item: &dyn Describe) {
    println!("{}", item.describe());
}

let person = Person { name: String::from("Alice") };
print_description(&person); // Works with any type implementing `Describe`
{% endhighlight %}

In this example, `print_description` can accept any type that implements `Describe`, thanks to dynamic dispatch.

# Generics and Bounds

Generics in Rust allow writing code that can operate on different types. Generics are declared with angle brackets 
(`<T>`) and can be constrained with trait bounds to ensure they meet specific requirements.

## Defining Generics

Generics can be used in functions or structs, acting as placeholders for any type.

{% highlight rust %}
fn largest<T: PartialOrd>(a: T, b: T) -> T {
    if a > b { a } else { b }
}
{% endhighlight %}

## Trait Bounds

Trait bounds restrict a generic type to those implementing specific traits, enabling functions and structs to safely 
assume certain behaviors.

{% highlight rust %}
struct Container<T: Describe> {
    item: T,
}

impl<T: Describe> Container<T> {
    fn show(&self) {
        println!("{}", self.item.describe());
    }
}
{% endhighlight %}

In this example, the `Container` struct accepts only types implementing the `Describe` trait, ensuring `show` can safely 
call `describe`.

# Standard Traits 

Rust includes standard traits that add common behavior to types. Here are a few of them.

## `Clone`: Duplicate a Value

The `Clone` trait enables a type to be duplicated.

{% highlight rust %}
#[derive(Clone)]
struct Point { x: i32, y: i32 }

let p1 = Point { x: 5, y: 10 };
let p2 = p1.clone();
{% endhighlight %}

## `Copy`: Lightweight Copies for Simple Types

The `Copy` trait is used for types that can be copied by value, such as integers and simple structs.

{% highlight rust %}
#[derive(Copy, Clone)]
struct Point { x: i32, y: i32 }
{% endhighlight %}

## `Display` and `Debug`: Print-Friendly Output

* **Display**: Used to define how types are formatted in a user-friendly way, with `{}` in `println!`.
* **Debug**: Used for formatting types in a developer-friendly way, with `{:?}` in `println!`. It’s often used for logging and debugging.

You typically implement `Display` for custom types if they will be user-facing, while `Debug` is helpful for logging.

{% highlight rust %} 
use std::fmt;

struct Point { x: i32, y: i32 }

impl fmt::Display for Point { 
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "({}, {})", self.x, self.y) } 
}

impl fmt::Debug for Point { 
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "Point {% raw %}{{{% endraw %} x: {}, y: {} {% raw %}}}{% endraw %}", self.x, self.y) } 
}

let point = Point { x: 5, y: 10 }; 

println!("Display: {}", point); // Uses Display 
println!("Debug: {:?}", point); // Uses Debug
{% endhighlight %}

## `Iterator`: Sequentially Access Elements

The `Iterator` trait allows types to be iterated over in a sequence. Implementing `Iterator` requires defining the next 
method, which returns an `Option<T>`—either `Some(value)` for each item in the sequence or `None` to signal the end.

{% highlight rust %}
struct Counter { count: u32, }

impl Counter { 
    fn new() -> Self { Counter { count: 0 } } 
}

impl Iterator for Counter { 
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        self.count += 1;
        if self.count <= 5 {
            Some(self.count)
        } else {
            None
        }
    }
}

let mut counter = Counter::new(); 
while let Some(num) = counter.next() { 
    println!("{}", num); // Prints 1 through 5 
}
{% endhighlight %}

## `Into`: Converting to a Specified Type

The `Into` trait allows an instance of one type to be converted into another type. Implementing `Into` for a type 
enables conversions with `.into()`, making it easy to transform values between compatible types.

{% highlight rust %} 
struct Celsius(f64); 
struct Fahrenheit(f64);

impl Into<Fahrenheit> for Celsius { 
    fn into(self) -> Fahrenheit { Fahrenheit(self.0 * 1.8 + 32.0) } 
}

let temp_c = Celsius(30.0); 
let temp_f: Fahrenheit = temp_c.into(); // Converts Celsius to Fahrenheit 
{% endhighlight %}

## `From`: Converting from Another Type

The `From` trait is the counterpart to `Into`, providing a way to create an instance of a type from another type. Types 
that implement `From` for a specific type enable that conversion via `From::from`.

{% highlight rust %} 
struct Millimeters(u32);

impl From<u32> for Millimeters { 
    fn from(value: u32) -> Self { Millimeters(value) } 
}

let length = Millimeters::from(100); // Converts a u32 into Millimeters 
{% endhighlight %}

## `PartialEq` and `Eq`: Equality Comparison

The `PartialEq` trait enables types to be compared for equality with `==` and inequality with `!=`. Rust requires 
implementing `PartialEq` for custom types if you want to use them in conditional checks. `Eq` is a marker trait that 
indicates total equality, meaning the type has no partial or undefined cases (e.g., `NaN` for floats). Types that 
implement `Eq` also implement `PartialEq`.

{% highlight rust %} 
#[derive(PartialEq, Eq)] 
struct Point { x: i32, y: i32 }

let p1 = Point { x: 5, y: 10 }; 
let p2 = Point { x: 5, y: 10 }; 

assert_eq!(p1, p2); // Checks equality using == 
{% endhighlight %}

## `PartialOrd` and `Ord`: Ordering and Comparison

`PartialOrd` allows types to be compared for ordering with `<`, `>`, `<=`, and `>=`, while `Ord` requires that the 
ordering is total (e.g., every value is comparable). `Ord` is often used with types that have a logical sequence or 
order.

{% highlight rust %} 
#[derive(PartialOrd, PartialEq, Ord, Eq)] 
struct Temperature(i32);

let t1 = Temperature(30); 
let t2 = Temperature(40); 

assert!(t1 < t2); // Checks if t1 is less than t2 
{% endhighlight %}

## `Default`: Default Values

The `Default` trait provides a way to create a default instance of a type with `Default::default()`. This trait is 
particularly useful in generic programming when you want a type to have an initial state.

{% highlight rust %} 
#[derive(Default)] 
struct Config { debug: bool, timeout: u32, }

let config = Config::default(); // Initializes Config with default values 
{% endhighlight %}

## `Drop`: Custom Cleanup Logic

The `Drop` trait is called automatically when a value goes out of scope, making it ideal for managing resources, like 
closing files or network connections. `Drop` provides the drop method for custom cleanup logic.

{% highlight rust %} 
struct File { name: String, }

impl Drop for File { 
    fn drop(&mut self) { println!("Closing file: {}", self.name); } 
}

fn main() { 
    let f = File { name: String::from("data.txt") }; 
} 

// Drop is called here, and "Closing file: data.txt" is printed 

{% endhighlight %}

## `AsRef` and `AsMut`: Lightweight Borrowing

`AsRef` and `AsMut` enable types to convert themselves to references of another type, often used when you want to treat 
multiple types uniformly. They’re frequently used in APIs that need flexible input types.

{% highlight rust %} 
{% raw %}
fn print_length<T: AsRef<str>>(s: T) { 
    println!("Length: {}", s.as_ref().len()); 
}

print_length("hello"); // &str 
print_length(String::from("hello")); // String
{% endraw %}
{% endhighlight %}

## `Deref` and `DerefMut`: Custom Dereferencing

The `Deref` and `DerefMut` traits allow custom types to behave like references, enabling access to the inner data with 
the `*` operator. This is particularly useful for types like `Box`, which act as smart pointers to heap-allocated 
values.

{% highlight rust %} 
{% raw %}
use std::ops::Deref;

struct MyBox<T>(T);

impl<T> Deref for MyBox<T> { 
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

let x = MyBox(String::from("Hello")); 
println!("Deref: {}", *x); // Prints "Hello" due to Deref implementation
{% endraw %}
{% endhighlight %}

# Advanced Trait Bounds and Lifetimes

In more complex scenarios, multiple trait bounds and lifetimes help enforce requirements on generic types and 
references.

## Combining Multiple Trait Bounds

Multiple trait bounds can be combined with `+`, allowing a function to require several capabilities from a type.

{% highlight rust %}
{% raw %}
fn describe<T: Describe + Debug>(item: T) {
    println!("{:?}", item);
    println!("{}", item.describe());
}
{% endraw %}
{% endhighlight %}

## Lifetimes in Generics

When generics involve references, lifetimes ensure the references remain valid for the required scope. We went over 
lifetimes in [part 2]({% post_url 2024-10-29-learning-rust-part-2-memory-safety %}) of this series.

{% highlight rust %}
fn longest<'a, T>(x: &'a T, y: &'a T) -> &'a T
where
    T: PartialOrd,
{
    if x > y { x } else { y }
}
{% endhighlight %}

# Operator Overloading with Traits

Rust allows operator overloading for custom types through traits in the `std::ops` module. This enables intuitive syntax 
for user-defined types.

## Implementing `Add` for Custom `+` Behavior

The `Add` trait allows custom behavior for the `+` operator.

{% highlight rust %}
use std::ops::Add;

struct Point {
    x: i32,
    y: i32,
}

impl Add for Point {
    type Output = Point;

    fn add(self, other: Point) -> Point {
        Point { x: self.x + other.x, y: self.y + other.y }
    }
}

fn main() {
    let p1 = Point { x: 1, y: 2 };
    let p2 = Point { x: 3, y: 4 };
    let result = p1 + p2;
    println!("Result: ({}, {})", result.x, result.y);
}
{% endhighlight %}

This code allows adding two `Point` instances with the `+` operator, thanks to the `Add` trait implementation.

# Summary

Rust’s traits and generics enable developers to write flexible, reusable code while maintaining type safety. Traits 
define shared behavior, making it easy to build common functionality for different types, while generics allow for code 
that adapts to various types. The combination of traits, generics, and advanced features like dynamic dispatch and 
operator overloading make Rust’s type system both powerful and expressive, allowing you to build complex, maintainable 
applications with ease.
