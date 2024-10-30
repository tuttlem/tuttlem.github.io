---
layout: post
title: Learning Rust Part 5 - Data Structures
date: 2024-10-30
comments: false
categories: [ "rust" ]
---

# Introduction

Rust offers a versatile range of data structures that make working with various types of data efficient and safe. In 
this post, we’ll cover fundamental data structures like vectors, hash maps, and arrays, along with Rust’s powerful enums 
and structs for creating custom data types.

# Text

## Strings and String Slices

Rust provides two main string types:

* **String**: A growable, heap-allocated string.
* **&str (string slice)**: An immutable view into a string, commonly used for read-only access.

String example:

{% highlight rust %}
let mut s = String::from("Hello");
s.push_str(", world!");
println!("{}", s);
{% endhighlight %}

String slice example:

{% highlight rust %}
let greeting = "Hello, world!";
let slice = &greeting[0..5]; // "Hello"
{% endhighlight %}

# Collections

## `Vectors`

Vectors (`Vec<T>`) are dynamic arrays that can grow or shrink in size, making them ideal for storing sequences of values 
when the size isn’t known at compile-time.

{% highlight rust %}
fn main() {
    let mut numbers = vec![1, 2, 3];
    numbers.push(4); // Add an element
    println!("{:?}", numbers);
}
{% endhighlight %}

## `LinkedList`

A `LinkedList<T>` is a doubly linked list that allows fast insertion and deletion of elements at both ends of the list. 
It is less commonly used than `Vec` but can be useful when you need to insert and remove elements frequently from both 
the front and back of the collection.

{% highlight rust %} 
use std::collections::LinkedList;

fn main() { 
    let mut list = LinkedList::new(); 

    list.push_back(1); 
    list.push_back(2); 
    list.push_front(0);

    for value in &list {
        println!("{}", value); // Prints 0, 1, 2
    }
} 
{% endhighlight %}

## `HashMap`

A `HashMap<K, V>` stores key-value pairs, enabling efficient value retrieval based on keys.

{% highlight rust %}
use std::collections::HashMap;

fn main() {
    let mut scores = HashMap::new();
    scores.insert("Alice", 10);
    scores.insert("Bob", 20);

    println!("{:?}", scores.get("Alice")); // Some(&10)
}
{% endhighlight %}

## `BTreeMap`

`BTreeMap<K, V>` is similar to `HashMap` but keeps keys sorted, making it useful for scenarios where sorted keys are 
necessary.

{% highlight rust %}
use std::collections::BTreeMap;

fn main() {
    let mut scores = BTreeMap::new();
    scores.insert("Alice", 10);
    scores.insert("Bob", 20);

    for (key, value) in &scores {
        println!("{}: {}", key, value);
    }
}
{% endhighlight %}

## `BinaryHeap`

A `BinaryHeap<T>` is a priority queue implemented with a binary heap, where elements are always ordered so the largest 
(or smallest) element can be accessed quickly. By default, `BinaryHeap` maintains a max-heap, but it can be customized 
for min-heap operations.

{% highlight rust %} 
use std::collections::BinaryHeap;

fn main() { 
    let mut heap = BinaryHeap::new(); 

    heap.push(1); 
    heap.push(5); 
    heap.push(3);

    while let Some(top) = heap.pop() {
        println!("{}", top); // Prints values in descending order: 5, 3, 1
    }
}
{% endhighlight %}

## `HashSet`

A `HashSet<T>` is a collection of unique values, implemented as a hash table. It provides fast membership checking and 
is useful when you need to store non-duplicate items without any specific order.

{% highlight rust %} 
use std::collections::HashSet;

fn main() { 
    let mut set = HashSet::new(); 

    set.insert("apple"); 
    set.insert("banana"); 
    set.insert("apple"); // Duplicate, ignored by the set

    println!("{:?}", set.contains("banana")); // true
    println!("{:?}", set); // {"apple", "banana"}
}
{% endhighlight %}

## `BTreeSet`

A `BTreeSet<T>` is a sorted, balanced binary tree-based set. Like `HashSet`, it only stores unique values, but unlike 
`HashSet`, it maintains its items in sorted order. This makes it suitable for range queries and ordered data.

{% highlight rust %} 
use std::collections::BTreeSet;

fn main() { 
    let mut set = BTreeSet::new(); 

    set.insert(10); 
    set.insert(20); 
    set.insert(15);

    for value in &set {
        println!("{}", value); // Prints 10, 15, 20 in sorted order
    }
}
{% endhighlight %}

## `VecDeque`

A `VecDeque<T>` (double-ended queue) is a resizable, efficient data structure that supports adding and removing elements 
from both the front and back. It’s ideal for queue-like operations where both ends need to be accessible.

{% highlight rust %} 
use std::collections::VecDeque;

fn main() { 
    let mut deque = VecDeque::new(); 

    deque.push_back(1); 
    deque.push_front(0);

    println!("{:?}", deque.pop_back()); // Some(1)
    println!("{:?}", deque.pop_front()); // Some(0)

} 
{% endhighlight %}

# `Option` and `Result` Types

Rust’s `Option` and `Result` types are enums that enable safe handling of optional values and errors. We went over 
error handling in [part 3]({% post_url 2024-10-29-learning-rust-part-3-error-handling %}) of this series.

## `Option`

The `Option<T>` type represents an optional value: `Some(T)` for a value, or `None` if absent.

{% highlight rust %}
fn find_element(index: usize) -> Option<i32> {
    let numbers = vec![1, 2, 3];
    numbers.get(index).copied()
}

fn main() {
    match find_element(1) {
        Some(number) => println!("Found: {}", number),
        None => println!("Not found"),
    }
}
{% endhighlight %}

## `Result`

The `Result<T, E>` type is used for functions that may succeed (`Ok(T)`) or fail (`Err(E)`), promoting explicit error 
handling.

{% highlight rust %}
fn divide(a: f64, b: f64) -> Result<f64, &'static str> {
    if b == 0.0 {
        Err("Cannot divide by zero")
    } else {
        Ok(a / b)
    }
}
{% endhighlight %}

# Custom Data Types and Enums

Rust’s enums and structs allow you to create custom data types, essential for building complex and expressive programs.

## Enums

Rust’s enums can hold different types of data within each variant, enabling versatile data representations.

{% highlight rust %}
enum Message {
    Text(String),
    Image { url: String, width: u32, height: u32 },
    Quit,
}

fn main() {
    let msg = Message::Text(String::from("Hello"));
    match msg {
        Message::Text(text) => println!("Text: {}", text),
        Message::Image { url, width, height } => println!("Image at {}, size: {}x{}", url, width, height),
        Message::Quit => println!("Quit message"),
    }
}
{% endhighlight %}

## Structs and Tuple Structs

Structs allow for creating complex types with named fields.

{% highlight rust %}
struct Person {
    name: String,
    age: u8,
}

fn main() {
    let person = Person {
        name: String::from("Alice"),
        age: 30,
    };
    println!("Name: {}, Age: {}", person.name, person.age);
}
{% endhighlight %}

Tuple structs are useful for grouping values without naming fields, often for simpler data types.

{% highlight rust %}
struct Color(u8, u8, u8);

fn main() {
    let red = Color(255, 0, 0);
    println!("Red: {}, {}, {}", red.0, red.1, red.2);
}
{% endhighlight %}

# Arrays, Slices, and Compile-Time Length Arrays

## Arrays

Arrays in Rust are fixed-size collections of elements with known length at compile-time. They’re stack-allocated, 
offering efficiency and safety.

{% highlight rust %}
fn main() {
    let numbers: [i32; 3] = [1, 2, 3];
    println!("First element: {}", numbers[0]);
}
{% endhighlight %}

## Slices

Slices provide a way to view sections of an array or vector, avoiding the need to copy data.

{% highlight rust %}
fn main() {
    let numbers = [1, 2, 3, 4];
    let slice = &numbers[1..3];
    println!("{:?}", slice); // [2, 3]
}
{% endhighlight %}

Slices work with both arrays and vectors and are typically used as function parameters to avoid copying large data 
structures.

# Reference Counting

## `Rc<Vec<T>>` and `Arc<Vec<T>>`

 `Rc` (Reference Counting) and `Arc` (Atomic Reference Counting) are common wrappers around collections like `Vec` to 
 allow multiple ownership. `Rc` is single-threaded, while `Arc` is thread-safe, and both are used frequently for sharing 
 collections between parts of a program.

{% highlight rust %} 
use std::rc::Rc; 
use std::sync::Arc;

fn main() { 
    let vec = vec![1, 2, 3]; 
    let shared_rc = Rc::new(vec.clone()); 
    let shared_arc = Arc::new(vec);

    println!("Rc count: {}", Rc::strong_count(&shared_rc)); // Rc count: 1
    println!("Arc count: {}", Arc::strong_count(&shared_arc)); // Arc count: 1

} 
{% endhighlight %}

# Summary

Rust’s data structures—from collections like `Vec` and `HashMap` to custom types with `struct` and `enum`—enable 
flexible, efficient, and safe data handling. With tools like `Option` and `Result`, Rust enforces a safety-first 
approach without compromising on performance, making it an ideal language for robust application development.
