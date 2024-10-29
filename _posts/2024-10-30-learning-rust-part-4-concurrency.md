---
layout: post
title: Learning Rust Part 4 - Concurrency
date: 2024-10-30
comments: false
categories: [ "rust" ]
---

# Introduction

Rust’s concurrency model provides a unique approach to safe parallel programming by eliminating data races and 
encouraging structured, reliable concurrent code. Through its ownership model, concurrency primitives, and async/await 
syntax, Rust enables developers to write efficient, parallel programs. In this post, we’ll explore Rust’s key tools and 
patterns for safe concurrency.

# Threads and Thread Safety

Rust’s `std::thread` module allows developers to create threads, enabling programs to perform multiple tasks 
concurrently.

## Creating Threads

Rust threads are created with `std::thread::spawn`, and they can run independently of the main thread. The `join` 
method is used to wait for threads to complete.

{% highlight rust %}
use std::thread;

fn main() {
    let handle = thread::spawn(|| {
        for i in 1..5 {
            println!("Thread: {}", i);
        }
    });

    for i in 1..5 {
        println!("Main: {}", i);
    }

    handle.join().unwrap(); // Wait for the thread to finish
}
{% endhighlight %}

## Thread Safety

Rust’s ownership model ensures that data shared across threads is managed safely. Rust achieves this through two primary 
mechanisms:

* **Ownership Transfer**: Data can be transferred to threads, where the original owner relinquishes control.
* **Immutable Sharing**: If data is borrowed immutably, it can be accessed concurrently across threads without modification.

# Concurrency Primitives (`Mutex`, `RwLock`)

Rust offers concurrency primitives, such as `Mutex` and `RwLock`, to allow safe mutable data sharing across threads.

## Mutex (Mutual Exclusion)

A `Mutex` ensures that only one thread can access the data at a time. When using `lock()` on a `Mutex`, it returns a 
guard that releases the lock automatically when dropped.

{% highlight rust %}
use std::sync::{Mutex, Arc};
use std::thread;

fn main() {
    let data = Arc::new(Mutex::new(0));
    let mut handles = vec![];

    for _ in 0..10 {
        let data = Arc::clone(&data);
        let handle = thread::spawn(move || {
            let mut num = data.lock().unwrap();
            *num += 1;
        });
        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap();
    }

    println!("Result: {}", *data.lock().unwrap());
}
{% endhighlight %}

## `RwLock` (Read-Write Lock)

An `RwLock` allows multiple readers or a single writer, making it ideal for scenarios where data is read often but 
updated infrequently.

{% highlight rust %}
use std::sync::{RwLock, Arc};

fn main() {
    let data = Arc::new(RwLock::new(0));

    {
        let read_data = data.read().unwrap();
        println!("Read: {}", *read_data);
    }

    {
        let mut write_data = data.write().unwrap();
        *write_data += 1;
    }
}
{% endhighlight %}

## Atomic Types

Atomic types like `AtomicBool`, `AtomicIsize`, and `AtomicUsize` enable lock-free, atomic operations on shared data, 
which is useful for simple counters or flags.

{% highlight rust %}
use std::sync::atomic::{AtomicUsize, Ordering};
use std::thread;

fn main() {
    let counter = AtomicUsize::new(0);

    let handles: Vec<_> = (0..10).map(|_| {
        thread::spawn(|| {
            counter.fetch_add(1, Ordering::SeqCst);
        })
    }).collect();

    for handle in handles {
        handle.join().unwrap();
    }

    println!("Counter: {}", counter.load(Ordering::SeqCst));
}
{% endhighlight %}

# Channel Communication

Rust’s channels, provided by the `std::sync::mpsc` module, allow message passing between threads. Channels provide safe 
communication without shared memory, following a multiple-producer, single-consumer pattern.

## Creating and Using Channels

{% highlight rust %}
use std::sync::mpsc;
use std::thread;

fn main() {
    let (tx, rx) = mpsc::channel();

    thread::spawn(move || {
        let message = String::from("Hello from thread");
        tx.send(message).unwrap();
    });

    let received = rx.recv().unwrap();
    println!("Received: {}", received);
}
{% endhighlight %}

## Multi-Threaded Producers

To enable multiple threads to send messages to the same receiver, you can clone the transmitter.

{% highlight rust %}
let tx = mpsc::Sender::clone(&tx);
{% endhighlight %}

# Async/Await and Asynchronous Programming

Rust’s async/await syntax supports asynchronous programming, allowing tasks to pause (await) without blocking the 
entire thread. Async functions in Rust return `Future` types, which represent values available at a later time.

## Defining and Using Async Functions

An async function returns a `Future` and only runs when awaited.

{% highlight rust %}
async fn fetch_data() -> u32 {
    42
}

#[tokio::main]
async fn main() {
    let data = fetch_data().await;
    println!("Data: {}", data);
}
{% endhighlight %}

`.await` will force the application to wait for `fetch_data()` to complete before moving on.

## Combining Async Functions

Multiple async calls can be combined with `tokio::join!`, allowing concurrency without additional threads.

{% highlight rust %}
async fn first() -> u32 { 10 }
async fn second() -> u32 { 20 }

async fn run() {
    let (a, b) = tokio::join!(first(), second());
    println!("Sum: {}", a + b);
}
{% endhighlight %}

# Task-Based Concurrency with Tokio and async-std

Rust offers runtime libraries like Tokio and async-std for task-based concurrency, providing asynchronous runtimes 
suited for managing complex async workflows.

## Using Tokio

Tokio is a popular async runtime, offering tools for task management, timers, and network I/O.

{% highlight rust %}
use tokio::time::{sleep, Duration};

#[tokio::main]
async fn main() {
    let handle = tokio::spawn(async {
        sleep(Duration::from_secs(1)).await;
        println!("Task completed!");
    });

    handle.await.unwrap();
}
{% endhighlight %}

### async-std Example

async-std offers similar functionality with a simpler API for certain tasks.

{% highlight rust %}
use async_std::task;
use std::time::Duration;

fn main() {
    task::block_on(async {
        task::sleep(Duration::from_secs(1)).await;
        println!("Task completed!");
    });
}
{% endhighlight %}

## Summary

Rust’s concurrency model provides robust tools for safe multithreading and asynchronous programming. By combining 
threads, async/await syntax, and concurrency primitives like `Mutex` and `RwLock`, Rust enables safe data sharing and 
task-based concurrency, making it a powerful choice for high-performance concurrent applications.