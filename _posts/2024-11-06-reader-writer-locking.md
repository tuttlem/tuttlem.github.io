---
layout: post
title: Reader Writer Locking
date: 2024-11-06
comments: false
categories: [ "rust" ]
---

# Introduction

The Reader-Writer problem is a classic synchronization problem that explores how multiple threads access shared 
resources when some only need to read the data, while others need to write (or modify) it.

In this problem:

* **Readers** can access the resource simultaneously, as they only need to view the data.
* **Writers** require exclusive access because they modify the data, and having multiple writers or a writer and a reader simultaneously could lead to data inconsistencies.

In Rust, this problem is a great way to explore `RwLock` (read-write lock), which allows us to grant multiple readers 
access to the data but restricts it to a single writer at a time.

# Implementing

Here's a step-by-step guide to implementing a simple version of this problem in Rust.

1. **Set up a shared resource**: We’ll use an integer counter that both readers and writers will access.
2. **Create multiple readers and writers**: Readers will print the current value, while writers will increment the value.
3. **Synchronize access**: Using `RwLock`, we’ll ensure readers can access the counter simultaneously but block writers when they’re active.

## Setting Up Shared State

To manage shared access to the counter, we use `Arc<RwLock<T>>`. `Arc` allows multiple threads to own the same data, and 
`RwLock` ensures that we can have either multiple readers or a single writer at any time.

Here’s the initial setup:

{% highlight rust %}
use std::sync::{Arc, RwLock};
use std::thread;
use std::time::Duration;

fn main() {
    // Shared counter, initially 0, wrapped in RwLock and Arc for thread-safe access
    let counter = Arc::new(RwLock::new(0));

    // Vector to hold all reader and writer threads
    let mut handles = vec![];
{% endhighlight %}

## Creating Reader Threads

Readers will read the counter’s value and print it. Since they only need to view the data, they’ll acquire a read lock 
on the `RwLock`.

Here’s how a reader thread might look:

{% highlight rust %}
    // create 5 reader threads
    for i in 0..5 {
        let counter = Arc::clone(&counter);
        let handle = thread::spawn(move || {
            loop {
                // acquire a read lock
                let read_lock = counter.read().unwrap();
                
                println!("Reader {} sees counter: {}", i, *read_lock);
                
                // simulate work
                thread::sleep(Duration::from_millis(100)); 
            }
        });
        handles.push(handle);
    }
{% endhighlight %}

Each reader:

* Clones the `Arc` so it has its own reference to the shared counter.
* Acquires a read lock with `counter.read()`, which allows multiple readers to access it simultaneously.
* Prints the counter value and then waits briefly, simulating reading work.

## Creating Writer Threads

Writers need exclusive access, as they modify the data. Only one writer can have a write lock on the RwLock at a time.

Here’s how we set up a writer thread:

{% highlight rust %}
    // create 2 writer threads
    for i in 0..2 {
        let counter = Arc::clone(&counter);
        let handle = thread::spawn(move || {
            loop {
                // acquire a write lock
                let mut write_lock = counter.write().unwrap();
                *write_lock += 1;
                println!("Writer {} increments counter to: {}", i, *write_lock);
                thread::sleep(Duration::from_millis(150)); // Simulate work
            }
        });
        handles.push(handle);
    }
{% endhighlight %}

Each writer:

* Clones the `Arc` to access the shared counter.
* Acquires a write lock with `counter.write()`. When a writer holds this lock, no other readers or writers can access the data.
* Increments the counter and waits, simulating writing work.

## Joining the Threads

Finally, we join the threads so the main program waits for all threads to finish. Since our loops are infinite for 
demonstration purposes, you might add a termination condition or handle to stop the threads gracefully.

{% highlight rust %}
    // wait for all threads to finish (they won't in this infinite example)
    for handle in handles {
        handle.join().unwrap();
    }
}
{% endhighlight %}

# Complete Code

Here's the complete code breakdown for this problem:

{% highlight rust %}
use std::sync::{Arc, RwLock};
use std::thread;
use std::time::Duration;

fn main() {
    let counter = Arc::new(RwLock::new(0));
    let mut handles = vec![];

    // Create reader threads
    for i in 0..5 {
        let counter = Arc::clone(&counter);
        let handle = thread::spawn(move || {
            loop {
                let read_lock = counter.read().unwrap();
                println!("Reader {} sees counter: {}", i, *read_lock);
                thread::sleep(Duration::from_millis(100));
            }
        });
        handles.push(handle);
    }

    // Create writer threads
    for i in 0..2 {
        let counter = Arc::clone(&counter);
        let handle = thread::spawn(move || {
            loop {
                let mut write_lock = counter.write().unwrap();
                *write_lock += 1;
                println!("Writer {} increments counter to: {}", i, *write_lock);
                thread::sleep(Duration::from_millis(150));
            }
        });
        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap();
    }
}
{% endhighlight %}

## Key Components

* `Arc<RwLock<T>>`: `Arc` provides shared ownership, and `RwLock` provides a mechanism for either multiple readers or a single writer.
* `counter.read()` and `counter.write()`: `RwLock`'s `.read()` grants a shared read lock, and `.write()` grants an exclusive write lock. While the write lock is held, no other threads can acquire a read or write lock.
* **Concurrency Pattern**: This setup ensures that multiple readers can operate simultaneously without blocking each other. However, when a writer needs access, it waits until all readers finish, and once it starts, it blocks other readers and writers.

# Conclusion

The Reader-Writer problem is an excellent way to understand Rust’s concurrency features, especially RwLock. By 
structuring access in this way, we allow multiple readers or a single writer, which models real-world scenarios like 
database systems where reads are frequent but writes require careful, exclusive access.