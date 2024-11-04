---
layout: post
title: The Producer-Consumer Pattern
date: 2024-11-05
comments: false
categories: [ "rust" ]
---

# Introduction

Concurrency can feel tricky to manage, especially in a language like Rust where memory safety is strictly enforced. The 
Producer-Consumer pattern, however, provides a clean way to manage workloads between multiple threads, ensuring data is 
processed as it’s produced. In this post, we’ll explore this pattern by building a **prime number checker** in Rust. 
We’ll use a producer to generate candidate numbers and several consumers to check each candidate’s primality.

By the end of this post, you'll understand how to manage shared data between threads, use an atomic flag for graceful 
termination, and leverage Rust’s concurrency tools.

# The Producer-Consumer Pattern

In the Producer-Consumer pattern, one or more **producers** generate data and place it in a shared buffer. **Consumers** 
then take this data from the buffer and process it. This pattern is great for tasks that can be distributed, as it 
allows producers and consumers to run concurrently without overwhelming the system.

In our example, the **producer** is a main thread that generates numbers for prime-checking, while the **consumers** are 
threads that take these numbers and determine if they are prime. Here’s what we’ll build:

1. A producer (main thread) to generate candidate numbers.
2. A shared buffer where the candidates are stored.
3. Consumers (worker threads) that retrieve numbers from the buffer and check their primality.
4. An atomic stop flag to catch `SIGINT` (Ctrl+C) and cleanly stop the program.

# Code Overview

Let’s break down the code section by section.

## Setting Up Shared State 

First, we need to set up the shared data structures for our threads. Rust provides a few useful tools for this: `Arc`, 
`Mutex`, and `AtomicBool`.

* `Arc<T>` (Atomic Reference Counting) allows us to share ownership of data between threads.
* `Mutex<T>` protects shared data, ensuring only one thread can access it at a time.
* `AtomicBool` is a thread-safe boolean that can be modified atomically, which we’ll use to signal our threads when it’s time to stop.

{% highlight rust %}
use std::sync::{Arc, Mutex, atomic::{AtomicBool, Ordering}};
use std::thread;
use std::time::Duration;
use ctrlc;

fn main() {
    // small primes for checking divisibility
    let primes = vec![2, 3, 5, 7, 11, 13, 17]; 
    
    // shared buffer with Mutex and Arc
    let candidates_mutex = Arc::new(Mutex::new(vec![])); 
    
    // this is our kill switch
    let stop_flag = Arc::new(AtomicBool::new(false));
    
    // here's where we're currently up to in the candidate check
    let mut current = 20;
{% endhighlight %}

Here, `candidates_mutex` is an `Arc<Mutex<Vec<u32>>>` — an atomic reference-counted mutex around a vector of candidate 
numbers. By wrapping our vector with `Mutex`, we ensure that only one thread can modify the buffer at any time, 
preventing race conditions.

The `stop_flag` is an `AtomicBool`, which will allow us to signal when it’s time for all threads to stop processing. 
We’ll look at how to use this flag in the section on handling `SIGINT`.

## Stopping Gracefully

When running a multi-threaded application, it's essential to handle termination gracefully. Here, we’ll use the `ctrlc` 
crate to catch the `SIGINT` signal (triggered by `Ctrl+C`) and set `stop_flag` to `true` to signal all threads to stop.

{% highlight rust %}
    let stop_flag_clone = stop_flag.clone();
    ctrlc::set_handler(move || {
        stop_flag_clone.store(true, Ordering::SeqCst);
        println!("Received SIGINT, stopping . . . ");
    }).expect("Error setting Ctrl-C handler");
{% endhighlight %}

The `store` function sets the value of `stop_flag` to `true`, and we use `Ordering::SeqCst` to ensure all threads see 
the change immediately. This will stop all consumers from further processing once they check the flag.

## Creating the Consumer Threads

Now that we have a stop flag and a shared buffer, we can create our consumer threads. Each consumer thread will:

1. Check the `stop_flag`.
2. Attempt to retrieve a candidate number from `candidates_mutex`.
3. Check if the candidate is prime.

Here’s the code:

{% highlight rust %}
let handles: Vec<_> = (0..3)
    .map(|_| {
        // clone all of our concurrent structures for each thread
        let candidates_mutex = Arc::clone(&candidates_mutex);
        let primes = primes.clone();
        let stop_flag_clone = stop_flag.clone();

        thread::spawn(move || {
            // we make sure that we're still "running"
            while !stop_flag_clone.load(Ordering::SeqCst) {
                
                let candidate = {
                    // lock the mutex to get a candidate number
                    let mut candidates = candidates_mutex.lock().unwrap();
                    candidates.pop()
                };

                // check that a candidate was available
                if let Some(num) = candidate {
                    
                    // perform a primality check (basic division check for illustration)
                    let is_prime = primes.iter().all(|&p| num % p != 0 || num == p);

                    if is_prime {
                        println!("{} is prime", num);
                    } else {
                        println!("{} is not prime", num);
                    }

                } else {
                    // If no candidates are available, wait a moment before retrying
                    thread::sleep(Duration::from_millis(10));
                }
            }
        })
    })
    .collect();
{% endhighlight %}

### Explanation

* Each consumer thread runs a loop, checking the `stop_flag` using the `.load(Ordering::SeqCst)` function on `AtomicBool`. This function reads the current value of `stop_flag`, and with `Ordering::SeqCst`, we ensure that all threads see consistent updates.
* Inside the loop, the thread locks the `candidates_mutex` to safely access `candidates`.
* If a candidate is available, the thread checks its primality using modulus operations. If no candidate is available, it sleeps briefly before trying again, minimizing CPU usage.

### Why `.clone()`?

You'll notice at the start of the thread's execution, we setup a group of clones to work with.

If you’re new to Rust, you might wonder why we need to clone our `Arc` references when passing them to threads. In many 
languages, you can freely share references between threads without much consideration. Rust, however, has strict rules 
about data ownership and thread safety, which is why cloning `Arc`s becomes essential.

#### Rust’s Ownership Model and Shared Data

Rust enforces a strong ownership model to prevent data races, requiring that only one thread can "own" any piece of data 
at a time. This rule ensures that data cannot be modified simultaneously by multiple threads, which could lead to 
unpredictable behavior and bugs.

However, our prime-checker example needs multiple threads to access shared data (the list of candidates), which would 
normally violate Rust’s ownership rules. To make this possible, we use `Arc` and `Mutex`:

1. `Arc<T>`: Atomic Reference Counting for Shared Ownership 

`Arc` stands for _Atomic Reference Counted_, and it enables multiple threads to safely share ownership of the same data. 
Unlike a regular reference, `Arc` keeps a reference count, incremented each time you "clone" it. When the reference 
count drops to zero, Rust automatically deallocates the data.

Each clone of an Arc doesn’t copy the data itself; it only adds a new reference, allowing multiple threads to safely 
access the same data.

2. `Mutex<T>`: Ensuring Safe Access

Wrapping the shared data (in this case, our vector of candidates) in a `Mutex` allows threads to lock the data for 
exclusive access, preventing simultaneous modifications. The combination of `Arc` and `Mutex` gives us shared, safe, and 
controlled access to the data across threads.

#### Why Clone and Not Move?

You might wonder why we don’t simply "move" the Arc into each thread. Moving the Arc would transfer ownership to a 
single thread, leaving it inaccessible to other threads. Cloning allows us to create additional references to the same 
`Arc`, giving each thread access without compromising Rust’s ownership and thread-safety guarantees.

In essence, cloning an `Arc` doesn’t duplicate the data; it just creates another reference to it. This approach allows 
multiple threads to access shared data while still adhering to Rust’s safety guarantees.

By using `Arc` for shared ownership and `Mutex` for safe, exclusive access, we can implement the **Producer-Consumer** 
pattern in Rust without breaking any ownership or thread-safety rules.

## Producing Prime Candidates

Now, let’s look at the producer, which is responsible for generating numbers and adding them to the shared buffer. Here’s how it works:

{% highlight rust %}
// Main thread generating numbers
loop {
    // if we get the stop flag, we stop producing candidates
    if stop_flag.load(Ordering::SeqCst) {
        println!("Main thread stopping...");
        break;
    }

    {
        // acquire a lock on the candidates vector as we need to push some new candidates on
        let mut candidates = candidates_mutex.lock().unwrap();

        // 4 potential candidates per groups of 10
        candidates.push(current + 1);
        candidates.push(current + 3);
        candidates.push(current + 7);
        candidates.push(current + 9);

        println!("Added candidates, buffer size: {}", candidates.len());
    }

    // move on to the next group of 10
    current += 10;

    // slow down for illustration
    thread::sleep(Duration::from_millis(1)); 
}
{% endhighlight %}

The producer runs in a loop, checking `stop_flag` with `.load(Ordering::SeqCst)` to know when to stop. It then locks 
`candidates_mutex`, adds numbers to the buffer, and increments current to generate new candidates.

This part ties back to the **Producer-Consumer pattern**: while the producer keeps generating numbers, the consumers 
independently pull them from the shared buffer for prime-checking.

## Finishing up

Finally, we ensure a clean exit by calling `join` on each consumer thread. This function blocks until the thread 
completes, ensuring all threads finish gracefully.

{% highlight rust %}
    // Wait for all threads to finish
    for handle in handles {
        handle.join().unwrap();
    }

    println!("All threads exited. Goodbye!");
}
{% endhighlight %}

# Conclusion

This project is a great way to explore concurrency in Rust. By applying the **Producer-Consumer** pattern, we can 
efficiently manage workloads across threads while ensuring safety with `Arc`, `Mutex`, and `AtomicBool`.

Key takeaways:

* `AtomicBool` and `.load(Ordering::SeqCst)`: Allow threads to check for a termination signal in a consistent manner.
* `Mutex` and `Arc` for Shared Data: Ensures that multiple threads can safely read and write to the same data.
* **Producer-Consumer** Pattern: A practical way to distribute workloads and ensure efficient resource utilization.
* By catching `SIGINT`, we also made our program resilient to unexpected terminations, ensuring that threads exit cleanly.
