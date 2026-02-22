---
layout: post
title: crossbeam
date: 2026-02-23
comments: false
categories: [ rust crates concurrency crossbeam ]
---

Rust gives you threads in the standard library. It also gives you `std::sync::mpsc`. For simple programs, that might 
be enough.

But once you start writing serious concurrent code, you quickly run into limitations:

- `std::sync::mpsc` is single-producer (despite the name).
- Scoped threads are awkward.
- Lock-free structures are not in std.
- Performance characteristics are conservative.

`crossbeam` fills that gap.

It provides fast, well-designed concurrency primitives without inventing a runtime.

No async.  
No executors.  
Just threads done properly.

## What Problem Does crossbeam Solve?

Two major ones:

1. Better channels.
2. Safe scoped threads.

Plus a toolbox of lock-free and memory-ordering utilities if you need them.

Unlike Tokio, Crossbeam is about native threads — not async tasks.

It embraces OS threads, but makes them less painful.

## Minimal Example: Multi-Producer, Multi-Consumer Channel

The standard library’s channel has limitations.

Crossbeam’s channel is:

- Multi-producer
- Multi-consumer
- Fast
- Flexible (bounded or unbounded)

### Cargo.toml

{% highlight toml %}
[package]
name = "crossbeam_demo"
version = "0.1.0"
edition = "2021"

[dependencies]
crossbeam = "0.8"
{% endhighlight %}

### main.rs

{% highlight rust %}
use crossbeam::channel;
use std::thread;
use std::time::Duration;

fn main() {
    let (tx, rx) = channel::unbounded();

    for i in 0..4 {
        let tx = tx.clone();
        thread::spawn(move || {
            tx.send(format!("worker {i} done")).unwrap();
        });
    }

    drop(tx); // close the channel

    for msg in rx.iter() {
        println!("{msg}");
    }
}
{% endhighlight %}

All workers send into the same channel.

Multiple producers.
Single consumer here — but you could clone `rx` too.

That’s already more flexible than `std::sync::mpsc`.

## Bounded Channels (Backpressure)

Unbounded channels are convenient — but sometimes dangerous.

Crossbeam supports bounded channels:

{% highlight rust %}
let (tx, rx) = channel::bounded(2);
{% endhighlight %}

If the channel fills up, `send()` blocks until space is available.

That gives you real backpressure.

This matters in systems code.  
It prevents silent memory growth.

## What’s Actually Happening?

Crossbeam channels are built for high performance and low contention.

Key design traits:

- MPMC (multi-producer, multi-consumer)
- Blocking and non-blocking operations
- `select!` support
- Efficient wakeups

You also get:

- `try_send`
- `try_recv`
- timeouts
- `select!` across multiple channels

Example:

{% highlight rust %}
use crossbeam::select;

select! {
    recv(rx) -> msg => println!("got {msg:?}"),
    default => println!("no message available"),
}
{% endhighlight %}

This gives you multiplexing without async.

## Scoped Threads (The Underrated Feature)

This is the feature most people overlook.

Rust’s `std::thread::spawn` requires `'static` lifetimes.

That forces you to clone or move data into threads.

Crossbeam’s `scope` lets threads borrow from the stack safely.

{% highlight rust %}
use crossbeam::thread;

fn main() {
    let data = vec![1, 2, 3];

    thread::scope(|s| {
        s.spawn(|_| {
            println!("first element: {}", data[0]);
        });
    }).unwrap();
}
{% endhighlight %}

The compiler guarantees the threads finish before the scope exits.

This eliminates a huge amount of lifetime friction.

In systems code, this is extremely useful.

## Where crossbeam Fits

Crossbeam is ideal when:

- You want native threads.
- You need fast channels.
- You care about memory ordering.
- You are building concurrent data structures.
- You don’t want an async runtime.

It’s particularly useful in:

- CPU-bound workloads
- Pipelines
- Parallel algorithms
- Systems utilities

If Tokio is “async orchestration”, Crossbeam is “disciplined threaded concurrency”.

## Where It Does Not Fit

Crossbeam does not replace async runtimes.

If you’re doing high-scale network I/O, async usually scales better.

Crossbeam also won’t magically solve design problems.

You still need to think about:

- Ownership
- Contention
- Deadlocks
- Memory visibility

It just gives you better tools.

## Trade-offs

### Pros
- High-performance channels
- MPMC support
- Scoped threads remove `'static` pain
- No runtime required

### Cons
- Still manual threading model
- Easier to shoot yourself in the foot than async
- Requires stronger concurrency discipline

## Should You Use It?

If you are building CPU-bound concurrent systems or pipelines:

Yes.

If you need structured async I/O:

Probably not — Tokio is a better fit.  Crossbeam is not flashy.

It doesn’t come with a runtime banner. It simply provides sharper concurrency primitives, and sometimes that’s exactly 
what you want.