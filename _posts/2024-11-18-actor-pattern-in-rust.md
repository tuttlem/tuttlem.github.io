---
layout: post
title: Actor Pattern in Rust
date: 2024-11-18
comments: false
categories: [ "rust" ]
---

# Introduction

Concurrency is a cornerstone of modern software development, and the actor pattern is a well-established model for 
handling concurrent computations. Rust, with its focus on safety, performance, and concurrency, provides an excellent 
platform for implementing the actor model. In this article, we'll explore what the actor pattern is, how it works in 
Rust, and dive into some popular libraries that implement it.

# What is the Actor Pattern?

The actor pattern revolves around the concept of "actors," which are independent, lightweight entities that communicate 
exclusively through message passing. Each actor encapsulates state and behavior, processing messages asynchronously and 
maintaining its own isolated state. This model eliminates the need for shared state, reducing the complexity and risks 
associated with multithreaded programming.

## Why Use the Actor Pattern?

* **Isolation**: Each actor manages its own state, ensuring safety.
* **Message Passing**: Communication happens via asynchronous messages, avoiding direct interactions or locks.
* **Fault Tolerance** : Actor hierarchies can implement supervision strategies, enabling automatic recovery from failures.

# Libraries

As a basic example for comparison, we'll create an actor that handles one message "Ping".

## Actix

[Actix](https://actix.rs/) is the most popular and mature actor framework in Rust. Built on top of `tokio`, it offers 
high-performance async I/O along with a robust actor-based architecture.

### Features:

* Lightweight actors with asynchronous message handling.
* Built-in supervision for error recovery.
* Excellent integration with web development (`actix-web`).

### Example:

Here’s how to create a simple actor that responds to messages with Actix:

{% highlight toml %}
actix = "0.13.5"
{% endhighlight %}

{% highlight rust %}
use actix::prelude::*;

struct MyActor;

impl Actor for MyActor {
    type Context = Context<Self>;
}

struct Ping;

impl Message for Ping {
    type Result = String; 
}

impl Handler<Ping> for MyActor {
    type Result = String; 

    fn handle(&mut self, _msg: Ping, _ctx: &mut Context<Self>) -> Self::Result {
        "Pong".to_string()
    }
}

#[actix::main]
async fn main() {
    let addr = MyActor.start();
    let res = addr.send(Ping).await.unwrap();
    println!("Response: {}", res);
}
{% endhighlight %}

### Breakdown

* Any rust type can be an actor, it only needs to implement the `Actor` trait
  * We've defined `MyActor` for this
* To be able to handle a specific message the actor has to provide a `Handler<M>` implementation
  * The `Ping` message is defined and handled by `MyActor`'s `handle` function
* The actor is now `start`ed
* A `Ping` message is sent, and the response is waited on


## Riker

Inspired by Akka (Scala's popular actor framework), [Riker](https://github.com/riker-rs/riker) is another actor-based 
framework in Rust. While less active than Actix, Riker focuses on distributed systems and fault tolerance.

### Features:

* Actor supervision strategies.
* Distributed messaging.
* Strong typing for messages.

### Example:

This example is taken from the Riker Github repository:

{% highlight toml %}
riker = "0.4.2"
{% endhighlight %}

{% highlight rust %}
use std::time::Duration;
use riker::actors::*;

#[derive(Default)]
struct MyActor;

// implement the Actor trait
impl Actor for MyActor {
    type Msg = String;

    fn recv(&mut self,
            _ctx: &Context<String>,
            msg: String,
            _sender: Sender) {

        if msg == "Ping" {
            println!("Pong!");
        } else {
            println!("Received: {}", msg);
        }

    }
}

// start the system and create an actor
fn main() {
    let sys = ActorSystem::new().unwrap();

    let my_actor = sys.actor_of::<MyActor>("my-actor").unwrap();

    my_actor.tell("Ping".to_string(), None);

    std::thread::sleep(Duration::from_millis(500));
}
{% endhighlight %}

### Breakdown

* `MyActor` is implemented from an `Actor` trait
* Messages are handled by the `recv` function
* An actor system is started with `ActorSystem::new()`
* We need to wait at the end for the message to be processed 

## Xactor

[xactor](https://github.com/sunli829/xactor) is a more modern and ergonomic actor framework, simplifying async/await 
integration compared to Actix. xactor is based on `async-std`.

### Example:

This example was taken from xactor's Github README.

{% highlight toml %}
xactor = "0.7.11"
{% endhighlight %}

{% highlight rust %}
use xactor::*;

#[message(result = "String")]
struct Ping;

struct MyActor;

impl Actor for MyActor {}

#[async_trait::async_trait]
impl Handler<Ping> for MyActor {
    async fn handle(&mut self, _ctx: &mut Context<Self>, _: Ping) -> String {
        "Pong".to_string()
    }
}

#[xactor::main]
async fn main() -> Result<()> {
    // Start actor and get its address
    let addr = MyActor.start().await?;

    let res = addr.call(Ping).await?;
    println!("{}", res);

    Ok(())
}
{% endhighlight %}

### Breakdown

* Defined is a `MyActor` actor trait, and a `Ping` message
* The `handle` function is implemented for `MyActor`
* Using this framework, `async` and `await` allows for the result to be waited on

## Advantages of the Actor Pattern in Rust

Rust's concurrency features and the actor model complement each other well:

* **Memory Safety**: The actor model eliminates data races, and Rust's borrow checker enforces safe state access.
* **Scalability**: Asynchronous message passing allows scaling systems efficiently.
* **Fault Tolerance**: Supervision hierarchies help manage errors and recover gracefully.

## When to Use the Actor Pattern

The actor pattern is a good fit for:

* **Distributed Systems**: Where isolated units of computation need to communicate across nodes.
* **Concurrent Systems**: That require fine-grained message handling without shared state.
* **Web Applications**: With complex stateful backends (e.g., using Actix-Web).

## Alternatives to the Actor Pattern

While powerful, the actor model isn't always necessary. Rust offers other concurrency paradigms:

* **Channels**: Using `std::sync::mpsc` or `tokio::sync::mpsc` for message passing.
* **Shared-State Concurrency**: Leveraging `Arc<Mutex<T>>` to manage shared state.
* **Futures and Tasks**: Directly working with Rust’s async ecosystem.

# Conclusion

The actor pattern is alive and well in Rust, with libraries like `Actix`, `Riker`, and `xactor` making it accessible to 
developers. Whether you’re building distributed systems, scalable web applications, or concurrent computation engines, 
the actor model can simplify your design while leveraging Rust’s safety and performance guarantees.
