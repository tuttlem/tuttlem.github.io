---
layout: post
title: Exploring async and await in Rust
date: 2024-12-24
comments: false
categories: [ rust, async, await ]
---

# Introduction 

Rust's `async` and `await` features bring modern asynchronous programming to the language, enabling developers to write 
non-blocking code efficiently. In this blog post, we’ll explore how `async` and `await` work, when to use them, and 
provide practical examples to demonstrate their power.

# What Are `async` and `await`?

Rust uses an `async` and `await` model to handle concurrency. These features allow you to write asynchronous code that 
doesn’t block the thread, making it perfect for tasks like I/O operations, networking, or any scenario where waiting on 
external resources is necessary.

## Key Concepts:

1. **`async`**:
   - Marks a function or block as asynchronous.
   - Returns a `Future` instead of executing immediately.

2. **`await`**:
   - Suspends the current function until the `Future` completes.
   - Only allowed inside an `async` function or block.

# Getting Started

To use `async` and `await`, you'll need an asynchronous runtime such as [Tokio](https://tokio.rs) or 
[async-std](https://async.rs). These provide the necessary infrastructure to execute asynchronous tasks.

## Practical Examples

### A Basic `async` Function

{% highlight rust %}
use tokio::time::{sleep, Duration};

async fn say_hello() {
    println!("Hello, world!");
    sleep(Duration::from_secs(2)).await; // Non-blocking wait
    println!("Goodbye, world!");
}

#[tokio::main]
async fn main() {
    say_hello().await;
}
{% endhighlight %}

**Explanation**:
- `say_hello` is an `async` function that prints messages and waits for 2 seconds without blocking the thread.
- The `.await` keyword pauses execution until the `sleep` operation completes.

### Running Tasks Concurrently with `join!`

{% highlight rust %}
use tokio::time::{sleep, Duration};

async fn task_one() {
    println!("Task one started");
    sleep(Duration::from_secs(2)).await;
    println!("Task one completed");
}

async fn task_two() {
    println!("Task two started");
    sleep(Duration::from_secs(1)).await;
    println!("Task two completed");
}

#[tokio::main]
async fn main() {
    tokio::join!(task_one(), task_two());
    println!("All tasks completed");
}
{% endhighlight %}

**Explanation**:
- `join!` runs multiple tasks concurrently.
- Task two finishes first, even though task one started earlier, demonstrating concurrency.

### Handling Errors in Asynchronous Code

{% highlight rust %}
async fn fetch_data(url: &str) -> Result<String, reqwest::Error> {
    let response = reqwest::get(url).await?.text().await?;
    Ok(response)
}

#[tokio::main]
async fn main() {
    match fetch_data("https://example.com").await {
        Ok(data) => println!("Fetched data: {}", data),
        Err(err) => eprintln!("Error fetching data: {}", err),
    }
}
{% endhighlight %}

**Explanation**:
- Uses the `reqwest` crate to fetch data from a URL.
- Error handling is built-in with `Result` and the `?` operator.

### Spawning Tasks with `tokio::task`

{% highlight rust %}
use tokio::task;
use tokio::time::{sleep, Duration};

async fn do_work(id: u32) {
    println!("Worker {} starting", id);
    sleep(Duration::from_secs(2)).await;
    println!("Worker {} finished", id);
}

#[tokio::main]
async fn main() {
    let handles: Vec<_> = (1..=5)
        .map(|id| task::spawn(do_work(id)))
        .collect();

    for handle in handles {
        handle.await.unwrap(); // Wait for each task to complete
    }
}
{% endhighlight %}

**Explanation**:
- `tokio::task::spawn` creates lightweight, non-blocking tasks.
- The `await` ensures all tasks complete before exiting.

### Asynchronous File I/O

{% highlight rust %}
use tokio::fs;

async fn read_file(file_path: &str) -> Result<String, std::io::Error> {
    let contents = fs::read_to_string(file_path).await?;
    Ok(contents)
}

#[tokio::main]
async fn main() {
    match read_file("example.txt").await {
        Ok(contents) => println!("File contents:\n{}", contents),
        Err(err) => eprintln!("Error reading file: {}", err),
    }
}
{% endhighlight %}

**Explanation**:
- Uses `tokio::fs` for non-blocking file reading.
- Handles file errors gracefully with `Result`.

# Key Points to Remember

1. **Async Runtime**:
   - You need an async runtime like Tokio or async-std to execute `async` functions.

2. **Concurrency**:
   - Rust’s async model is cooperative, meaning tasks must yield control for others to run.

3. **Error Handling**:
   - Combine `async` with `Result` for robust error management.

4. **State Sharing**:
   - Use `Arc` and `Mutex` for sharing state safely between async tasks.

## Conclusion

Rust’s `async` and `await` features empower you to write efficient, non-blocking code that handles concurrency 
seamlessly. By leveraging async runtimes and best practices, you can build high-performance applications that scale 
effortlessly.

Start experimenting with these examples and see how `async` and `await` can make your Rust code more powerful and 
expressive. Happy coding!
