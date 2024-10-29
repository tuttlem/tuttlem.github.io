---
layout: post
title: Learning Rust Part 9 Files and I/O
date: 2024-10-30
comments: false
categories: [ "rust" ]
---

# Introduction

Rust’s I/O capabilities provide a range of options for efficiently handling files, streams, and standard input/output. 
Rust’s `std::fs` module offers synchronous file handling, while libraries like `tokio` and `async-std` add support for 
asynchronous I/O, enabling non-blocking operations. In this post, we’ll explore Rust’s key I/O operations, including 
file reading, writing, metadata, streaming, and error handling.

# Standard Input and Output

Rust provides convenient tools for interacting with the console, allowing programs to communicate with users or other 
processes.

## Standard Output

Rust’s `print!`, `println!`, and `eprintln!` macros are used to display messages. `println!` sends output to standard 
output, while `eprintln!` sends output to standard error.

{% highlight rust %}
fn main() {
    println!("Hello, world!");      // Standard output
    eprintln!("This is an error");   // Standard error
}
{% endhighlight %}

## Standard Input

To read user input, `std::io::stdin` provides a `read_line` method that stores console input into a `String`.

{% highlight rust %}
use std::io;

fn main() {
    let mut input = String::new();
    println!("Enter your name:");
    io::stdin().read_line(&mut input).expect("Failed to read input");
    println!("Hello, {}", input.trim());
}
{% endhighlight %}

# Reading and Writing Files

Rust’s `std::fs` module makes file reading and writing straightforward, offering methods like `File::open` for reading 
and `File::create` for writing.

## Reading Files

The `read_to_string` method reads the entire contents of a file into a `String`.

{% highlight rust %}
use std::fs;

fn main() -> std::io::Result<()> {
    let content = fs::read_to_string("example.txt")?;
    println!("{}", content);
    Ok(())
}
{% endhighlight %}

## Writing Files

To write to a file, use `File::create` to open or create the file, and `write_all` to write bytes to it.

{% highlight rust %}
use std::fs::File;
use std::io::Write;

fn main() -> std::io::Result<()> {
    let mut file = File::create("output.txt")?;
    file.write_all(b"Hello, world!")?;
    Ok(())
}
{% endhighlight %}

# Streaming I/O

Streaming I/O is efficient for reading or writing large files in chunks, especially when loading the entire file into 
memory is impractical. `BufReader` and `BufWriter` provide buffering for improved performance.

## Buffered Reading

`BufReader` reads data in chunks, storing it in a buffer for efficient access.

{% highlight rust %}
use std::fs::File;
use std::io::{self, BufRead, BufReader};

fn main() -> io::Result<()> {
    let file = File::open("example.txt")?;
    let reader = BufReader::new(file);

    for line in reader.lines() {
        println!("{}", line?);
    }
    Ok(())
}
{% endhighlight %}

## Buffered Writing

`BufWriter` buffers output, which is particularly useful when writing multiple small pieces of data.

{% highlight rust %}
use std::fs::File;
use std::io::{self, BufWriter, Write};

fn main() -> io::Result<()> {
    let file = File::create("buffered_output.txt")?;
    let mut writer = BufWriter::new(file);

    writer.write_all(b"Hello, world!")?;
    writer.flush()?; // Ensures all data is written
    Ok(())
}
{% endhighlight %}

# File Metadata and Permissions

Rust allows access to and modification of file metadata, including permissions and timestamps, via the `metadata` method 
and the `Permissions` struct.

## Retrieving Metadata

The `metadata` function provides details such as file size and permissions.

{% highlight rust %}
use std::fs;

fn main() -> std::io::Result<()> {
    let metadata = fs::metadata("example.txt")?;
    println!("File size: {}", metadata.len());
    println!("Is read-only: {}", metadata.permissions().readonly());
    Ok(())
}
{% endhighlight %}

## Changing Permissions

You can modify file permissions with `set_permissions`, which can be particularly useful for restricting access to 
sensitive files.

{% highlight rust %}
use std::fs;
use std::os::unix::fs::PermissionsExt;

fn main() -> std::io::Result<()> {
    let mut perms = fs::metadata("example.txt")?.permissions();
    perms.set_readonly(true);
    fs::set_permissions("example.txt", perms)?;
    Ok(())
}
{% endhighlight %}

# Asynchronous I/O

For non-blocking I/O, Rust offers asynchronous support through libraries like `tokio` and `async-std`. These libraries 
allow file and network operations to run without blocking the main thread, making them ideal for scalable applications.

## Using Tokio for Async I/O

The `tokio::fs` module provides async counterparts to common file operations, like reading and writing.

{% highlight rust %}
use tokio::fs::File;
use tokio::io::AsyncWriteExt;

#[tokio::main]
async fn main() -> tokio::io::Result<()> {
    let mut file = File::create("async_output.txt").await?;
    file.write_all(b"Hello, async world!").await?;
    Ok(())
}
{% endhighlight %}

## Async Streaming with Tokio

`BufReader` and `BufWriter` are also available in asynchronous forms with Tokio, enabling efficient non-blocking I/O.

{% highlight rust %}
use tokio::fs::File;
use tokio::io::{self, AsyncBufReadExt, BufReader};

#[tokio::main]
async fn main() -> io::Result<()> {
    let file = File::open("example.txt").await?;
    let reader = BufReader::new(file);

    let mut lines = reader.lines();
    while let Some(line) = lines.next_line().await? {
        println!("{}", line);
    }
    Ok(())
}
{% endhighlight %}

# Error Handling in I/O Operations

Error handling is essential in I/O operations, as access to files can fail due to permissions, missing files, or storage 
limitations. Rust’s `Result` type and the `?` operator streamline error handling in I/O tasks.

## Using `Result` and `?` for Concise Error Handling

Most I/O functions return `Result`, enabling explicit error handling or propagation with `?`. We covered this syntax in 
[part 3]({% post_url 2024-10-29-learning-rust-part-3-error-handling }) of this series.

{% highlight rust %}
use std::fs;

fn read_file() -> std::io::Result<String> {
    let content = fs::read_to_string("example.txt")?;
    Ok(content)
}

fn main() {
    match read_file() {
        Ok(content) => println!("File content: {}", content),
        Err(e) => eprintln!("Error reading file: {}", e),
    }
}
{% endhighlight %}

# Summary

Rust provides comprehensive tools for file handling and I/O, from basic read/write operations to asynchronous streaming 
and metadata management. With built-in error handling and async capabilities, Rust’s I/O tools allow for efficient, 
flexible, and reliable code, making it well-suited for building high-performance applications that handle complex I/O 
tasks with ease.

