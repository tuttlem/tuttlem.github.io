---
layout: post
title: Building a Daemon using Rust
date: 2024-11-16
comments: false
categories: [ "rust", "daemon" ]
---

# Introduction

Daemons — long-running background processes — are the backbone of many server applications and system utilities. In 
this tutorial, we’ll explore how to create a robust daemon using Rust, incorporating advanced concepts like double 
forking, `setsid`, signal handling, working directory management, file masks, and standard file descriptor redirection.

If you’re familiar with my earlier posts on [building CLI tools]({% post_url 2024-11-11-building-a-cli-tool-using-rust %}) 
and [daemon development in C]({% post_url 2019-07-05-daemon-development %}), this article builds on those concepts, 
showing how Rust can achieve similar low-level control while leveraging its safety and modern tooling.

# What Is a Daemon?

A daemon is a background process that runs independently of user interaction. It often starts at system boot and 
remains running to perform specific tasks, such as handling requests, monitoring resources, or providing services.

## Key Features of a Daemon

1. **Independence from a terminal**: It should not terminate if the terminal session closes.
2. **Clean shutdown**: Handle signals gracefully for resource cleanup.
3. **File handling**: Operate with specific file permissions and manage standard descriptors.

Rust, with its safety guarantees and powerful ecosystem, is an excellent choice for implementing these processes.

# Setup

First, we'll need to setup some dependencies.

Add these to your `Cargo.toml` file:

{% highlight toml %}
[dependencies]
log = "0.4"
env_logger = "0.11.5"
nix = { version = "0.29.0", features = ["process", "fs", "signal"] }
signal-hook = "0.3"
{% endhighlight %}

# Daemonization in Rust

The first step in daemonizing a process is separating it from the terminal and creating a new session. This involves 
**double forking** and calling `setsid`.

{% highlight rust %}
use nix::sys::stat::{umask, Mode};
use nix::sys::signal::{signal, SigHandler, Signal};
use std::fs::File;
use std::os::unix::io::AsRawFd;
use std::env;
use nix::unistd::{ForkResult, fork};

pub unsafe fn daemonize() -> Result<(), Box<dyn std::error::Error>> {
    // First fork
    match fork()? {
        ForkResult::Parent { .. } => std::process::exit(0),
        ForkResult::Child => {}
    }

    // Create a new session
    nix::unistd::setsid()?;

    // Ignore SIGHUP
    unsafe {
        signal(Signal::SIGHUP, SigHandler::SigIgn)?;
    }

    // Second fork
    match fork()? {
        ForkResult::Parent { .. } => std::process::exit(0),
        ForkResult::Child => {}
    }

    // Set working directory to root
    env::set_current_dir("/")?;

    // Set file mask
    umask(Mode::empty());

    // Close and reopen standard file descriptors
    close_standard_fds();

    Ok(())
}

fn close_standard_fds() {
    // Close STDIN, STDOUT, STDERR
    for fd in 0..3 {
        nix::unistd::close(fd).ok();
    }

    // Reopen file descriptors to /dev/null
    let dev_null = File::open("/dev/null").unwrap();
    nix::unistd::dup2(dev_null.as_raw_fd(), 0).unwrap(); // STDIN
    nix::unistd::dup2(dev_null.as_raw_fd(), 1).unwrap(); // STDOUT
    nix::unistd::dup2(dev_null.as_raw_fd(), 2).unwrap(); // STDERR
}
{% endhighlight %}

Notice the usage of `unsafe`. Because we are reaching out to some older system calls here, we need to bypass some of 
the safety that rust provides but putting this code into these `unsafe` blocks.

Whenever using unsafe in Rust:

* **Justify its Use**: Ensure it is necessary, such as for interacting with low-level system calls.
* **Minimize its Scope**: Encapsulate unsafe operations in a well-tested function to isolate potential risks.
* **Document Clearly**: Explain why unsafe is needed and how the function remains safe in practice.

# Handling Signals

Daemons need to handle signals for proper shutdown and cleanup. We’ll use the `signal-hook` crate for managing signals.

{% highlight rust %}
use signal_hook::iterator::Signals;
use std::thread;

pub fn setup_signal_handlers() -> Result<(), Box<dyn std::error::Error>> {
    // Capture termination and interrupt signals
    let mut signals = Signals::new(&[signal_hook::consts::SIGTERM, signal_hook::consts::SIGINT])?;

    thread::spawn(move || {
        for sig in signals.forever() {
            match sig {
                signal_hook::consts::SIGTERM | signal_hook::consts::SIGINT => {
                    log::info!("Received termination signal. Shutting down...");
                    std::process::exit(0);
                }
                _ => {}
            }
        }
    });

    Ok(())
}
{% endhighlight %}

# Managing the Environment

A daemon should start in a safe, predictable state.

## Working Directory

Change the working directory to a known location, typically the root directory (`/`).

{% highlight rust %}
env::set_current_dir("/")?;
{% endhighlight %}

## File Mask

Set the umask to `0` to ensure the daemon creates files with the desired permissions.

{% highlight rust %}
// Set file mask
umask(Mode::empty());
{% endhighlight %}

# Putting It All Together

Integrate the daemonization process with signal handling and environment setup in `main.rs`:

{% highlight rust %}
mod daemon;
mod signals;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize logging
    env_logger::init();

    log::info!("Starting daemonization process...");

    // Daemonize the process
    unsafe { daemon::daemonize()?; }

    // Set up signal handling
    signals::setup_signal_handlers()?;

    // Main loop
    loop {
        log::info!("Daemon is running...");
        std::thread::sleep(std::time::Duration::from_secs(5));
    }
}
{% endhighlight %}

Because we marked the `daemonize` function as `unsafe`, we must wrap it in `unsafe` to use it here.

# Advanced Features

## Signal Handlers for Additional Signals

Add handlers for non-critical signals like `SIGCHLD`, `SIGTTOU`, or `SIGTTIN`.

{% highlight rust %}
use nix::sys::signal::{SigHandler, Signal};

unsafe {
    signal(Signal::SIGCHLD, SigHandler::SigIgn)?;
    signal(Signal::SIGTTOU, SigHandler::SigIgn)?;
    signal(Signal::SIGTTIN, SigHandler::SigIgn)?;
}
{% endhighlight %}

# Integration with systemd

To run the daemon with `systemd`, create a service file:

{% highlight plain %}
[Unit]
Description=Logger Daemon
After=network.target

[Service]
ExecStart=/path/to/logger_daemon
Restart=always

[Install]
WantedBy=multi-user.target
{% endhighlight %}

# Conclusion

With the foundational concepts and Rust's ecosystem, you can build robust daemons that integrate seamlessly with the 
operating system. The combination of double forking, signal handling, and proper environment management ensures your 
daemon behaves predictably and safely.

A full example of this project is up on [my github](https://github.com/tuttlem/logger_daemon).