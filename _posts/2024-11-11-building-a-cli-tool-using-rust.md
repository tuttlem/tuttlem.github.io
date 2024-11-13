---
layout: post
title: Building a CLI Tool using Rust
date: 2024-11-11
comments: false
categories: [ "rust", "cli" ]
---

# Introduction

Command-Line Interface (CLI) tools are fundamental for developers, system administrators, and power users alike, 
offering efficient ways to perform tasks, automate processes, and manage systems. Rust is a popular choice for creating 
CLI tools due to its high performance, reliability, and modern tooling support. 

In this tutorial, we’ll walk through building a simple Rust CLI tool that **flips the case of a given string**—converting 
uppercase letters to lowercase and vice versa. By exposing this small function through the command line, we’ll cover 
Rust’s basics for CLI development, including handling arguments, configuration files, error handling, and more. 

## Overview

Here’s the roadmap for this tutorial:

1. **Setting Up:** Create a scalable project directory.
2. **Parsing Command-Line Arguments:** Handle CLI inputs using Rust's `std::env::args` and the `clap` crate.
3. **Adding Configuration:** Set up external configuration options with `serde`.
4. **Using Standard Streams:** Handle standard input and output for versatile functionality.
5. **Adding Logging:** Use logging to monitor and debug the application.
6. **Error Handling:** Make errors descriptive and friendly.
7. **Testing:** Write unit and integration tests.
8. **Distribution:** Build and distribute the CLI tool.

# Setting Up

Let’s start by creating a basic Rust project structured to support scalability and best practices.

## Creating a Rust Project

Open a terminal and create a new project:

{% highlight rust %}
cargo new text_tool
cd text_tool
{% endhighlight %}

This initializes a Rust project with a basic src directory containing `main.rs`. However, rather than placing all our 
code in `main.rs`, let’s structure our project with separate modules and a clear `src` layout.

## Directory Structure

To make our project modular and scalable, let’s organize our project directory as follows:

{% highlight plain %}
text_tool
├── src
│   ├── main.rs    # main entry point of the program
│   ├── lib.rs     # main library file
│   ├── config.rs  # configuration-related code
│   ├── cli.rs     # command-line parsing logic
├── tests
│   └── integration_test.rs # integration tests
└── Cargo.toml
{% endhighlight %}

* `main.rs`: The primary entry point, managing the CLI tool setup and orchestrating modules.
* `lib.rs`: The library file, which makes our code reusable.
* `config.rs`, `cli.rs`: Modules for specific functions—parsing CLI arguments, handling configuration.

This structure keeps our code modular, organized, and easy to test and maintain. Throughout the rest of the tutorial, 
we’ll add components to each module, implementing new functionality step-by-step.

# Parsing the Command Line

Rust’s `std::env::args` allows us to read command-line arguments directly. However, for robust parsing, validation, and 
documentation, we’ll use the `clap` crate, a powerful library for handling CLI arguments.

## Using std::env::args

To explore the basics, let’s try out `std::env::args` by updating `main.rs` to print any arguments provided by the user:

{% highlight rust %}
// main.rs
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("{:?}", args);
}
{% endhighlight %}

Running `cargo run -- hello world` will output the full list of command-line arguments, with the first entry as the 
binary name itself.

## Switching to `clap`

While `std::env::args` works, `clap` makes argument parsing cleaner and adds support for help messages, argument 
validation, and more.

Add `clap` to your project by updating `Cargo.toml`:

{% highlight toml %}
[dependencies]
clap = "4.0"
{% endhighlight %}

Then, update `src/cli.rs` to define the CLI arguments and sub-commands:

{% highlight rust %}
use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(name = "Text Tool")]
#[command(about = "A simple CLI tool for text transformations", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Uppercase { input: String, output: Option<String> },
    Lowercase { input: String, output: Option<String> },
    Replace { input: String, from: String, to: String, output: Option<String> },
    Count { input: String },
}

fn main() {
    let args = Cli::parse();
    match &args.command {
        Commands::Uppercase { input, output } => { /* function call */ },
        Commands::Lowercase { input, output } => { /* function call */ },
        Commands::Replace { input, from, to, output } => { /* function call */ },
        Commands::Count { input } => { /* function call */ },
    }
}
{% endhighlight %}

In `main.rs`, configure the clap command and process arguments:

{% highlight rust %}
// main.rs
mod cli;

fn main() {
    let matches = cli::build_cli().get_matches();
    let input = matches.value_of("input").unwrap();
    println!("Input: {}", input);
}
{% endhighlight %}

# Adding Configuration

To add configuration flexibility, we’ll use the serde crate to allow loading options from an external file, letting 
users configure input and output file paths, for example.

Add `serde` and `serde_json` to `Cargo.toml`:

{% highlight toml %}
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
{% endhighlight %}

Define the configuration in `src/config.rs`:

{% highlight rust %}
// src/config.rs
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct Config {
    pub input_file: Option<String>,
    pub output_file: Option<String>,
}
{% endhighlight %}

This function will look for a `config.toml` file with a structure like:

{% highlight toml %}
default_output = "output.txt"
{% endhighlight %}

# Using Standard Streams

Like any well-behaved unix tool, we look to take advantage of the standard streams like `STDIN`, `STDOUT`, and `STDERR` 
so that users of our tool can utilise pipes and redirection and compose our tool in among any of the other tools.

In the case of this application, if we don't receive an input via the command line parameters, the tool will assume 
that input is being delivered over `STDIN`:

{% highlight rust %}
use std::fs::File;
use std::io::{self, Read};

/// Reads content from a file if a path is provided, otherwise reads from STDIN.
fn read_input(input_path: Option<&str>) -> Result<String, io::Error> {
    let mut content = String::new();

    if let Some(path) = input_path {
        // Read from file
        let mut file = File::open(path)?;
        file.read_to_string(&mut content)?;
    } else {
        // Read from STDIN
        io::stdin().read_to_string(&mut content)?;
    }

    Ok(content)
}
{% endhighlight %}

`read_input` here handling both scenarios for us.

To integrate with `STDOUT` we simply use our logging facilities.

# Adding Logging

Using the `log` macros, we can send messages out to `STDOUT` that are classified into different severities. These 
severities are:

| Level | Description |
|-------|---------------------------------|
| `trace` | Use this log level to set tracing in your code |
|`debug` |	Useful for debugging; provides insights into internal states. |
|`info`	| General information about what the tool is doing. |
|`warn`	| Indicates a potential problem that isn’t necessarily critical. |
|`error` |	Logs critical issues that need immediate attention. |

These log levels allow developers to adjust the verbosity of the logs based on the environment or specific needs. 
Here’s how we can add logging to our Rust CLI.

## Initialize the logger

At the start of your application, initialize the `env_logger`. This reads an environment variable (usually `RUST_LOG`) 
to set the desired log level.

{% highlight rust %}
use log::{info, warn, error, debug, trace};

fn main() {
    env_logger::init();

    info!("Starting the text tool application");

    // Example logging at different levels
    trace!("This is a trace log - very detailed.");
    debug!("This is a debug log - useful for development.");
    warn!("This is a warning - something unexpected happened, but it’s not critical.");
    error!("This is an error - something went wrong!");
}
{% endhighlight %}

## Setting log levels

With `env_logger`, you can control the logging level via the `RUST_LOG` environment variable. This lets users or 
developers dynamically set the level of detail they want to see without changing the code.

{% highlight bash %}
RUST_LOG=info ./text_tool
{% endhighlight %}

## Using Log Messages in Functions

Add log messages throughout your functions to provide feedback on various stages or states of the process. Here’s how 
logging can be added to a text transformation function:

{% highlight rust %}
pub fn uppercase(input: &str) -> Result<String, std::io::Error> {
    log::debug!("Attempting to read input from '{}'", input);

    let content = std::fs::read_to_string(input)?;
    log::info!("Converting text to uppercase");

    let result = content.to_uppercase();

    log::debug!("Finished transformation to uppercase");
    Ok(result)
}
{% endhighlight %}

## Environment-Specific Logging

During development, you might want `debug` or `trace` logs to understand the application flow. In production, however, you 
might set the log level to `info` or `warn` to avoid verbose output. The `env_logger` configuration allows for this 
flexibility without code changes.

## Why Logging Matters

Logging gives developers and users insight into the application’s behavior and status, helping identify issues, track 
performance, and understand what the tool is doing. This flexibility and transparency in logging make for a more robust, 
user-friendly CLI tool.

Using these logging best practices will make your Rust CLI tool easier to debug, monitor, and maintain, especially as 
it grows or gets deployed to different environments.

# Error Handling

In a CLI tool, it’s crucial to handle errors gracefully and present clear messages to users. Rust’s `Result` type 
makes it easy to propagate errors up the call chain, where they can be handled in a central location. We’ll log error 
messages to help users and developers understand what went wrong.

## Define a Custom Error Type

Defining a custom error type allows you to capture specific error cases and add contextual information.

{% highlight rust %}
use std::fmt;

#[derive(Debug)]
enum CliError {
    Io(std::io::Error),
    Config(config::ConfigError),
    MissingArgument(String),
}

impl fmt::Display for CliError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CliError::Io(err) => write!(f, "I/O error: {}", err),
            CliError::Config(err) => write!(f, "Configuration error: {}", err),
            CliError::MissingArgument(arg) => write!(f, "Missing required argument: {}", arg),
        }
    }
}

impl From<std::io::Error> for CliError {
    fn from(err: std::io::Error) -> CliError {
        CliError::Io(err)
    }
}
{% endhighlight %}

## Returning Errors from Functions

In each function, use `Result<T, CliError>` to propagate errors. For example, in a function reading from a file or 
`STDIN`, return a Result so errors bubble up:

{% highlight rust %}
fn read_input(input_path: Option<&str>) -> Result<String, CliError> {
    let mut content = String::new();
    
    if let Some(path) = input_path {
        let mut file = std::fs::File::open(path)?;
        file.read_to_string(&mut content)?;
    } else {
        std::io::stdin().read_to_string(&mut content)?;
    }

    Ok(content)
}
{% endhighlight %}

## Logging Errors and Returning `ExitCode`

In `main.rs`, handle errors centrally. If an error occurs, log it at an appropriate level and exit with a non-zero 
status code. For critical issues, use `error!`, while `warn!` is suitable for non-fatal issues.

{% highlight rust %}
use std::process::ExitCode;
use log::{error, warn};

fn main() -> ExitCode {
    env_logger::init();

    match run() {
        Ok(_) => {
            log::info!("Execution completed successfully");
            ExitCode::SUCCESS
        }
        Err(err) => {
            // Log the error based on its type or severity
            match err {
                CliError::MissingArgument(_) => warn!("{}", err),
                _ => error!("{}", err),
            }

            ExitCode::FAILURE
        }
    }
}

fn run() -> Result<(), CliError> {
    // Your main application logic here
    Ok(())
}
{% endhighlight %}

## Presenting Error Messages to the User

By logging errors at different levels, users get clear, contextual feedback. Here’s an example scenario where an error 
is encountered:

{% highlight rust %}
fn uppercase(input: Option<&str>) -> Result<String, CliError> {
    let input_path = input.ok_or_else(|| CliError::MissingArgument("input file".to_string()))?;
    let content = read_input(Some(input_path))?;
    Ok(content.to_uppercase())
}
{% endhighlight %}

# Testing

To ensure our CLI tool functions correctly, we’ll set up both unit tests and integration tests. Unit tests allow us to 
validate individual transformation functions, while integration tests test the CLI’s behavior from end to end.

## Testing Core Functions

In Rust, unit tests typically go in the same file as the function they’re testing. Since our main transformation 
functions are in `src/lib.rs`, we’ll add unit tests there.

Here’s an example of how to test the `uppercase` function:

{% highlight rust %}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_uppercase() {
        let input = "hello world";
        let expected = "HELLO WORLD";
        
        let result = uppercase(input).unwrap();
        
        assert_eq!(result, expected);
    }

    #[test]
    fn test_replace() {
        let input = "hello world";
        let from = "world";
        let to = "Rust";
        let expected = "hello Rust";
        
        let result = replace(input, from, to).unwrap();
        
        assert_eq!(result, expected);
    }
}
{% endhighlight %}

Each test function:

* Calls the transformation function with specific input.
* Asserts that the result matches the expected output, ensuring each function behaves correctly in isolation.

## Integration Tests for End-to-End Behavior

Integration tests verify that the CLI as a whole works as expected, handling command-line arguments, file I/O, 
and expected outputs. These tests go in the `tests/` directory, with each test file representing a suite of related 
tests.

Let’s create an integration test in `tests/integration_test.rs`:

{% highlight rust %}
use assert_cmd::Command;

#[test]
fn test_uppercase_command() {
    let mut cmd = Command::cargo_bin("text_tool").unwrap();
    
    cmd.arg("uppercase").arg("hello.txt").assert().success();
}

#[test]
fn test_replace_command() {
    let mut cmd = Command::cargo_bin("text_tool").unwrap();
    
    cmd.arg("replace").arg("hello.txt").arg("hello").arg("Rust").assert().success();
}
{% endhighlight %}

In this example:

* We use the `assert_cmd` crate, which makes it easy to test command-line applications by running them as subprocesses.
* Each test case calls the CLI with arguments to simulate user input and checks that the process completes successfully (`assert().success()`).
* Additional assertions can check the output to ensure that the CLI’s behavior matches expectations.

## Testing for Errors

We should also verify that errors are handled correctly, showing meaningful messages without crashing. This is 
especially useful for testing scenarios where users might provide invalid inputs or miss required arguments.

Here’s an example of testing an expected error:

{% highlight rust %}
#[test]
fn test_missing_argument_error() {
    let mut cmd = Command::cargo_bin("text_tool").unwrap();
    
    cmd.arg("replace").arg("hello.txt")  // Missing "from" and "to" arguments
        .assert()
        .failure()
        .stderr(predicates::str::contains("Missing required argument"));
}
{% endhighlight %}

This test:

* Runs the CLI without the necessary arguments.
* Asserts that the command fails (`.failure()`) and that the error message contains a specific string. The predicates crate is handy here for asserting on specific error messages.

## Snapshot Testing for Outputs

Snapshot testing is useful for CLI tools that produce consistent, predictable output. A snapshot test compares the 
tool’s output to a saved “snapshot” and fails if the output changes unexpectedly.

Using the `insta` crate for snapshot testing:

{% highlight rust %}
use insta::assert_snapshot;

#[test]
fn test_uppercase_output() {
    let mut cmd = Command::cargo_bin("text_tool").unwrap();
    let output = cmd.arg("uppercase").arg("hello.txt").output().unwrap();

    assert_snapshot!(String::from_utf8_lossy(&output.stdout));
}
{% endhighlight %}

This test:

* Runs the `uppercase` command and captures its output.
* Compares the output to a stored snapshot, failing if they don’t match. This approach is excellent for catching unexpected changes in output format.

## Running Tests

To run all tests (both unit and integration), use:

{% highlight bsah %}
cargo test
{% endhighlight %}

If you’re using `assert_cmd` or `insta`, add them as development dependencies in `Cargo.toml`:

{% highlight toml %}
[dev-dependencies]
assert_cmd = "2.0"
insta = "1.16"
predicates = "2.1"  # For testing error messages
{% endhighlight %}

# Distribution

Distributing your Rust CLI tool doesn’t need to be complicated. Here’s a simple way to package it so that others can 
easily download and use it.

## Build the Release Binary

First, compile a release version of your application. The release build optimizes for performance, making it 
faster and smaller.

{% highlight plain %}
cargo build --release
{% endhighlight %}

This command creates an optimized binary in `target/release/`. The resulting file (e.g., `text_tool` on Linux/macOS or 
`text_tool.exe` on Windows) is your compiled CLI tool, ready for distribution.

## Distribute the Binary Directly

For quick sharing, you can simply share the binary file. Make sure it’s compiled for the target platform (Linux, macOS, 
or Windows) that your users need.

1. **Zip the Binary**: Compress the binary into a `.zip` or `.tar.gz` archive so users can download and extract it easily.

   {% highlight plain %}
   zip text_tool.zip target/release/text_tool
   {% endhighlight %}

2. **Add Instructions**: In the same directory as your binary, add a `README.md` or `INSTALL.txt` file with basic instructions on how to use and run the tool.

## Publishing on GitHub Releases

If you want to make the tool available for a broader audience, consider uploading it to GitHub. Here’s a quick process:

1. **Create a GitHub Release**: Go to your GitHub repository and click **Releases** > **Draft a new release**.

2. **Upload the Binary**: Attach your zipped binary (like `text_tool.zip`) to the release.

3. **Add a Release Note**: Include a description of the release, any new features, and basic installation instructions.

## Cross-Platform Binaries (Optional)

To make your tool available on multiple platforms, consider cross-compiling:

- For Linux: {% highlight plain %}cargo build --release --target x86_64-unknown-linux-musl{% endhighlight %}
- For Windows: {% highlight plain %}cargo build --release --target x86_64-pc-windows-gnu{% endhighlight %}
- For macOS: Run the default release build on macOS.

# Putting it all together

The full code for a `text_tool` application written in Rust can be found in my Github repository [here](https://github.com/tuttlem/text_tool).

This should take you through most of the concepts here, and also give you a robust start on creating your own CLI apps.
