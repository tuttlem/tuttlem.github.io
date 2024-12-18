---
layout: post  
title: Writing Python Extensions with Rust  
date: 2024-12-16  
comments: false  
categories: [ "rust", "python", "pyo3" ]  
---

# Introduction

Sometimes, you need to squeeze more performance out of your Python code, and one great way to do that is to offload some of your CPU-intensive tasks to an extension. Traditionally, you might use a language like C for this. I've covered this topic in a [previous post]({% post_url 2016-02-07-external-modules-in-python %}).

In today's post, we'll use the Rust language to create an extension that can be called from Python. We'll also explore the reverse: allowing your Rust code to call Python.

# Setup

Start by creating a new project. You'll need to switch to the nightly Rust compiler:

{% highlight shell %}
# Create a new project
cargo new hello_world_ext

cd hello_world_ext

# Set the preference to use the nightly compiler
rustup override set nightly
{% endhighlight %}

Next, ensure `pyo3` is installed with the `extension-module` feature enabled. Update your `Cargo.toml` file:

{% highlight toml %}
[package]
name = "hello_world_ext"
version = "0.1.0"
edition = "2021"

[lib]
name = "hello_world_ext"
crate-type = ["cdylib"]

[dependencies.pyo3]
version = "0.8.4"
features = ["extension-module"]
{% endhighlight %}

# Code

The project setup leaves you with a `main.rs` file in the `src` directory. Rename this to `lib.rs`.

Now, let's write the code for the extension. In the `src/lib.rs` file, define the functions you want to expose and the module they will reside in.

First, set up the necessary imports:

{% highlight rust %}
use pyo3::prelude::*;
use pyo3::wrap_pyfunction;
{% endhighlight %}

Next, define the function to expose:

{% highlight rust %}
#[pyfunction]
fn say_hello_world() -> PyResult<String> {
    Ok("Hello, world!".to_string())
}
{% endhighlight %}

This function simply returns the string `"Hello, world!"`.

The [`#[pyfunction]`](https://docs.rs/pyo3/latest/pyo3/attr.pyfunction.html) attribute macro exposes Rust functions to Python. The return type `PyResult<T>` is an alias for `Result<T, PyErr>`, which handles Python function call results.

Finally, define the module and add the function:

{% highlight rust %}
#[pymodule]
fn hello_world_ext(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_wrapped(wrap_pyfunction!(say_hello_world))?;
    Ok(())
}
{% endhighlight %}

The [`#[pymodule]`](https://docs.rs/pyo3/latest/pyo3/attr.pymodule.html) attribute macro defines the module. The `add_wrapped` method adds the wrapped function to the module.

# Building

With the code in place, build the module:

{% highlight shell %}
cargo build
{% endhighlight %}

Once built, install it as a Python package using [maturin](https://github.com/PyO3/maturin). First, set up a virtual environment and install `maturin`:

{% highlight shell %}
# Create a new virtual environment
python -m venv venv

# Activate the environment
source ./venv/bin/activate

# Install maturin
pip install maturin
{% endhighlight %}

Now, build and install the module:

{% highlight shell %}
maturin develop
{% endhighlight %}

The `develop` command that we use here builds our extension, and automatically installs the result into our virtual 
environment. This makes life easy for us during the development and testing stages.

# Testing

After installation, test the module in Python:

{% highlight python %}
>>> import hello_world_ext
>>> hello_world_ext.say_hello_world()
'Hello, world!'
{% endhighlight %}

Success! You've called a Rust extension from Python.

# Python from Rust

To call Python from Rust, follow this example from the [pyo3](https://pyo3.rs/v0.23.3/) homepage.

Create a new project:

{% highlight shell %}
cargo new py_from_rust
{% endhighlight %}

Update `Cargo.toml` to include `pyo3` with the `auto-initialize` feature:

{% highlight toml %}
[package]
name = "py_from_rust"
version = "0.1.0"
edition = "2021"

[dependencies.pyo3]
version = "0.23.3"
features = ["auto-initialize"]
{% endhighlight %}

Here is an example `src/main.rs` file:

{% highlight rust %}
use pyo3::prelude::*;
use pyo3::types::IntoPyDict;

fn main() -> PyResult<()> {
    Python::with_gil(|py| {
        let sys = py.import("sys")?;
        let version: String = sys.getattr("version")?.extract()?;

        let locals = [("os", py.import("os")?)].into_py_dict(py);
        let user: String = py.eval("os.getenv('USER') or os.getenv('USERNAME') or 'Unknown'", None, Some(&locals))?.extract()?;

        println!("Hello {}, I'm Python {}", user, version);
        Ok(())
    })
}
{% endhighlight %}

Build and run the project:

{% highlight shell %}
cargo build
cargo run
{% endhighlight %}

You should see output similar to:

{% highlight plain %}
Hello user, I'm Python 3.12.7 (main, Oct  1 2024, 11:15:50) [GCC 14.2.1 20240910]
{% endhighlight %}

# Conclusion

Rewriting critical pieces of your Python code in a lower-level language like Rust can significantly improve performance. With `pyo3`, the integration between Python and Rust becomes seamless, allowing you to harness the best of both worlds.
