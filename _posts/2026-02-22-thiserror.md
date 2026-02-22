---
layout: post
title: thiserror
date: 2026-02-22
comments: false
categories: [ rust crates error-handling ]
---

In the [previous article]({% post_url 2026-02-21-anyhow %}), we looked at `anyhow` — a pragmatic error type for 
applications.

But what if you're writing a library? Now things change.

Applications can afford type erasure. Libraries cannot.

If your crate exposes errors publicly, callers need to:

- Match on them
- React differently to different failures
- Potentially convert or wrap them

That requires structured error types.

That’s where `thiserror` fits.

## What Problem Does `thiserror` Solve?

Defining error enums in Rust is straightforward — but repetitive.

Without helper crates, you end up writing:

- `std::fmt::Display` manually
- `std::error::Error` manually
- `From<T>` implementations manually

Typically, this boilerplate looks like this:

{% highlight rust %}
use std::fmt;
use std::num::ParseIntError;

#[derive(Debug)]
pub enum ParsePortError {
    Empty,
    InvalidNumber(ParseIntError),
    OutOfRange(u16),
}

impl fmt::Display for ParsePortError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParsePortError::Empty =>
                write!(f, "input was empty"),

            ParsePortError::InvalidNumber(_) =>
                write!(f, "invalid number"),

            ParsePortError::OutOfRange(port) =>
                write!(f, "port out of range (0-65535): {}", port),
        }
    }
}

impl std::error::Error for ParsePortError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            ParsePortError::InvalidNumber(err) => Some(err),
            _ => None,
        }
    }
}

impl From<ParseIntError> for ParsePortError {
    fn from(err: ParseIntError) -> Self {
        ParsePortError::InvalidNumber(err)
    }
}
{% endhighlight %}

`thiserror` removes that boilerplate while keeping your error types explicit and structured.

It does not introduce a new error model. It simply makes the standard one ergonomic.

## Minimal Example

Let’s build a small parsing function inside a pretend library.

### Cargo.toml

{% highlight toml %}
[package]
name = "thiserror_demo"
version = "0.1.0"
edition = "2021"

[dependencies]
thiserror = "1"
{% endhighlight %}

### lib.rs

{% highlight rust %}
use std::num::ParseIntError;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParsePortError {
    #[error("input was empty")]
    Empty,

    #[error("invalid number")]
    InvalidNumber(#[from] ParseIntError),

    #[error("port out of range (0-65535): {0}")]
    OutOfRange(u16),
}

pub fn parse_port(input: &str) -> Result<u16, ParsePortError> {
    if input.is_empty() {
        return Err(ParsePortError::Empty);
    }

    let value: u16 = input.parse()?;

    if value > 65535 {
        return Err(ParsePortError::OutOfRange(value));
    }

    Ok(value)
}
{% endhighlight %}

## What’s Actually Happening?

The key piece is:

{% highlight rust %}
#[derive(Error, Debug)]
{% endhighlight %}

This macro generates:

- `impl std::fmt::Display`
- `impl std::error::Error`
- `From<T>` implementations where `#[from]` is used

Nothing magical. No hidden runtime.

Just code generation that removes ceremony.

### The `#[error(...)]` Attribute

Each enum variant defines its display string:

{% highlight rust %}
#[error("port out of range (0-65535): {0}")]
{% endhighlight %}

The formatting syntax behaves like `format!`.

This keeps error messaging colocated with the type definition — which is exactly where it belongs.

### The `#[from]` Attribute

This line:

{% highlight rust %}
InvalidNumber(#[from] ParseIntError),
{% endhighlight %}

Generates:

{% highlight rust %}
impl From<ParseIntError> for ParsePortError
{% endhighlight %}

Which is why this works:

{% highlight rust %}
let value: u16 = input.parse()?;
{% endhighlight %}

The `?` operator automatically converts `ParseIntError` into `ParsePortError`.

That’s clean design. No glue code required.

## Where It Fits

`thiserror` is for:

- Public library crates
- Internal shared libraries
- Modules that define a clear error boundary

You use it when:

- Error types are part of your API contract
- Callers must distinguish between failure modes
- You care about structured recovery

This is the opposite end of the spectrum from `anyhow`.

Where `anyhow` erases types, `thiserror` preserves them.

## How It Pairs With `anyhow`

The modern pattern in Rust looks like this:

- Libraries define structured errors with `thiserror`.
- Applications consume them and convert into `anyhow::Error`.

For example:

{% highlight rust %}
fn main() -> anyhow::Result<()> {
    let port = parse_port("abc")?;
    println!("{port}");
    Ok(())
}
{% endhighlight %}}

The structured error flows upward — then becomes type-erased at the application boundary.

That separation is intentional.

## Trade-offs

### Pros

- Zero runtime overhead
- Clear, explicit error types
- Eliminates boilerplate
- Works perfectly with `?`

### Cons

- Still requires you to design error enums properly
- Easy to over-design large error hierarchies
- Adds a derive dependency (macro-based)

The biggest risk is not technical — it’s architectural. Don’t build elaborate error trees unless they actually 
serve callers.

## Should You Use It?

If you're writing a reusable library:

Yes.

If you're writing a binary and don’t care about matching on errors:

Probably not.

You could still use it — but `anyhow` is usually simpler. `thiserror` enforces discipline. It keeps error design 
explicit without making it painful. And in serious Rust codebases, that balance matters.
