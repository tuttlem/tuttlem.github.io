---
layout: post
title: Understanding Pin-safe Types in Rust
date: 2025-05-26
comments: false
categories: [ rust, memory, unsafe, systems ]
---

# Introduction

Rust is famous for giving you memory safety without a garbage collector. But when you start doing lower-level work — 
self-referential structs, async state machines, or FFI — you run into a powerful but mysterious feature: [`Pin`](https://doc.rust-lang.org/std/pin/).

In this article, we’ll answer the following:

- What does it mean for a type to be *pin-safe*?
- Why would a type need to be pinned in the first place?
- How do you build one safely — without fighting the borrow checker?

We’ll walk through simple examples first, then build up to a self-referential type.

# What is a Pin-safe Type?

A **pin-safe type** is a type that can be safely used with Rust’s `Pin` API. 

{% include callout.html type="success" title="Pin:" text="It promises not to move itself in memory after being pinned, and it uses unsafe code responsibly to uphold that guarantee." %}

You create a `Pin-safe` type when:

- You need to guarantee that a value won’t move in memory after being created.
- You want to allow self-referencing inside a struct (e.g., a field pointing to another field).
- You're building async state machines, generators, or intrusive data structures.

# Self-Referential Structs: The Core Problem

Let’s look at a classic case. Say you have a struct like this:

{% highlight rust %}
struct Example {
    a: String,
    b: *const String, // b points to a
}
{% endhighlight %}

This is a **self-referential struct**: `b` stores a pointer to another field inside the same struct.

Seems harmless?

Here’s the catch: **Rust moves values around freely** — into function calls, collections, etc. If you set up a pointer 
inside a struct and then move the struct, your pointer is now invalid. This opens the door to **use-after-free** bugs.

Rust’s borrow checker normally prevents you from doing this. But sometimes you *do* need this — and that’s where `Pin` 
comes in.

# `Pin` to the Rescue

{% include callout.html type="success" title="Pin&lt;T&gt; says" text="Once this value is pinned, it cannot be moved again." %}

This is perfect for self-referential types — it guarantees their memory address won’t change.

But you have to build your type carefully to uphold this contract.

# A Pin-safe Self-Referential Type

Now let’s build a `Pin-safe` type step-by-step.

## Step 1: Define the structure

{% highlight rust %}
use std::pin::Pin;
use std::marker::PhantomPinned;

struct SelfRef {
    data: String,
    data_ref: *const String, // raw pointer, not a safe Rust reference
    _pin: PhantomPinned,     // opt-out of Unpin
}
{% endhighlight %}

- `data`: holds some content.
- `data_ref`: stores a pointer *to* that data.
- `PhantomPinned`: tells Rust this type is *not* safe to move after being pinned.

## Step 2: Pin and initialize

{% highlight rust %}
impl SelfRef {
    fn new(data: String) -> Pin<Box<SelfRef>> {
        let mut s = Box::pin(SelfRef {
            data,
            data_ref: std::ptr::null(),
            _pin: PhantomPinned,
        });

        let data_ref = &s.data as *const String;

        unsafe {
            let mut_ref = Pin::as_mut(&mut s);
            Pin::get_unchecked_mut(mut_ref).data_ref = data_ref;
        }

        s
    }

    fn get(&self) -> &String {
        unsafe { &*self.data_ref }
    }
}
{% endhighlight %}

## Step 3: Use it

{% highlight rust %}
fn main() {
    let s = SelfRef::new("Hello, world!".into());
    println!("Data ref points to: {}", s.get());
}
{% endhighlight %}

# Key Points

- You must pin the struct **before** setting the self-reference.
- `Box::pin` allocates it on the heap and returns a pinned pointer.
- `PhantomPinned` disables auto-`Unpin` so it can’t be accidentally moved.
- `unsafe` is required to set the internal pointer — you must guarantee it’s only done after pinning.

# Summary Table

| Concept               | Example                          | Needs `Pin`? | Why?                             |
|-----------------------|----------------------------------|--------------|----------------------------------|
| Normal struct         | `Logger { name: String }`        | ❌           | No self-references               |
| Self-referential      | `SelfRef { data, data_ref: &data }` | ✅           | Unsafe if moved                 |
| Async generators      | `async fn` or `Future`            | ✅           | Compiler may generate self-refs |
| FFI callbacks         | `extern "C"` with inner pointers  | ✅           | Must stay in place for C code    |

# Conclusion

Most types in Rust are move-safe and don’t need `Pin`. But when you’re working with:

- self-referential structs,
- low-level async primitives,
- foreign function interfaces (FFI),

...you may need to reach for `Pin`.

A Pin-safe type is your promise to the compiler that “this won’t move again — and I’ve made sure everything inside 
is OK with that.”

