---
layout: post
title: Exploring Continuations and CPS Transformation
date: 2025-06-23
comments: false
categories: [ "javascript", "rust", "python", "async", "continuations" ]
---

# Introduction

Continuations are one of those ideas that seem abstract at first — but once they click, you’ll start seeing them 
everywhere: in asynchronous code, in exception handling, in generators, even in the way you reason about program flow.

In this post, we’ll explore what continuations are, how Continuation-Passing Style (CPS) transforms your code, and how 
these concepts are quietly powering modern async constructs like `async/await`.

We’ll walk through synchronous code, asynchronous patterns with callbacks and promises, and finally reach a new 
understanding of what’s really going on under the hood. You won’t need to reimplement a language or build a compiler 
to follow along — we’ll do everything with regular JavaScript, Python, and Rust.

# What is a Continuation?

A *continuation* is a representation of “what to do next” in a program. At any point in your code, the rest of the 
computation can be thought of as a function — the continuation.

Let’s start simple:

{% highlight javascript %}
function addOne(x) {
  return x + 1;
}

console.log(addOne(2)); // prints 3
{% endhighlight %}

Now, instead of returning, what if we *passed the result to another function* — the continuation?

{% highlight javascript %}
function addOneCPS(x, cont) {
  cont(x + 1);
}

addOneCPS(2, (result) => {
  console.log(result); // prints 3
});
{% endhighlight %}

This style is called **Continuation-Passing Style (CPS)**. In CPS, **functions never return** — they 
*call their continuation* instead.

This isn't immediately remarkable. Callbacks have been used widely in code for quite some time now. This is just 
building a picture of where we've come from.

# From Sync Code to CPS

Let’s expand our example to chain two operations:

{% highlight javascript %}
function double(x) {
  return x * 2;
}

function addOne(x) {
  return x + 1;
}

console.log(addOne(double(5))); // 11
{% endhighlight %}

Now in CPS:

{% highlight javascript %}
function doubleCPS(x, cont) {
  cont(x * 2);
}

function addOneCPS(x, cont) {
  cont(x + 1);
}

doubleCPS(5, (doubled) => {
  addOneCPS(doubled, (result) => {
    console.log(result); // 11
  });
});
{% endhighlight %}

We’ve restructured our program so that each step explicitly passes control to the next.

This may seem verbose, but it turns out to be extremely powerful — especially when dealing with asynchronous code.

# CPS is Everywhere in JavaScript

Let’s look at an example from the Node.js world:

{% highlight javascript %}
const fs = require("fs");

fs.readFile("data.txt", "utf8", (err, data) => {
  if (err) return console.error("Failed to read file:", err);
  processData(data, (result) => {
    console.log("Result:", result);
  });
});
{% endhighlight %}

This is literally CPS — instead of returning data, `fs.readFile` *passes it to a callback*.

What’s the callback? The continuation.

# Promises: CPS with a Better API

JavaScript promises are built around the same idea, just cleaner:

{% highlight javascript %}
fetch("/api/data")
  .then((res) => res.json())
  .then((data) => {
    console.log("Fetched:", data);
  })
  .catch((err) => {
    console.error("Error:", err);
  });
{% endhighlight %}

Every `.then(fn)` is passing the result to a new continuation function. But promises let us flatten the nesting and 
chain more cleanly.

# async/await: The Illusion of Sync

Now the same thing with `async/await`:

{% highlight javascript %}
async function loadData() {
  try {
    const res = await fetch("/api/data");
    const data = await res.json();
    console.log("Fetched:", data);
  } catch (err) {
    console.error("Error:", err);
  }
}
{% endhighlight %}

But this isn't "real" synchronous code — it just *looks* like it.

Behind the scenes, the JavaScript engine is:
- Splitting the function into pieces at each `await`
- Saving the continuation (the rest of the function) in a hidden state machine
- Resuming that continuation when the awaited promise resolves

That’s CPS at work.

# Manual CPS in Other Languages

You don’t need a JavaScript engine to try this out. Let’s look at Rust and Python examples to see how CPS can be 
expressed in ordinary code.

## Rust Example

{% highlight rust %}
fn double_cps(x: i32, cont: impl FnOnce(i32)) {
    cont(x * 2);
}

fn add_one_cps(x: i32, cont: impl FnOnce(i32)) {
    cont(x + 1);
}

fn main() {
    double_cps(5, |doubled| {
        add_one_cps(doubled, |result| {
            println!("Result: {}", result); // prints 11
        });
    });
}
{% endhighlight %}

Rust’s closures let us express continuations cleanly without needing async runtimes or macros.

## Python Example

The same example can be implemented using python pretty simply.

{% highlight python %}
def double_cps(x, cont):
    cont(x * 2)

def add_one_cps(x, cont):
    cont(x + 1)

double_cps(5, lambda doubled:
    add_one_cps(doubled, lambda result:
        print("Result:", result)
    )
)
{% endhighlight %}

Python’s lambdas work just like JavaScript’s arrow functions here. Every step is chained by explicitly passing the 
next operation as a continuation.

# async/await in Python: CPS in the Runtime

Just like JavaScript, Python’s `async def` and `await` are built on top of generators and continuations:

{% highlight python %}
import asyncio

async def get_data():
    await asyncio.sleep(1)
    return 42

async def main():
    value = await get_data()
    print("Got:", value)

asyncio.run(main())
{% endhighlight %}

Here, too, the interpreter:
- Splits your function at each `await`
- Stores the rest of the computation (continuation) to run later

# Why Learn CPS?

Once you can see continuations, you start to understand:
- How async runtimes work
- How exception handling works (dual continuations!)
- How interpreters implement tail calls and coroutines
- How to reason about control flow in state machines

## Bonus: Control Flow as a Tree

You can visualize your program’s control flow like a tree. Each continuation is a branch.

<div class="mermaid">
graph TD
    A[Start]
    A --> B[doubleCPS]
    B --> C[addOneCPS]
    C --> D[print]
</div>

When you `await`, you're pausing on one branch — and resuming it later.

## Conclusion

Continuations and CPS transform how we think about execution.

They explain:
- Why callbacks exist
- How `async/await` works under the hood
- How control flow can be captured, resumed, or redirected

You don’t need to write a compiler to use CPS — just pass the “rest of your program” as a function.

By making the invisible visible, continuations give us precise control over what our code *does next*.
