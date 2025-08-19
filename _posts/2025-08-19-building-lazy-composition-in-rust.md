---
layout: post
title: Building Lazy Composition in Rust
date: 2025-08-19
comments: false
categories: [ "rust", "thunk", "combinator" ]
---

# Introduction

Rust programmers encounter combinators all the time: `map()`, `and_then()`, `filter()`. They're everywhere in 
`Option`, `Result`, `Iterator`, and of course, `Future`. But if you're coming from a functional programming 
background — or just curious how these things *work* — you might ask:

> What actually *is* a combinator?

Let’s strip it down to the bare minimum: a value, a function, and some deferred execution.

# A Lazy Computation

We’ll start with a structure called `Thunk`. It wraps a closure that does some work, and it defers that work until we 
explicitly ask for it via `.run()`.

{% highlight rust %}
pub struct Thunk<F> {
    f: Option<F>,
}

impl<F, R> Thunk<F>
where
    F: FnOnce() -> R,
{
    pub fn new(f: F) -> Self {
        Self { f: Some(f) }
    }

    pub fn run(mut self) -> R {
        let f = self.f.take().expect("already run");
        f()
    }
}
{% endhighlight %}

It’s essentially a one-shot deferred computation. We stash a closure inside, and we invoke it only when we're ready. 

Here, `F` is the type of the closure (the function) we’re wrapping, and `R` is the result it will produce once called. This lets 
`Thunk` be generic over any one-shot computation.

The work here is really wrapped up by `self.f.take()` which will force the value.

Simple.

## Example

Here's what this looks like in practice:

{% highlight rust %}
fn main() {
    let add_one = Thunk::new(|| 3 + 1);
    let result = add_one.run();
    println!("Result: {}", result); // prints 4
}
{% endhighlight %}

No magic. No threading. No async. Just a delayed function call.

# Composing Thunks

The real value in combinators is that they **compose**. We can make more complex computations out of simpler ones — 
without immediately evaluating them.

Here's how we can build on top of multiple `Thunk`s:

{% highlight rust %}
fn main() {
    let m = Thunk::new(|| 3 + 1); // 4
    let n = Thunk::new(|| 9 + 1); // 10

    let o = Thunk::new(|| m.run() + n.run()); // 14
    let result = o.run();

    println!("Result: {}", result);
}
{% endhighlight %}

We’ve built a new computation (`o`) that depends on two others (`m` and `n`). They won’t run until `o.run()` is 
called — and then they run in the correct order, and just once.

## Look Familiar?

If you've spent time in Haskell, this structure might look suspiciously familiar:

> `fmap :: Functor f => (a -> b) -> f a -> f b`

This is a form of `fmap`. We're not building a full trait implementation here, but the shape is the same. We can even 
imagine extending `Thunk` with a `map()` method:

{% highlight rust %}
impl<F, R> Thunk<F>
where
    F: FnOnce() -> R,
{
    pub fn map<G, S>(self, g: G) -> Thunk<impl FnOnce() -> S>
    where
        G: FnOnce(R) -> S,
    {
        Thunk::new(|| g(self.run()))
    }
}
{% endhighlight %}

And now:

{% highlight rust %}
let t = Thunk::new(|| 42);
let u = t.map(|x| x * 2);
assert_eq!(u.run(), 84);
{% endhighlight %}

No typeclasses, no lifetimes — just combinator building blocks.

# From Lazy to Async

Now here’s the twist. What if our `.run()` method couldn’t give us a value right away? What if it needed to register a 
waker, yield, and be polled later?

That’s exactly what happens in Rust’s async system. The structure is the same — a value and a function bundled 
together — but the execution context changes. Instead of calling `.run()`, we implement `Future` and respond to 
`.poll()`.

Here’s what that looks like for a simple async `Map` combinator:

{% highlight rust %}
use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll};
use pin_project::pin_project;

// Our Map combinator
#[pin_project]
pub struct Map<Fut, F> {
    #[pin]
    future: Fut,

    f: Option<F>, // Option to allow taking ownership in poll
}

impl<Fut, F> Map<Fut, F> {
    pub fn new(future: Fut, f: F) -> Self {
        Self { future, f: Some(f) }
    }
}

impl<Fut, F, T, U> Future for Map<Fut, F>
where
    Fut: Future<Output = T>,
    F: FnOnce(T) -> U,
{
    type Output = U;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let mut this = self.project();

        match this.future.poll(cx) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(val) => {
                let f = this.f.take().expect("polled Map after completion");
                Poll::Ready(f(val))
            }
        }
    }
}

// Helper function to use it ergonomically
pub fn map<Fut, F, T, U>(future: Fut, f: F) -> Map<Fut, F>
where
    Fut: Future<Output = T>,
    F: FnOnce(T) -> U,
{
    Map::new(future, f)
}
{% endhighlight %}

Let’s take a step back and notice something: this structure is almost identical to `Thunk`. We’re still storing a value 
(`future`) and a function (`f`), and the combinator (`Map`) still controls when that function is applied. The 
difference is that we now interact with the asynchronous task system via `poll()`, instead of calling `.run()` ourselves.

This is how `Future` combinators in futures and `tokio` work under the hood — by carefully pinning, polling, and 
composing smaller futures into larger ones.

This is essentially a hand-rolled version of what `futures::FutureExt::map()` gives you for free.

As a simple example, we can use this as follows:

{% highlight rust %}
#[tokio::main]
async fn main() {
    let fut = async { 21 };
    let mapped = map(fut, |x| x * 2);
    let result = mapped.await;
    println!("Result: {}", result); // Should print 42
}
{% endhighlight %}

# Conclusion

We often think of combinators as “just utility functions.” But they’re really more than that: they’re 
**a way of thinking**. Package a value and a transformation together. Delay the work. Compose more when you're ready.

So the next time you write `.map()`, remember — it’s just a `Thunk` waiting to happen.

