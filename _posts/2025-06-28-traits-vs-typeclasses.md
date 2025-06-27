---
layout: post
title: Traits vs Typeclasses - A Deep Comparison
date: 2025-06-28
comments: false
categories: [ rust, haskell, polymorphism, type-system ]
---

# Introduction

If you’ve spent time in both Rust and Haskell, you’ve likely noticed that **traits** and **typeclasses** seem eerily 
similar. In fact, many people describe Rust traits as “typeclasses in disguise.”

But that’s only the beginning.

While traits and typeclasses both offer *ad-hoc polymorphism* — enabling different types to share behavior — the 
*details* around coherence, inference, dispatch, extensibility, and even type-level programming are very different.

In this post, we’ll dig into the core similarities and differences, and walk through side-by-side examples that 
highlight the strengths (and limitations) of both.

# What Are We Talking About?

Let’s start with some basic definitions:

- A **trait** in Rust defines a set of methods or behavior that types can implement.
- A **typeclass** in Haskell defines a set of functions that a type must implement to be considered part of that class.

At a glance, they look almost identical:

{% highlight rust %}
trait Printable {
    fn print(&self);
}
{% endhighlight %}

{% highlight haskell %}
class Printable a where
    print :: a -> IO ()
{% endhighlight %}

## Implementation: Explicit vs Global

In Rust, you explicitly implement traits per type:

{% highlight rust %}
impl Printable for i32 {
    fn print(&self) {
        println!("{}", self);
    }
}
{% endhighlight %}

In Haskell, typeclass instances are *global*:

{% highlight haskell %}
instance Printable Int where
    print x = putStrLn (show x)
{% endhighlight %}

This is one of the first major differences:

- **Rust**: Orphan rules prevent impls unless either the trait or type is defined locally.
- **Haskell**: Instances are globally coherent — there can only be one per type.

## Dispatch: Static vs Dynamic

Rust allows both **static** and **dynamic dispatch**:

{% highlight rust %}
// Static dispatch (monomorphized at compile time)
fn debug<T: Printable>(x: T) {
    x.print();
}

// Dynamic dispatch via trait objects
fn debug_dyn(x: &dyn Printable) {
    x.print();
}
{% endhighlight %}

Haskell only performs **static dispatch**, inserting a *dictionary* (a record of function pointers) at compile time:

{% highlight haskell %}
debug :: Printable a => a -> IO ()
debug x = print x
{% endhighlight %}

There is *no runtime polymorphism* in the sense of trait objects in Haskell.

## Type Inference

In Haskell, type inference is rich and automatic:

{% highlight haskell %}
addOne :: Num a => a -> a
addOne x = x + 1
{% endhighlight %}

Haskell will infer the constraint `Num a` based on the use of `+`.

In Rust, type annotations are often required — especially in generic code:

{% highlight rust %}
fn add_one<T: std::ops::Add<Output = T>>(x: T) -> T {
    x + x
}
{% endhighlight %}

Rust tends to prefer **explicitness**, while Haskell leans into **inference**.

## Higher-Kinded Types

Here’s where the two really diverge.

Haskell supports **higher-kinded types**, enabling expressive abstractions like `Functor`, `Applicative`, and `Monad`:

{% highlight haskell %}
class Functor f where
    fmap :: (a -> b) -> f a -> f b
{% endhighlight %}

Rust doesn’t currently support higher-kinded types (HKT), though you can simulate some of this with associated types, 
macros, or GATs (generic associated types).

This limitation makes certain patterns in Rust more awkward — or outright impossible — compared to Haskell.

## Overlapping and Flexible Instances

Haskell allows overlapping and multi-parameter instances (with extensions):

{% highlight haskell %}
class Convert a b where
    convert :: a -> b
{% endhighlight %}

Rust has no support for overlapping impls. Every `impl` must be unambiguous, and Rust’s coherence rules 
(the "orphan rule") enforce this at compile time.

## Trait Objects vs Typeclass Dictionaries

Here’s a behind-the-scenes peek:

- Rust: `&dyn Trait` compiles to a pointer + vtable.
- Haskell: `T :: C a => ...` becomes an implicit *dictionary* passed around — just like a trait object, but known at compile time.

This makes Haskell’s typeclass dispatch fully zero-cost — but not as flexible at runtime.

## Example: A Shared Interface

Let’s implement a toy `AddOne` behavior in both:

**Rust:**

{% highlight rust %}
trait AddOne {
    fn add_one(&self) -> Self;
}

impl AddOne for i32 {
    fn add_one(&self) -> Self {
        self + 1
    }
}
{% endhighlight %}

**Haskell:**

{% highlight haskell %}
class AddOne a where
    addOne :: a -> a

instance AddOne Int where
    addOne x = x + 1
{% endhighlight %}

Nearly identical — but the differences we’ve seen so far affect **how** you use these abstractions in larger codebases.

## So, Which Is Better?

That depends on what you value:

| Feature                       | Rust Traits | Haskell Typeclasses |
|------------------------------|--------------|----------------------|
| Explicit control             | Yes         | Partial             |
| Higher-kinded types          | Not yet     | Core feature        |
| Inference                    | Sometimes   | Strong              |
| Localized coherence          | Yes         | Global-only         |
| Overlapping instances        | No          | With extensions     |
| Runtime polymorphism         | Via `dyn`   | Not supported       |

# Final Thoughts

Rust’s trait system is heavily influenced by Haskell’s typeclasses, but it trades some flexibility for stronger 
guarantees around coherence, locality, and performance. If you want maximum abstraction power, Haskell wins. If you 
want performance, predictability, and control — Rust is often a better fit.

Both systems are brilliant in their own way — and understanding both gives you a deeper insight into how powerful 
type systems can unlock both correctness *and* expressiveness.

