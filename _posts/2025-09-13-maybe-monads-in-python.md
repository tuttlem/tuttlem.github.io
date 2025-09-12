---
layout: post
title: Maybe Monads in Python
date: 2025-09-13
comments: false
categories: [ "" ]
---

# Introduction

If you’ve spent any time in Haskell or FP circles, you’ll have run into the terms **Functor**, **Applicative**, and 
**Monad**. They can sound mysterious, but at their core they’re just **design patterns for sequencing computations**.

Python isn’t a purely functional language, but we can still capture these ideas in code. In this post, we’ll build a 
full `Maybe` type in Python: a safe container that represents either a value (`Some`) or no value (`Nothing`). We’ll 
compare it with the Haskell version along the way.

A full runnable demo of the code presented here is available as [a gist up on GitHub](https://gist.github.com/tuttlem/39bd51e7560dc4523ae69b1dcaefa6b1).

# Maybe

We start of with our **box** or **context**. In our case today, we _might_ have a value in the box (`Some`) or the box 
maybe empty (`Nothing`). Because both of these are derivatives of the same thing, we create a base class of `Maybe`.

{% highlight python %}
class Maybe(Generic[T]):
    @staticmethod
    def some(value: T) -> "Maybe[T]":
        return Some(value)

    @staticmethod
    def nothing() -> "Maybe[T]":
        return NOTHING  # singleton

    @staticmethod
    def from_nullable(value: Optional[T]) -> "Maybe[T]":
        return Nothing() if value is None else Some(value)

    @staticmethod
    def from_predicate(value: T, predicate: Callable[[T], bool]) -> "Maybe[T]":
        return Some(value) if predicate(value) else Nothing()
{% endhighlight %}

Now we need our derivatives:

{% highlight python %}
@dataclass(frozen=True)
class Some(Maybe[T]):
    value: T

class Nothing(Maybe[Any]):
{% endhighlight %}

Our `Maybe` class here defines all of the operations we want to be able to perform on this datatype, but does not 
implement any of them; leaving the implementation to be filled in my the derivatives. You can expect the implementations 
between these derived classes to be quite different to each other.

We should end up with something like this:

<div class="mermaid">
classDiagram
    class Maybe {
        +map(f)
        +ap(mb)
        +bind(f)
    }
    class Some {
        +value: T
    }
    class Nothing
    Maybe <|-- Some
    Maybe <|-- Nothing
</div>


# Functor: Mapping over values

A **Functor** is anything you can `map` a function over. In Haskell, the generic `Functor` version of this is called 
`fmap`:

{% highlight haskell %}
fmap (+1) (Just 10)   -- Just 11
fmap (+1) Nothing     -- Nothing
{% endhighlight %}

The flow of values through `map` (or `fmap`) looks like this:

<div class="mermaid">
flowchart LR
    A[Some 10] -->|map +1| B[Some 11]
    C[Nothing] -->|map +1| D[Nothing]
</div>

For our Python implementation, we implement map like this

{% highlight python %}
# "Some" implementation
def map(self, f: Callable[[T], U]) -> "Maybe[U]":
    try:
        return Some(f(self.value))
    except Exception:
        return Nothing()

# "Nothing" implementation
def map(self, f: Callable[[Any], U]) -> "Maybe[U]":
    return self
{% endhighlight %}

We can now implement the example using these functions:

{% highlight python %}
Some(10).map(lambda x: x + 1)   # Some(11)
Nothing().map(lambda x: x + 1)  # Nothing()
{% endhighlight %}

The idea: if there’s a value inside, apply the function. If not, do nothing.

# Applicative: Combining values

Applicatives let us apply *functions that are also inside the box*. In Haskell, this is the `<*>` operator:

{% highlight haskell %}
pure (+) <*> Just 2 <*> Just 40   -- Just 42
{% endhighlight %}

Here we’re applying a function wrapped in `Some` to a value wrapped in `Some`. If either side is `Nothing`, the result 
is `Nothing`.

<div class="mermaid">
flowchart LR
    F[Some f] -->|ap| V[Some 2]
    V --> R[Some f2]

    FN[Some f] -->|ap| N[Nothing]
    N --> RN[Nothing]

    N2[Nothing] -->|ap| V2[Some 2]
    V2 --> RN2[Nothing]
</div>


For our Python implementation, we’ll call this `ap`.  

The `Some` implementation takes a function out of one box, and applies it to the value inside another box:  

{% highlight python %}
def ap(self: "Maybe[Callable[[T], U]]", mb: "Maybe[T]") -> "Maybe[U]":
    func = self.value
    return mb.map(func)
{% endhighlight %}

The `Nothing` implementation just returns itself:  

{% highlight python %}
def ap(self: "Maybe[Callable[[Any], U]]", mb: "Maybe[Any]") -> "Maybe[U]":
    return self
{% endhighlight %}

This lets us combine multiple values when both boxes are full:  

{% highlight python %}
add = lambda x, y: x + y
Some(add).ap(Some(2)).ap(Some(40))   # Some(42)
Some(add).ap(Some(2)).ap(Nothing())  # Nothing()
{% endhighlight %}

# Monad: Sequencing computations

A Monad takes things further: it lets us *chain together* computations that themselves return a `Maybe`.

In Haskell, this is the `>>=` operator (bind):  

{% highlight haskell %}
halfIfEven :: Int -> Maybe Int
halfIfEven x = if even x then Just (x `div` 2) else Nothing

Just 10 >>= halfIfEven    -- Just 5
Just 3  >>= halfIfEven    -- Nothing
{% endhighlight %}

Here we’re chaining a computation that itself returns a `Maybe`. If the starting point is `Nothing`, or if the 
function returns Nothing, the whole chain collapses.

<div class="mermaid">
flowchart LR
    S[Some x] --bind f--> FOUT[Some y]
    S --bind g--> GOUT[Nothing]

    N[Nothing] --bind f--> NRES[Nothing]
</div>


In Python we implement `bind`:  

{% highlight python %}
# "Some" implementation
def bind(self, f: Callable[[T], Maybe[U]]) -> "Maybe[U]":
    try:
        return f(self.value)
    except Exception:
        return Nothing()

# "Nothing" implementation
def bind(self, f: Callable[[Any], Maybe[U]]) -> "Maybe[U]":
    return self
{% endhighlight %}

And use it like this:  

{% highlight python %}
def half_if_even(x: int) -> Maybe[int]:
    return Some(x // 2) if x % 2 == 0 else Nothing()

Some(10).bind(half_if_even)   # Some(5)
Some(3).bind(half_if_even)    # Nothing()
{% endhighlight %}

Notice how the “empty box” propagates: if at any point we hit `Nothing`, the rest of the chain is skipped.

You'll also see a common pattern emerging with all of the implementations for `Nothing`. There's no computation. It's 
simply just returning itself. As soon as you hit `Nothing`, you're short-circuited to nothing.

# Do Notation (Syntactic Sugar)

Haskell makes monadic code look imperative with **do notation**:  

{% highlight haskell %}
do
  a <- Just 4
  b <- halfIfEven a
  return (a + b)
{% endhighlight %}

In Python, we can approximate this style using a generator-based decorator. Each `yield` unwraps a `Maybe`, and the 
whole computation short-circuits if we ever see `Nothing`.  

{% highlight python %}
@maybe_do
def pipeline(start: int):
    a = yield Some(start + 1)
    b = yield half_if_even(a)
    c = yield Maybe.from_predicate(b + 3, lambda n: n > 4)
    return a + b + c

print(pipeline(3))  # Some(11)
print(pipeline(1))  # Nothing()
{% endhighlight %}

This isn’t strictly necessary, but it makes larger chains of monadic code read like straight-line Python.

# Wrapping Up

By porting `Maybe` into Python and implementing `map`, `ap`, and `bind`, we’ve seen how Functors, Applicatives, and 
Monads aren’t magic at all — just structured patterns for working with values in context.  

- **Functor**: apply a function inside the box.  
- **Applicative**: apply a function that’s also in a box.  
- **Monad**: chain computations that each return a box.  

Haskell bakes these ideas into the language; in Python, we can experiment with them explicitly. The result is safer, 
more composable code — and maybe even a little functional fun.  
