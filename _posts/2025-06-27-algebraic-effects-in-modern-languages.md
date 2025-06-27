---
layout: post
title: Algebraic Effects in Modern Languages
date: 2025-06-27
comments: false
categories: [ effects, programming, languages, compilers ]
---

# Introduction

Programming languages have long struggled with how to represent _side effects_ — actions like printing to the console, 
handling exceptions, or managing state. From exceptions to async/await, from monads to callbacks, the industry has 
iterated through many paradigms to isolate or compose effectful behavior.

But there’s a new player in town: **algebraic effects**. Once a theoretical construct discussed in type theory papers, 
they're now making their way into real-world languages like **Eff**, **Koka**, **Multicore OCaml**, and even Haskell 
(via libraries). This post dives into what algebraic effects are, why they matter, and how modern languages are 
putting them to work.

# The Problem With Traditional Control Flow

Most languages bake side effects deep into their semantics. Consider these examples:

- **Exceptions** break flow but are hard to compose.
- **Async/await** adds sugar but doesn't unify with other control patterns.
- **Monads** (in Haskell and friends) offer composability but can be verbose and hard to stack.

You often end up tightly coupling your program logic with the mechanism that implements side effects. For example, 
what if you want to switch how logging is done — or intercept all state mutations? In traditional paradigms, that 
typically requires invasive changes.

# Enter Algebraic Effects

**Algebraic effects** offer a clean abstraction: you _declare_ an operation like `Print` or `Throw`, and you _handle_ 
it separately from where it’s invoked. Think of them as **resumable exceptions** — but first-class and composable.

There are two parts:

1. **Effect operations** – like `Log("Hello")` or `Choose(1, 2)`
2. **Effect handlers** – define how to interpret or respond to those operations

Here's a conceptual example:

{% highlight plain %}
operation Log : String -> Unit

handler ConsoleLogger {
  handle Log(msg) => print(msg)
}

handle {
  Log("Hello")
  Log("World")
} with ConsoleLogger
{% endhighlight %}

The code *requests* the effect, and the handler *interprets* it. 

This separation makes effects modular and swappable.

# Under the Hood: Continuations and Handlers

To implement algebraic effects, a language usually relies on **delimited continuations** — the ability to capture
“the rest of the computation” when an effect is invoked, and then resume it later.

Think of it like pausing the program, giving control to a handler, and optionally continuing from where you left off.

Let’s break it down.

## What Happens at Runtime?

Suppose we run this (in a made-up language):

{% highlight plain %}
effect Log : String -> Unit

handle {
  Log("step 1")
  Log("step 2")
  Log("done")
} with ConsoleLogger
{% endhighlight %}

The runtime treats `Log("step 1")` as a _request_ rather than a built-in action. 

When it hits that line:

1. It **pauses** execution at the `Log` point.
2. It captures the **continuation** — i.e., everything that comes *after* the `Log("step 1")`.
3. It gives control to the `ConsoleLogger` handler.
4. The handler **decides what to do**:
   - Call `print("step 1")`
   - Resume the captured continuation to proceed to `Log("step 2")`

This "pause-and-resume" behavior is the key.

## Visualizing With a Continuation

Let’s walk through this with a simplified stack trace:

Before the first `Log("step 1")`:

{% highlight plain %}
handle {
  [Log("step 1"); Log("step 2"); Log("done")]
} with ConsoleLogger
{% endhighlight %}

When `Log("step 1")` is reached, the continuation is:

{% highlight plain %}
continuation = {
  Log("step 2")
  Log("done")
}
{% endhighlight %}

The handler receives the message `"step 1"` and the continuation. It can:

- Resume it once (like normal flow)
- Discard it (like throwing an exception)
- Resume it **multiple times** (like a forked computation)

## How This Explains Exceptions

Exceptions are a special case of algebraic effects — with no continuation.

{% include callout.html type="error" title="Throwing says . . " text="Stop here. Find a handler up the call stack. Don’t resume." %}

Let’s define a custom effect `Throw(msg)`:

{% highlight plain %}
effect Throw : String -> Never

handle {
  if error {
    Throw("bad input")
  }
  print("This will never run")
} with ExceptionHandler
{% endhighlight %}

In this case, the handler intercepts `Throw`, but **never resumes** the continuation. The program takes a different branch.

{% include callout.html type="info" title="💡 Remember" text="Effect handlers don't have to resume — they define the rules" %}

## How This Explains I/O

Now suppose we want to model an I/O operation:

{% highlight plain %}
effect ReadLine : Unit -> String

handle {
  let name = ReadLine()
  Log("Hi " + name)
} with {
  handle ReadLine() => "Alice"
  handle Log(msg) => print(msg)
}
{% endhighlight %}

Here, `ReadLine` is **not tied** to any global input stream. It’s an abstract operation that the handler chooses how to 
interpret — maybe it prompts the user, maybe it returns a mock value.

{% include callout.html type="info" title="🧪 Perfect for Testing" text="Handlers let you swap out real I/O with fake data. You don’t need to patch or stub anything — just handle the effect differently." %}

The continuation gets resumed with the string `"Alice"`, and proceeds to log `"Hi Alice"`.

## How This Explains Async/Await

Let’s look at an async-style effect: `Sleep(ms)`. We could simulate async behavior with handlers and continuations:

{% highlight plain %}
effect Sleep : Int -> Unit

handle {
  Log("Start")
  Sleep(1000)
  Log("End")
} with AsyncHandler
{% endhighlight %}

When the program hits `Sleep(1000)`, it:

1. Captures the continuation (`Log("End")`)
2. Asks the handler to delay for 1000 ms
3. When the delay completes, **the handler resumes the continuation**

So in an async-capable runtime, `Sleep` could enqueue the continuation in a task queue — very similar to `await`.

## Effect Flow

Let’s visualize the execution:

<div class="mermaid">
graph TD
    A[Program Starts] --> B[Perform Log]
    B --> C[Handler Receives Effect and Continuation]
    C --> D[Handler Prints Hello]
    D --> E[Handler Resumes Continuation]
    E --> F[Next Effect or End]
</div>

Each effect call yields control to its handler, which decides what to do and when to resume.

## Summary

Algebraic effects give you a way to _pause_ execution at key points and _delegate_ the decision to an external handler. This lets you:

- Model exceptions (`Throw` with no resume)
- Emulate async/await (`Sleep` with delayed resume)
- Intercept I/O or tracing (`Log`, `ReadLine`, etc.)
- Compose multiple effects together (logging + state + error handling)

The idea is powerful because **you capture just enough of the stack to resume** — not the whole program, not the whole thread — just a clean slice.

This is the beating heart of algebraic effects: **capturable, resumable, programmable control flow**.

# Examples Across Languages

Let’s look at how modern languages express algebraic effects.

## Eff (by Andrej Bauer)

Eff is a small experimental language built around effects.

{% highlight plain %}
effect Choose : (int * int) -> int

let choose_handler = handler {
  val x -> x
  | Choose(x, y) k -> k(x) + k(y)
}

with choose_handler handle {
  let result = Choose(1, 2)
  result * 10
}
{% endhighlight %}

This handler resumes the continuation twice — once with `1` and once with `2` — and adds the results. Very cool.

## Koka

Koka (by Daan Leijen at Microsoft) is a strongly typed language where every function explicitly declares its effects.

{% highlight plain %}
function divide(x: int, y: int) : exn int {
  if (y == 0) throw("divide by zero")
  else x / y
}
{% endhighlight %}

Koka tracks effects statically in the type system — you can see `exn` in the return type above.

## OCaml with Multicore Support

Multicore OCaml added support for effects using new syntax:

{% highlight plain %}
effect ReadLine : string

let read_input () = perform ReadLine

let handler = 
  match read_input () with
  | effect ReadLine k -> continue k "mocked input"
{% endhighlight %}

You can install handlers and intercept effects using pattern matching.

## Haskell (with `polysemy` or `freer-simple`)

Algebraic effects in Haskell are expressed via libraries.

{% highlight plain %}
data Log m a where
  LogMsg :: String -> Log m ()

runLogToIO :: Member IO r => Sem (Log ': r) a -> Sem r a
runLogToIO = interpret (\case
  LogMsg s -> sendM (putStrLn s))
{% endhighlight %}

These libraries emulate effects using GADTs and free monads under the hood, offering a composable way to layer side 
effects.

# Why Use Algebraic Effects?

- **Separation of concerns** – pure logic stays free from effect details
- **Composable** – you can layer state, logging, exceptions, etc.
- **Testable** – effects can be mocked or redirected
- **Flexible control flow** – resumable exceptions, nondeterminism, backtracking

They’re especially attractive for interpreters, DSLs, async runtimes, and functional backends.

# The Downsides

Of course, there are tradeoffs:

- **Runtime overhead** – stack capturing can be expensive
- **Complexity** – debugging and stack traces are harder
- **Still experimental** – limited tooling, especially in statically typed systems
- **Compiler support** – not many mainstream languages have full support

But the ideas are gaining traction, and you can expect to see more of them in new languages (and maybe in existing ones 
like JavaScript or Swift).

# The Future of Effects

Algebraic effects could fundamentally change how we write software:

- Async/await might become just an effect
- Logging, tracing, and observability could become pluggable
- Pure functions could request effects without being impure

This vision aligns with a long-standing dream in language design: _orthogonal, composable effects_ that don’t 
compromise reasoning.

## Wrapping Up

Algebraic effects are still a frontier — but a promising one. They offer a middle ground between pure functions and 
side-effect-laden imperative code. By letting you _request_ an effect and _handle_ it elsewhere, they make programs 
easier to test, modify, and reason about.

Whether you’re writing interpreters, backend services, or just experimenting with new paradigms, algebraic effects 
are well worth exploring. The future of control flow may be algebraic — and the best part is, it’s just getting started.
