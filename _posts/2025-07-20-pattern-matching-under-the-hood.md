---
layout: post
title: Pattern Matching Under The Hood
date: 2025-07-20
comments: false
categories: [languages, compilation, pattern-matching]
---

Pattern matching is a powerful and expressive tool found in many modern languages. It enables concise branching based 
on the structure of data—a natural fit for functional and functional-style programming. But under the hood, not all 
pattern matching is created equal.

In this tutorial, we’ll explore how pattern matching works in three languages: **Rust**, **Haskell**, and **OCaml**. 

We’ll look at how it’s written, how it’s compiled, and how their differing philosophies impact both performance and 
expressiveness.

## What is Pattern Matching?

At its simplest, pattern matching allows a program to inspect and deconstruct data in a single, readable construct. 
Instead of chaining conditionals or nested `if let` statements, a match expression allows you to declare a structure 
and what to do with each shape of that structure.

Here’s a simple pattern match on a custom `Option` type in three languages:

### Rust

{% highlight rust %}
enum Option<T> {
    Some(T),
    None,
}

fn describe(opt: Option<i32>) -> &'static str {
    match opt {
        Some(0) => "zero",
        Some(_) => "non-zero",
        None => "nothing",
    }
}
{% endhighlight %}

### Haskell

{% highlight haskell %}
data Option a = Some a | None

describe :: Option Int -> String
describe (Some 0) = "zero"
describe (Some _) = "non-zero"
describe None     = "nothing"
{% endhighlight %}

### OCaml

{% highlight ocaml %}
type 'a option = Some of 'a | None

let describe = function
    | Some 0 -> "zero"
    | Some _ -> "non-zero"
    | None -> "nothing"
{% endhighlight %}

These look remarkably similar. All three match against the structure of the input value, and bind variables (`_`) to 
reuse them in later expressions. But how each language *executes* these match statements differs significantly.

## Compiling Simple Matches

Even with these trivial examples, each compiler approaches code generation differently.

### Rust

Rust generates a **decision tree** at compile time. The compiler ensures that all possible variants are covered and 
arranges branches efficiently. The tree checks discriminants of enums and can often compile to a jump table if the 
match is dense enough.

Crucially, **Rust’s matches must be exhaustive**. The compiler will throw an error if you leave out a case—this 
improves safety.

### Haskell

Haskell also builds decision trees, but the situation is complicated by **lazy evaluation**. Pattern matching in 
Haskell can introduce runtime thunks or failures if evaluation is deferred and a non-exhaustive pattern is forced later.

Haskell’s compiler (`GHC`) issues warnings for non-exhaustive patterns, but you can still write incomplete 
matches—leading to runtime errors.

### OCaml

OCaml compiles pattern matches to decision trees as well. Like Rust, OCaml enforces exhaustiveness checking and gives 
helpful compiler feedback. However, a non-exhaustive match is still allowed if you're okay with a `Match_failure` 
exception at runtime.

## Nested and Complex Patterns

Pattern matching really shines when dealing with recursive or nested structures. Let's explore a small binary tree type 
and how it's matched in each language.

### Example: Summing a Binary Tree

We'll define a binary tree of integers and write a function to sum its contents.

### Rust

{% highlight rust %}
enum Tree {
    Leaf(i32),
    Node(Box<Tree>, Box<Tree>),
}

fn sum(tree: &Tree) -> i32 {
    match tree {
        Tree::Leaf(n) => *n,
        Tree::Node(left, right) => sum(left) + sum(right),
    }
}
{% endhighlight %}

{% include callout.html type="info" title="Keep in mind!" text="Rust enforces match exhaustiveness at compile time. If you forget to handle a variant, the compiler will issue an error—this ensures total coverage and prevents runtime surprises." %}

### Haskell

{% highlight haskell %}
data Tree = Leaf Int | Node Tree Tree

sumTree :: Tree -> Int
sumTree (Leaf n)     = n
sumTree (Node l r) = sumTree l + sumTree r
{% endhighlight %}

### OCaml

{% highlight ocaml %}
type tree = Leaf of int | Node of tree * tree

let rec sum = function
    | Leaf n -> n
    | Node (l, r) -> sum l + sum r
{% endhighlight %}

### What's Happening Under the Hood?

- **Rust** compiles this match into a series of type-discriminant checks followed by destructuring and recursive calls. Thanks to `Box`, the heap allocations are clear and explicit.
- **Haskell** uses lazy evaluation. Pattern matching on a `Leaf` or `Node` may delay execution until the value is demanded—this can impact stack behavior or cause runtime pattern failures if a pattern is too strict.
- **OCaml** uses a decision tree again, with efficient memory representation for variants. Tail recursion may be optimized by the compiler, depending on structure.

## Or-Patterns and Guards

Another powerful feature is the ability to match multiple shapes with a single branch or apply a condition to a match.

### Rust: Or-Patterns and Guards

{% highlight rust %}
fn describe(n: i32) -> &'static str {
    match n {
        0 | 1 => "small",
        x if x < 10 => "medium",
        _ => "large",
    }
}
{% endhighlight %}

Rust allows or-patterns (`0 | 1`) and guard clauses (`if x < 10`). The compiler desugars these into conditional 
branches with runtime checks where needed.

### Haskell: Guards and Pattern Overlap

{% highlight haskell %}
describe :: Int -> String
describe n
    | n == 0 || n == 1 = "small"
    | n < 10 = "medium"
    | otherwise = "large"
{% endhighlight %}

Haskell separates pattern matching and guards, giving guard syntax its own block. Pattern matching and guards can 
interact, but not all combinations are possible (e.g., no or-patterns directly in a pattern match).

### OCaml: Or-Patterns and Guards

{% highlight ocaml %}
let describe = function
    | 0 | 1 -> "small"
    | x when x < 10 -> "medium"
    | _ -> "large"
{% endhighlight %}

OCaml supports both or-patterns and `when` guards, very similar to Rust. These are compiled into branches with 
explicit condition checks.

## Pattern Matching as a Compilation Strategy

At this point, it's clear that although syntax is similar, the languages diverge significantly in how patterns are 
interpreted and executed:

- **Rust** performs pattern checking and optimization at compile time with strict exhaustiveness.
- **Haskell** balances pattern evaluation with laziness, leading to different runtime behavior.
- **OCaml** focuses on expressive patterns and efficient compilation, with an option for partial matches.

## Desugaring and Compilation Internals

Pattern matching may look declarative, but under the hood, it's compiled down to a series of conditional branches, 
memory lookups, and control flow structures. Let's unpack what happens behind the scenes.

### Rust: Match Desugaring and Code Generation

Rust's `match` is exhaustively checked and compiled to a decision tree or jump table, depending on context. For enums 
like `Option` or `Result`, the compiler performs:

1. **Discriminant extraction** – Read the tag value stored in the enum.
2. **Branch selection** – Choose code based on the tag (e.g., `Some`, `None`).
3. **Destructuring** – Bind values as specified in the pattern.

For example, the match:

{% highlight rust %}
match opt {
    Some(x) if x > 10 => "large",
    Some(_) => "small",
    None => "none",
}
{% endhighlight %}

is compiled into a match tree:
- First, match on the enum tag.
- If `Some`, extract the value and check the guard.
- Fall through to next branch if guard fails.

The compiler avoids repeated guard checks and can inline branches aggressively. The borrow checker and ownership model 
also enforce safe destructuring.

### Haskell: Lazy Matching and Thunks

Haskell's pattern matching is governed by laziness. When a match is encountered, the value being matched may not yet 
be evaluated. This has consequences:

1. **Pattern matching may force evaluation** – e.g., matching `Just x` forces the outer constructor.
2. **Guards are checked in order** – evaluation is deferred until necessary.
3. **Non-exhaustive patterns fail at runtime** – Haskell compiles these into a fallback `error` or `incomplete pattern match`.

GHC desugars pattern matches into **case expressions**, and then optimizes these during Core-to-STG conversion. The 
use of strictness annotations or `BangPatterns` can influence when evaluation occurs.

{% include callout.html type="warning" title="Watch out!" text="In Haskell, non-exhaustive pattern matches may compile without errors but fail at runtime—especially when lazily evaluated expressions are forced later on." %}

### OCaml: Pattern Matrices and Decision Trees

OCaml's pattern matching is implemented via **pattern matrices**—a tabular representation where each row is a clause 
and each column is a pattern component. The compiler then constructs a decision tree based on:

- **Specificity** – More specific patterns are prioritized.
- **Order** – Clauses are matched in order written.
- **Exhaustiveness** – Checked at compile time with warnings for incomplete matches.

This allows OCaml to generate efficient code with minimal branching. The compiler may flatten nested patterns and 
inline small matches to avoid function call overhead.

For example:

{% highlight ocaml %}
match tree with
    | Leaf n when n < 0 -> "negative"
    | Leaf n -> "non-negative"
    | Node (_, _) -> "internal"
{% endhighlight %}

compiles to:
- Match the outer tag.
- For `Leaf`, bind `n` and test the guard.
- For `Node`, bind subtrees (discarded here).

### Common Patterns in Compilation

Despite differences, all three languages use similar compilation strategies:

- **Tag-dispatching** on variant constructors.
- **Destructuring** of values and recursive matching.
- **Decision trees** to minimize redundant checks.

Where they differ is in **evaluation strategy**, **error handling**, and **degree of compiler enforcement**.

- Rust: strict and eager, no runtime match failures.
- Haskell: lazy and permissive, with potential runtime errors.
- OCaml: eager, with optional runtime match failures (if unchecked).

Understanding these mechanisms can help you reason about performance, debugging, and maintainability—especially in 
performance-critical or safety-sensitive code.

## Performance Implications of Pattern Matching

Pattern matching isn't just about expressiveness—it's also about how efficiently your code runs. The compilation 
strategies we've seen have real consequences on performance, especially in tight loops or recursive data processing.

### Rust: Predictability and Optimization

Rust's eager evaluation and static analysis make it highly amenable to performance tuning:

- **Predictable branching** – Match arms can be compiled to jump tables or decision trees with minimal overhead.
- **Inlining and monomorphization** – Matches in generic code are monomorphized, allowing branch pruning and aggressive inlining.
- **No runtime overhead** – The compiler guarantees exhaustiveness, so there's no need for fallback match logic.

Because of Rust's focus on safety and zero-cost abstractions, pattern matching tends to compile into very efficient 
machine code—often indistinguishable from hand-written conditional logic.

**Performance Tip:** Prefer direct matching over nested `if let` chains when possible. The compiler optimizes `match` better.

### Haskell: Laziness and Thunks

In Haskell, performance depends not just on the match structure but also on **when** the value being matched is evaluated.

- **Laziness introduces indirection** – A pattern match may not actually evaluate the structure until needed.
- **Guards can delay failure** – Useful for modular logic, but may hide runtime errors.
- **Pattern match failures are costly** – Non-exhaustive patterns produce runtime exceptions, which can hurt reliability.

To improve performance:

- Use **`BangPatterns`** (`!`) or **strict data types** when you want eager evaluation.
- Be cautious with deeply nested matches that depend on lazily evaluated values.
- Profile with `-prof` to detect thunk buildup.

**Performance Tip:** Avoid unnecessary intermediate patterns or overly broad matches when working with large data structures.

### OCaml: Efficient Matching and Memory Use

OCaml benefits from an efficient memory layout for variants and predictable eager evaluation:

- **Tag-based matching is fast** – Patterns are compiled into compact branching code.
- **Pattern matrices optimize decision trees** – Redundant checks are minimized.
- **Partial matches incur runtime cost** – A `Match_failure` exception can be expensive and hard to debug.

Because OCaml has an optimizing native compiler (`ocamlopt`), well-structured matches can be nearly as fast as imperative conditionals.

**Performance Tip:** Make matches exhaustive or handle `Match_failure` explicitly, and avoid overly nested patterns without reason.

{% include callout.html type="info" title="Pro tip" text="Although OCaml performs exhaustiveness checking, it still allows incomplete matches if you accept the risk of a Match_failure exception at runtime. Consider enabling compiler warnings for safety." %}

### Comparing the Three

| Feature                  | Rust           | Haskell         | OCaml           |
|--------------------------|----------------|------------------|------------------|
| Evaluation strategy      | Eager          | Lazy             | Eager            |
| Exhaustiveness enforced  | Yes (always)   | No (warning only)| Yes (warning only) |
| Runtime match failure    | Impossible     | Possible         | Possible         |
| Match optimization       | Decision tree / Jump table | Decision tree w/ laziness | Pattern matrix → decision tree |
| Pattern ergonomics       | High           | Moderate         | High             |

Ultimately, **Rust** provides the most predictable and safe model, **Haskell** offers the most flexibility 
(with trade-offs), and **OCaml** strikes a balance with high-performance compilation and expressive syntax.

## Advanced Pattern Features

Beyond basic destructuring, modern languages introduce advanced pattern features that boost expressiveness and reduce 
boilerplate. Let's examine how Rust, Haskell, and OCaml extend pattern matching with power-user tools.

### Rust: Match Ergonomics and Binding Patterns

Rust takes care to make common patterns ergonomic while maintaining explicit control.

- **Match ergonomics** allow borrowing or moving values seamlessly. For instance:

{% highlight rust %}
match &opt {
    Some(val) => println!("Got: {}", val),
    None => println!("None"),
}
{% endhighlight %}

The compiler automatically dereferences `&opt` in this context.

- **Bindings with modifiers** like `ref`, `mut`, and `@` give fine-grained control:

{% highlight rust %}
match opt {
    Some(n @ 1..=10) => println!("small: {}", n),
    Some(n) => println!("other: {}", n),
    None => println!("none"),
}
{% endhighlight %}

- **Nested and conditional patterns** combine cleanly with guards and bindings, enabling expressive and safe matching on complex data.

### Haskell: View Patterns and Pattern Synonyms

Haskell’s type system supports powerful matching abstractions.

- **View patterns** allow you to pattern match against the *result* of a function:

{% highlight haskell %}
import Data.Char (isDigit)

f :: String -> String
f (view -> True) = "All digits"
f _ = "Something else"
    where view s = all isDigit s
{% endhighlight %}

This enables reusable abstractions over data representations.

- **Pattern synonyms** define reusable pattern constructs:

{% highlight haskell %}
pattern Zero <- (== 0) where
    Zero = 0

describe :: Int -> String
describe Zero = "zero"
describe _    = "non-zero"
{% endhighlight %}

- **Lazy patterns (~)** defer matching until values are needed, useful in infinite data structures or to avoid forcing evaluation prematurely.

### OCaml: Polymorphic Variants and Pattern Constraints

OCaml extends pattern matching with powerful type-level tools.

- **Polymorphic variants** allow open-ended variant types:

{% highlight ocaml %}
let rec eval = function
    | `Int n -> n
    | `Add (a, b) -> eval a + eval b
{% endhighlight %}

These enable modular and extensible match structures across modules.

- **Pattern guards** combine matching with runtime constraints:

{% highlight ocaml %}
let classify = function
    | n when n mod 2 = 0 -> "even"
    | _ -> "odd"
{% endhighlight %}

- **First-class modules** can also be unpacked with pattern matching, a feature unique among the three languages.

### Summary: Choosing the Right Tool

| Feature                 | Rust                    | Haskell                        | OCaml                      |
|-------------------------|--------------------------|---------------------------------|----------------------------|
| Ergonomic matching      | Yes (`ref`, `@`, auto-deref) | No (more explicit bindings)   | Yes (`when`, `or-patterns`) |
| Pattern synonyms        | No                      | Yes                             | No                         |
| View patterns           | No                      | Yes                             | Limited (via functions)    |
| Polymorphic variants    | No                      | No                              | Yes                        |
| Lazy pattern constructs | No                      | Yes (`~`, laziness by default) | No                         |

Each language extends pattern matching differently based on its design philosophy: Rust favors safety and ergonomics; 
Haskell favors abstraction and composability; OCaml favors flexibility and performance.

In our final section, we’ll wrap up with takeaways and guidance on how to use pattern matching effectively and safely 
across these languages.

## Conclusion: Patterns in Perspective

Pattern matching is more than syntactic sugar—it's a gateway into a language's core philosophy. From how values are 
represented, to how control flow is expressed, to how performance is tuned, pattern matching reflects a language’s 
trade-offs between power, safety, and clarity.

**Rust** emphasizes predictability and zero-cost abstractions. Pattern matching is strict, exhaustive, and optimized 
aggressively at compile time. You trade a bit of verbosity for guarantees about correctness and performance.

**Haskell** prioritizes abstraction and composability. Pattern matching fits elegantly into its lazy, pure model, but 
demands care: non-exhaustive matches and evaluation order can lead to surprises if you're not vigilant.

**OCaml** blends efficiency and expressiveness. Its pattern matrix compilation strategy and polymorphic variants 
enable succinct yet powerful constructs, backed by a mature native-code compiler.

When working with pattern matching:

- Think not just about syntax, but about **evaluation**—when and how values are computed.
- Use **exhaustive matches** wherever possible, even in languages where they’re not enforced.
- Consider the **performance implications** of deep nesting, guards, or lazy evaluation.
- Leverage each language’s **advanced features** to reduce boilerplate without sacrificing clarity.

Ultimately, understanding what happens *under the hood* makes you a better engineer—able to write code that's not 
only elegant, but also robust and efficient.

