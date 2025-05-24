---
layout: post
title: Building a Stack-Based VM in Rust – Part 2
date: 2025-05-24
comments: false
categories: [ rust, vm, forth, systems ]
---

# Introduction

In [Part 1]({% post_url 2025-05-22-building-a-stack-based-vm-in-rust-part-1 %}), we built the foundation of a 
Forth-inspired stack-based virtual machine in Rust. It could execute arithmetic expressions using a simple data stack, 
with support for operations like `PUSH`, `ADD`, `MUL`, and basic stack manipulation like `DUP`, `DROP`, and `SWAP`.

In this post, we’re going to extend our instruction set with **a broader set of stack manipulation words**, modeled 
after standard Forth operations.

Why focus on stack operations? Because in a language like Forth, **the stack is everything**. Understanding and 
manipulating it precisely is key to building complex programs — without variables, parentheses, or traditional 
control structures.

# Stack Operations in Forth

Let’s take a look at some of the classic stack words used in Forth and what they do:

| Word     | Stack Effect           | Description |
|----------|------------------------|-------------|
| `OVER`   | `( a b -- a b a )`     | Copies the second value to the top |
| `ROT`    | `( a b c -- b c a )`   | Rotates the third value to the top |
| `NIP`    | `( a b -- b )`         | Removes the second item |
| `TUCK`   | `( a b -- b a b )`     | Duplicates the top item under the second |
| `2DUP`   | `( a b -- a b a b )`   | Duplicates the top two items |
| `2DROP`  | `( a b -- )`           | Drops the top two items |
| `2SWAP`  | `( a b c d -- c d a b )` | Swaps the top two pairs |
| `DEPTH`  | `( -- n )`             | Pushes the current stack depth |

These tiny instructions are the building blocks for everything from loops and conditionals to data structures and 
control flow. Let’s implement them.

# Extending the Instruction Set

First, we add new variants to our `Instruction` enum:

{% highlight rust %}
enum Instruction {
    Push(i32),
    Add,
    Mul,
    Dup,
    Drop,
    Swap,
    Over,
    Rot,
    Nip,
    Tuck,
    TwoDup,
    TwoDrop,
    TwoSwap,
    Depth,
    Halt,
}
{% endhighlight %}

# Implementing the New Instructions

Each of these stack operations is implemented as a new `match` arm in our `run()` method. Here’s the complete method 
with all new instructions included:

## OVER

Copies the second value from the top and pushes it to the top.

**Stack effect**: `( a b -- a b a )`

{% highlight rust %}
Instruction::Over => {
    if self.stack.len() < 2 {
        panic!("Stack underflow on OVER");
    }
    let val = self.stack[self.stack.len() - 2];
    self.stack.push(val);
}
{% endhighlight %}

This implementation uses indexing to read the second-to-top value without popping. It's a clean operation that doesn’t disturb the existing stack order — a very common primitive in Forth.

## ROT

Rotates the third item to the top of the stack.

**Stack effect**: `( a b c -- b c a )`

{% highlight rust %}
Instruction::Rot => {
    if self.stack.len() < 3 {
        panic!("Stack underflow on ROT");
    }
    let c = self.stack.pop().unwrap();
    let b = self.stack.pop().unwrap();
    let a = self.stack.pop().unwrap();
    self.stack.push(b);
    self.stack.push(c);
    self.stack.push(a);
}
{% endhighlight %}

We pop all three values, then push them back in rotated order. It’s a destructive operation — it reshuffles the top 3 items completely.

## NIP

Removes the second item, leaving the top item alone.

**Stack effect**: `( a b -- b )`

{% highlight rust %}
Instruction::Nip => {
    if self.stack.len() < 2 {
        panic!("Stack underflow on NIP");
    }
    let top = self.stack.pop().unwrap();
    self.stack.pop(); // discard second
    self.stack.push(top);
}
{% endhighlight %}

Here we temporarily save the top, discard the second, then restore the top. This is essentially “keep the top, ignore the rest.”

## TUCK

Duplicates the top item and inserts it beneath the second.

**Stack effect**: `( a b -- b a b )`

{% highlight rust %}
Instruction::Tuck => {
    if self.stack.len() < 2 {
        panic!("Stack underflow on TUCK");
    }
    let top = *self.stack.last().unwrap();
    self.stack.insert(self.stack.len() - 2, top);
}
{% endhighlight %}

We avoid popping by using `last()` and `insert()`. Inserting at `len() - 2` puts the copy just beneath the second item, preserving the original order.

## 2DUP

Duplicates the top two stack items.

**Stack effect**: `( a b -- a b a b )`

{% highlight rust %}
Instruction::TwoDup => {
    if self.stack.len() < 2 {
        panic!("Stack underflow on 2DUP");
    }
    let len = self.stack.len();
    self.stack.push(self.stack[len - 2]);
    self.stack.push(self.stack[len - 1]);
}
{% endhighlight %}

We peek at the last two items and push duplicates in-place. It’s a straightforward double copy.

## 2DROP

Removes the top two items from the stack.

**Stack effect**: `( a b -- )`

{% highlight rust %}
Instruction::TwoDrop => {
    if self.stack.len() < 2 {
        panic!("Stack underflow on 2DROP");
    }
    self.stack.pop();
    self.stack.pop();
}
{% endhighlight %}

Just two pops in a row. Very simple and direct.

## 2SWAP

Swaps the top two pairs on the stack.

**Stack effect**: `( a b c d -- c d a b )`

{% highlight rust %}
Instruction::TwoSwap => {
    if self.stack.len() < 4 {
        panic!("Stack underflow on 2SWAP");
    }
    let d = self.stack.pop().unwrap();
    let c = self.stack.pop().unwrap();
    let b = self.stack.pop().unwrap();
    let a = self.stack.pop().unwrap();
    self.stack.push(c);
    self.stack.push(d);
    self.stack.push(a);
    self.stack.push(b);
}
{% endhighlight %}

This is the most complex so far. We destructure two pairs from the stack, then push them back in swapped order.

## DEPTH

Pushes the number of elements currently on the stack.

**Stack effect**: `( -- n )`

{% highlight rust %}
Instruction::Depth => {
    let depth = self.stack.len() as i32;
    self.stack.push(depth);
}
{% endhighlight %}

No stack input required. Just measure and push. Very handy for introspection or debugging.

# Example: Forth-ish Stack Dance

Let’s build a small program using some of these new instructions:

{% highlight rust %}
let program = vec![
    Instruction::Push(1),
    Instruction::Push(2),
    Instruction::Push(3),
    Instruction::Rot,      // [2, 3, 1]
    Instruction::Over,     // [2, 3, 1, 3]
    Instruction::Add,      // [2, 3, 4]
    Instruction::TwoDup,   // [2, 3, 4, 3, 4]
    Instruction::Swap,     // [2, 3, 4, 4, 3]
    Instruction::TwoDrop,  // [2, 3, 4]
    Instruction::Depth,    // [2, 3, 4, 3]
    Instruction::Halt,
];

let mut vm = VM::new(program);
vm.run();
println!("Final stack: {:?}", vm.stack);
{% endhighlight %}

The final stack should look like this:

{% highlight text %}
[2, 3, 4, 3]
{% endhighlight %}

That last `3` is the result of `DEPTH`, reporting how many values were on the stack before it was called.

# Conclusion

With just a few additional instructions, our little VM has become much more expressive. We've added powerful new tools 
to inspect, duplicate, and reorder values on the stack — just like a real Forth environment.

This kind of “stack choreography” might feel alien at first, but it's deeply intuitive once you start thinking in 
terms of data flow. It’s the perfect foundation for:

* Building control structures
* Defining new words
* Supporting conditionals and loops
* Creating a REPL

And that's where we're headed next.

The code for this part is available [up in my github](https://github.com/tuttlem/tiny_forth/tree/part2).