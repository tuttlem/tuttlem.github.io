---
layout: post
title: Building a Stack-Based VM in Rust - Part 4
date: 2025-05-25
comments: false
categories: [ rust, vm, forth, systems ]
---

# Introduction

In [Part 3]({% post_url 2025-05-25-building-a-stack-based-vm-in-rust-part-3 %}), we introduced **control flow** and 
**subroutines** into our virtual machine. That gave us branching logic and reusable code blocks — a huge step forward.

But one core Forth idea is still missing: the ability to **define and name new words**.

In this part, we’ll add a dictionary to our VM and support calling reusable routines by name. This will allow us to 
define Forth-style words like:

{% highlight forth %}
: square dup * ;
5 square
{% endhighlight %}

Let’s get into it.

# The Concept of a "Word"

In Forth, a *word* is any named function — even built-ins like `+` and `*` are just words. User-defined words are 
created using `:` and `;`, and then they behave just like native instructions.

To support this, we need:

- A **dictionary** mapping word names to addresses
- An instruction that can call a word by name
- A way to define new words at specific locations in the program

# Extending the Instruction Set

First, we extend our enum to support calling named words:

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
    Jump(isize),
    IfZero(isize),
    Call(usize),
    CallWord(String),     // new
    Return,
    Halt,
}
{% endhighlight %}

The new `CallWord(String)` variant allows us to write programs that reference named words directly.

# Adding a Dictionary

Next, we update our `VM` structure to store a dictionary:

{% highlight rust %}
use std::collections::HashMap;

struct VM {
    stack: Vec<i32>,
    program: Vec<Instruction>,
    ip: usize,
    return_stack: Vec<usize>,
    dictionary: HashMap<String, usize>,          // new
}
{% endhighlight %}

And initialize it in `VM::new()`:

{% highlight rust %}
impl VM {
    fn new(program: Vec<Instruction>) -> Self {
        Self {
            stack: Vec::new(),
            program,
            ip: 0,
            return_stack: Vec::new(),
            dictionary: HashMap::new(),         // new
        }
    }
}
{% endhighlight %}

# Adding New Words

We create a helper method to register a word at a specific address:

{% highlight rust %}
impl VM {
    fn add_word(&mut self, name: &str, address: usize) {
        self.dictionary.insert(name.to_string(), address);
    }
}
{% endhighlight %}

This lets us register any block of code under a name.

# Calling Named Words

Now we implement the `CallWord` instruction in our dispatch loop:

{% highlight rust %}
Instruction::CallWord(name) => {
    let addr = self.dictionary.get(name)
        .expect(&format!("Unknown word: {}", name));
    self.return_stack.push(self.ip + 1);
    self.ip = *addr;
    continue;
}
{% endhighlight %}

This works just like `Call`, but performs a dictionary lookup first.

# Example: Defining `square`

Here’s a complete program that defines and calls a `square` word:

{% highlight rust %}
let program = vec![
    Instruction::Push(5),
    Instruction::CallWord("square".to_string()),
    Instruction::Halt,

    // : square dup * ;
    Instruction::Dup,
    Instruction::Mul,
    Instruction::Return,
];

let mut vm = VM::new(program);
vm.add_word("square", 3); // definition starts at index 3
vm.run();

println!("Final stack: {:?}", vm.stack);
{% endhighlight %}

Output:

{% highlight text %}
[25]
{% endhighlight %}

We’ve now made it possible to extend the language from within the language — a hallmark of Forth.

# Optional: Parsing `: square dup * ;`

Currently we define words manually by inserting them into the dictionary, but in true Forth style we’d like to write:

{% highlight forth %}
: square dup * ;
5 square
{% endhighlight %}

To support that, we’ll need a minimal parser or macro-assembler to convert high-level Forth code into VM instructions. 
This will be the focus of a future post.

# Conclusion

In this post, we gave our VM the ability to define and call **named words**, which turns our stack machine into 
something far more expressive and composable.

Our VM now supports:

- Arithmetic
- Stack manipulation
- Control flow and subroutines
- A dictionary of named routines

In **Part 5**, we’ll push even further — implementing a **simple parser** that can read actual Forth-like text, 
resolve words, and build programs dynamically.

We’re getting very close to having a minimal, working Forth interpreter — and it’s all built in Rust.

The code for this part is available [here on GitHub](https://github.com/tuttlem/tiny_forth/tree/part4)
