---
layout: post
title: Building a Stack-Based VM in Rust - Part 1
date: 2025-05-22
comments: false
categories: [ rust, vm, forth, systems ]
---


# Introduction

Most of the code we write is eventually executed by some kind of virtual machine — whether it's the 
[JVM](https://en.wikipedia.org/wiki/Java_virtual_machine), the [CLR](https://en.wikipedia.org/wiki/Common_Language_Runtime), 
or the many interpreters embedded in your browser or shell. 

But how do these machines actually *work*?

To understand this from the ground up, we’re going to build a **stack-based virtual machine** — the simplest kind of VM 
there is.

## Stack Machines and Reverse Polish Notation

Unlike register-based architectures (like [x86](https://en.wikipedia.org/wiki/X86) or [ARM](https://en.wikipedia.org/wiki/ARM_architecture_family)), 
**stack-based machines** use a single stack for passing arguments and storing temporary values. Instructions operate by 
**pushing** and **popping** values to and from this stack.

For example, instead of writing:

```
x = (2 + 3) * 4
```

You would write it in [Reverse Polish Notation (RPN)](https://en.wikipedia.org/wiki/Reverse_Polish_notation):

```
2 3 + 4 *
```

This evaluates like so:

1. Push `2`
2. Push `3`
3. Add them (`5`)
4. Push `4`
5. Multiply (`5 * 4 = 20`)

This is not just a novelty — it's how many early languages and calculators (like HP RPN calculators) worked. It 
eliminates the need for parentheses and operator precedence, making parsing trivial.

## Enter Forth

[Forth](https://en.wikipedia.org/wiki/Forth_(programming_language)) is a language built entirely on this stack-based model. It's terse, powerful, and famously minimalist. Every 
Forth program is a sequence of *words* (commands) that manipulate the data stack. New words can be defined at runtime, 
giving Forth a unique mix of interactivity and extensibility.

Despite being decades old, the design of Forth still holds up as a brilliant way to think about interpreters, minimal 
systems, and direct computing.

Here’s an example of a simple Forth snippet:

```
: square ( n -- n^2 ) dup * ;
5 square
```

This defines a word `square` that duplicates the top of the stack and multiplies it by itself. Then it pushes `5` and 
runs `square`, leaving `25` on the stack.

## Why Rust?

Rust gives us a perfect platform for building this kind of system:

- It’s **low-level enough** to model memory and data structures precisely.
- It’s **safe and expressive**, letting us move fast without segmentation faults.
- It encourages **clean architecture** and high-performance design.

Over the next few posts, we’ll build a small but functional **Forth-inspired virtual machine** in Rust. In this first part, we’ll get a simple instruction set up and running — enough to perform arithmetic with a data stack.

Let’s get started.

# Defining a Machine

Let’s start by defining the fundamental pieces of our stack-based virtual machine.

Our machine is going to be made up of some basic building blocks such as:

* An instruction set (things to execute)
* A stack (to hold our state)
* A machine structure (something to bundle our pieces together)

## The Instruction Set

First, we need a basic set of **instructions**. These represent the operations our VM knows how to perform. We'll keep 
it simple to begin with:

- `Push(n)` – Push a number onto the stack
- `Add` – Pop two values, push their sum
- `Mul` – Pop two values, push their product
- `Dup` – Duplicate the top value on the stack
- `Drop` – Discard the top value
- `Swap` – Swap the top two values
- `Halt` – Stop execution

We express these as a Rust enum:

{% highlight rust %}
#[derive(Debug)]
enum Instruction {
    Push(i32),
    Add,
    Mul,
    Dup,
    Drop,
    Swap,
    Halt,
}
{% endhighlight %}

That's the start of what our machine will be capable of executing. As we move through this series, this enum will 
gather more and more complex operations that we can execute. For now though, these basic arithmetic operations will
be a good start.

## The Machine

Now let’s define the structure of the virtual machine itself. Our `VM` will contain:

- A **stack** (`Vec<i32>`) for evaluating instructions
- A **program** (`Vec<Instruction>`) which is just a list of instructions to run
- An **instruction pointer** (`ip`) to keep track of where we are in the program

{% highlight rust %}
#[derive(Debug)]
struct VM {
    stack: Vec<i32>,
    program: Vec<Instruction>,
    ip: usize, // instruction pointer
}

impl VM {
    fn new(program: Vec<Instruction>) -> Self {
        Self {
            stack: Vec::new(),
            program,
            ip: 0,
        }
    }

    // We'll implement `run()` in the next section...
}
{% endhighlight %}

This lays the foundation for our virtual machine. In the next section, we’ll bring it to life by writing the dispatch 
loop that runs our program.

## `run()`: Getting Things Done

Now that we have a structure for our VM, it’s time to give it life — with a `run()` function.

This will be our **dispatch loop** — the engine that drives our machine. It will:

- Read the instruction at the current position (`ip`)
- Execute it by manipulating the stack
- Move to the next instruction
- Halt when we encounter the `Halt` instruction

Let’s add this to our `impl VM` block:

{% highlight rust %}
fn run(&mut self) {
    while self.ip < self.program.len() {
        match &self.program[self.ip] {
            Instruction::Push(value) => {
                self.stack.push(*value);
            }
            Instruction::Add => {
                let b = self.stack.pop().expect("Stack underflow on ADD");
                let a = self.stack.pop().expect("Stack underflow on ADD");
                self.stack.push(a + b);
            }
            Instruction::Mul => {
                let b = self.stack.pop().expect("Stack underflow on MUL");
                let a = self.stack.pop().expect("Stack underflow on MUL");
                self.stack.push(a * b);
            }
            Instruction::Dup => {
                let top = *self.stack.last().expect("Stack underflow on DUP");
                self.stack.push(top);
            }
            Instruction::Drop => {
                self.stack.pop().expect("Stack underflow on DROP");
            }
            Instruction::Swap => {
                let b = self.stack.pop().expect("Stack underflow on SWAP");
                let a = self.stack.pop().expect("Stack underflow on SWAP");
                self.stack.push(b);
                self.stack.push(a);
            }
            Instruction::Halt => break,
        }

        self.ip += 1;
    }
}
{% endhighlight %}

This loop is dead simple — and that’s exactly what makes it elegant. There are no registers, no heap, no branches just 
yet — just a list of instructions and a stack to evaluate them on.

The use of `expect` on each of our `pop` operations is a small insurance policy. This allows us to report out and 
invalid state on the stack. If we're already at the top of stack (TOS) then we can't pop **more** values.

In future parts, we’ll introduce new instructions to handle control flow, user-defined words, and maybe even a return 
stack — all inspired by Forth.

But before we get ahead of ourselves, let’s write a small program and run it.

# Running

We don't have a parser or compiler yet, so we need to write our Forth program directly inside the Rust code. This will
take the form of a vector of instructions:

{% highlight rust %}
let program = vec![
    Instruction::Push(2),
    Instruction::Push(3),
    Instruction::Add,
    Instruction::Push(4),
    Instruction::Mul,
    Instruction::Halt,
];
{% endhighlight %}

If you squint a little, you’ll notice this is equivalent to the following Forth-style program:

```text
2 3 + 4 *
```

This is exactly the kind of thing you'd see in a Reverse Polish or Forth-based environment — values and operations in
sequence, evaluated by a stack machine.

Now, let’s run our program and inspect the result:

{% highlight rust %}
let mut vm = VM::new(program);
vm.run();

println!("Final stack: {:?}", vm.stack);
{% endhighlight %}

If everything has gone to plan, you should see this output in your terminal:

```text
[20]
```

Giving us the final answer of `20`. That confirms our machine is working — it's reading instructions, performing
arithmetic, and leaving the result on the stack. A tiny virtual computer, built from scratch.

# Conclusion

We’ve built the foundation of a working virtual machine — one that can evaluate simple arithmetic using a stack, just 
like a classic Forth system. It’s small, simple, and powerful enough to demonstrate key ideas behind interpreters, 
instruction dispatch, and virtual machines.

In future posts, we’ll extend this VM to include:

- More complex stack operations
- Named words and a dictionary
- Control flow (`if`, `loop`, etc.)
- A basic text-based REPL
- And maybe even user-defined memory or variables

The code for this article can be found [here](https://github.com/tuttlem/tiny_forth/tree/part1).

