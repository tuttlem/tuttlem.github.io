---
layout: post
title: Building a stack based VM in rust - Part 3
date: 2025-05-25
comments: false
categories: [ rust, vm, forth, systems ]
---

# Introduction

In [Part 2]({% post_url 2025-05-24-building-a-stack-based-vm-in-rust-part-2 %}), we extended our Forth-style virtual 
machine with a bunch of classic stack manipulation words — from `OVER` and `ROT` to `2DUP`, `2SWAP`, and more.

This gave our machine more expressive power, but it still lacked something crucial: **control flow**. In this part, 
we’ll fix that.

By adding **branching** and **subroutine support**, we allow our VM to make decisions and reuse logic — two 
foundational ideas in all real programming languages.

# Control Flow in Stack Machines

Stack machines like Forth typically handle control flow through explicit instruction manipulation — that is, 
**jumping** to new parts of the program and **returning** when done.

We’ll implement:

| Instruction     | Stack Effect       | Description |
|-----------------|--------------------|-------------|
| `IfZero(offset)`| `( n -- )`         | Jumps `offset` if top is zero |
| `Jump(offset)`  | `( -- )`           | Always jumps `offset` |
| `Call(addr)`    | `( -- )`           | Saves return address and jumps |
| `Return`        | `( -- )`           | Pops return address and jumps to it |

These instructions give us the power to create conditionals and function-like routines.

# Extending the Instruction Set

Let’s extend our enum with the new operations:

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
    Jump(isize),     // new
    IfZero(isize),   // new
    Call(usize),     // new
    Return,          // new
    Halt,
}
{% endhighlight %}

In order to support our ability to call subroutines, our virtual machine needs another stack. This stack is in charge 
of remembering where we came from so that we can return back to the correct place. The return stack is just another 
piece of state management for the virtual machine:

{% highlight rust %}
struct VM {
    stack: Vec<i32>,
    program: Vec<Instruction>,
    ip: usize,
    return_stack: Vec<usize>,       // new
}
{% endhighlight %}

And make sure `VM::new()` initializes that new return stack:

{% highlight rust %}
impl VM {
    fn new(program: Vec<Instruction>) -> Self {
        Self {
            stack: Vec::new(),
            program,
            ip: 0,
            return_stack: Vec::new(),       // new
        }
    }
}
{% endhighlight %}

# Implementing Control Instructions

Each control instruction is added to the `run()` method just like any other:

## JUMP

Unconditionally jumps to a new offset from the current instruction pointer.

**Stack effect**: `( -- )`

{% highlight rust %}
Instruction::Jump(offset) => {
    self.ip = ((self.ip as isize) + offset) as usize;
    continue;
}
{% endhighlight %}

We use `continue` here because we don’t want to execute the usual `ip += 1` after a jump.

## IFZERO

Conditionally jumps based on the top stack value.

**Stack effect**: `( n -- )`

{% highlight rust %}
Instruction::IfZero(offset) => {
    let cond = self.stack.pop().expect("Stack underflow on IFZERO");
    if cond == 0 {
        self.ip = ((self.ip as isize) + offset) as usize;
        continue;
    }
}
{% endhighlight %}

If the value is zero, we adjust `ip` by the offset. If not, we let the loop continue as normal.

## CALL

Pushes the current instruction pointer onto the return stack and jumps to the absolute address.

**Stack effect**: `( -- )`

{% highlight rust %}
Instruction::Call(addr) => {
    self.return_stack.push(self.ip + 1);
    self.ip = *addr;
    continue;
}
{% endhighlight %}

We store `ip + 1` so that `Return` knows where to go back to.

## RETURN

Pops the return stack and jumps to that address.

**Stack effect**: `( -- )`

{% highlight rust %}
Instruction::Return => {
    let ret = self.return_stack.pop().expect("Return stack underflow");
    self.ip = ret;
    continue;
}
{% endhighlight %}

This makes it possible to write reusable routines, just like functions.

# Example: Square a Number

Let’s write a subroutine that squares the top value of the stack — like this:

{% highlight forth %}
: square dup * ;
5 square
{% endhighlight %}

Translated into VM instructions:

{% highlight rust %}
let program = vec![
    // main
    Instruction::Push(5),       // [5]
    Instruction::Call(3),       // jump to square
    Instruction::Halt,

    // square (addr 3)
    Instruction::Dup,           // [5, 5]
    Instruction::Mul,           // [25]
    Instruction::Return,
];

let mut vm = VM::new(program);
vm.run();
println!("Final stack: {:?}", vm.stack);
{% endhighlight %}

Expected output:

{% highlight text %}
[25]
{% endhighlight %}

If you accidentally used `Call(5)`, you’d be jumping to `Return`, skipping your routine completely — a classic off-by-one bug that’s easy to spot once you think in terms of instruction addresses.

# Conclusion

With these new control flow instructions, we’ve unlocked a huge amount of expressive power. Our VM can now:

- Execute conditional logic
- Jump forwards and backwards
- Encapsulate and reuse stack behavior with subroutines

In the next part, we’ll take the leap into **defining named words**, allowing us to simulate real Forth syntax like:

{% highlight forth %}
: square dup * ;
5 square
{% endhighlight %}


We'll build a **dictionary**, wire up some simple parsing, and move closer to an interactive REPL.

The code for this part is available [here on GitHub](https://github.com/tuttlem/tiny_forth/tree/part3).
