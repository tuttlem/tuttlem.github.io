---
layout: post
title: How Modern Compilers Optimise Recursive Algorithms
date: 2024-12-21
comments: false
categories: [ "" ]
---

# Introduction

Modern compilers are incredibly sophisticated, capable of transforming even the most inefficient code into highly 
optimized machine instructions. Recursive algorithms, often seen as elegant yet potentially costly in terms of 
performance, present a fascinating case study for these optimizations. From reducing function call overhead to 
transforming recursion into iteration, compilers employ a range of techniques that balance developer productivity with 
runtime efficiency.

In this article, we’ll explore how GCC optimizes recursive algorithms. We’ll examine key techniques such as tail-call 
optimization, stack management, and inlining through a simple, easy to understand example. By the end, you’ll have a clearer 
understanding of the interplay between recursive algorithms and compiler optimizations, equipping you to write code that 
performs better while retaining clarity.

# Factorial

The first example that we'll look at is calculating a factorial.

{% highlight c %}
int factorial(int n, int acc) {
  if (n == 0) {
    return acc;
  }

  return factorial(n - 1, n * acc);
}
{% endhighlight %}

This block of code is fairly simple. `n` is the factorial that we want to calculate with `acc` facilitating the 
recursive processing that we're looking to optimise.

## `-O0`

First of all, we'll compile this function with `-O0` (no optimisation):

{% highlight plain %}
int factorial(int n, int acc) {
   0:   55                      push   %rbp
   1:   48 89 e5                mov    %rsp,%rbp
   4:   48 83 ec 10             sub    $0x10,%rsp
   8:   89 7d fc                mov    %edi,-0x4(%rbp)
   b:   89 75 f8                mov    %esi,-0x8(%rbp)
  if (n == 0) {
   e:   83 7d fc 00             cmpl   $0x0,-0x4(%rbp)
  12:   75 05                   jne    19 <factorial+0x19>
    return acc;
  14:   8b 45 f8                mov    -0x8(%rbp),%eax
  17:   eb 16                   jmp    2f <factorial+0x2f>
  }

  return factorial(n - 1, n * acc);
  19:   8b 45 fc                mov    -0x4(%rbp),%eax
  1c:   0f af 45 f8             imul   -0x8(%rbp),%eax
  20:   8b 55 fc                mov    -0x4(%rbp),%edx
  23:   83 ea 01                sub    $0x1,%edx
  26:   89 c6                   mov    %eax,%esi
  28:   89 d7                   mov    %edx,%edi
  2a:   e8 00 00 00 00          call   2f <factorial+0x2f>
}
  2f:   c9                      leave
  30:   c3                      ret
{% endhighlight %}

The compiler generates straightforward assembly that closely follows the original C code. No optimizations are applied 
to reduce function call overhead or improve performance. You would use this level of optimisation (or lack thereof) in 
situations where you might be debugging; and a straight-forward translation of your code is useful.

Stack operations (`push`, `mov`, `sub`, etc.) are explicitly performed for each recursive call. This results in the 
largest amount of assembly code and higher function call overhead.

## `-O1`

Next, we'll re-compile this function at `-O1` which will give us basic optimisations:

{% highlight plain %}
int factorial(int n, int acc) {
   0:   89 f0                   mov    %esi,%eax
  if (n == 0) {
   2:   85 ff                   test   %edi,%edi
   4:   75 01                   jne    7 <factorial+0x7>
    return acc;
  }

  return factorial(n - 1, n * acc);
}
   6:   c3                      ret
int factorial(int n, int acc) {
   7:   48 83 ec 08             sub    $0x8,%rsp
  return factorial(n - 1, n * acc);
   b:   0f af c7                imul   %edi,%eax
   e:   89 c6                   mov    %eax,%esi
  10:   83 ef 01                sub    $0x1,%edi
  13:   e8 00 00 00 00          call   18 <factorial+0x18>
}
  18:   48 83 c4 08             add    $0x8,%rsp
  1c:   c3                      ret
{% endhighlight %}

The first thing to notice here is the **stack management** at the start of the function. 

`-O0`:

{% highlight asm %}
push   %rbp
mov    %rsp,%rbp
sub    $0x10,%rsp
{% endhighlight %}

The stack frame is explicitly set up and torn down for every function call, regardless of whether it is needed. This 
includes saving the base pointer and reserving 16 bytes of stack space.

We then have slower execution due to redundant stack operations and higher memory overhead.

`-O1`:

{% highlight asm %}
sub    $0x8,%rsp
{% endhighlight %}

The stack frame is more compact, reducing overhead. The base pointer (`%rbp`) is no longer saved, as it’s not strictly 
necessary. This give us reduced stack usage and faster function calls

Next up, we see optimisations around **tail-call optimisation** (TCO).

`-O0`:

{% highlight asm %}
call   2f <factorial+0x2f>
{% endhighlight %}

Recursive calls are handled traditionally, with each call creating a new stack frame.

`-O1`:

{% highlight asm %}
call   18 <factorial+0x18>
{% endhighlight %}

While `-O1` still retains recursion, it simplifies the process by preparing for tail-call optimization. Unnecessary 
operations before and after the call are eliminated.

We also see some **arithmetic simplification** between the optimisation levels:

`-O0`:

{% highlight asm %}
mov    -0x4(%rbp),%eax
imul   -0x8(%rbp),%eax
sub    $0x1,%edx
{% endhighlight %}

Arithmetic operations explicitly load and store intermediate results in memory, reflecting a direct translation of the 
high-level code.

`-O1`:

{% highlight asm %}
imul   %edi,%eax
sub    $0x1,%edi
{% endhighlight %}

Intermediate results are kept in registers (`%eax`, `%edi`), avoiding unnecessary memory access.

There's also some **instruction elimination** between the optimisation levels:

`-O0`:

{% highlight asm %}
mov    -0x8(%rbp),%eax
mov    %eax,%esi
mov    -0x4(%rbp),%edx
{% endhighlight %}

Each variable is explicitly loaded from the stack and moved between registers, leading to redundant instructions.

`-O1`:

{% highlight asm %}
mov    %esi,%eax
{% endhighlight %}

The compiler identifies that some operations are unnecessary and eliminates them, reducing instruction count.

We finish off with a **return path optimisation**.

`-O0`:

{% highlight asm %}
leave
ret
{% endhighlight %}

Explicit `leave` and `ret` instructions are used to restore the stack and return from the function.

`-O1`:

{% highlight asm %}
ret
{% endhighlight %}

The `leave` instruction is eliminated as it’s redundant when the stack frame is managed efficiently.

With reduced stack overhead and fewer instructions, the function executes faster and consumes less memory at `-O1` 
compared to `-O0`. Now we'll see if we can squeeze things even further.

## `-02`

We re-compile the same function again, turning optimisations up to `-O2`. The resulting generated code is this:

{% highlight plain %}
int factorial(int n, int acc) {
   0:   89 f0                   mov    %esi,%eax
  if (n == 0) {
   2:   85 ff                   test   %edi,%edi
   4:   74 28                   je     2e <factorial+0x2e>
   6:   8d 57 ff                lea    -0x1(%rdi),%edx
   9:   40 f6 c7 01             test   $0x1,%dil
   d:   74 11                   je     20 <factorial+0x20>
    return acc;
  }

  return factorial(n - 1, n * acc);
   f:   0f af c7                imul   %edi,%eax
  12:   89 d7                   mov    %edx,%edi
  if (n == 0) {
  14:   85 d2                   test   %edx,%edx
  16:   74 17                   je     2f <factorial+0x2f>
  18:   0f 1f 84 00 00 00 00    nopl   0x0(%rax,%rax,1)
  1f:   00 
  return factorial(n - 1, n * acc);
  20:   0f af c7                imul   %edi,%eax
  23:   8d 57 ff                lea    -0x1(%rdi),%edx
  26:   0f af c2                imul   %edx,%eax
  if (n == 0) {
  29:   83 ef 02                sub    $0x2,%edi
  2c:   75 f2                   jne    20 <factorial+0x20>
}
  2e:   c3                      ret
  2f:   c3                      ret
{% endhighlight %}

First we see some **instruction-level parallelism** here. 

`-O2` introduces techniques that exploit CPU-level parallelism. This is visible in the addition of the lea (load 
effective address) instruction and conditional branching.

`-O1`:

{% highlight asm %}
imul   %edi,%eax
sub    $0x1,%edi
call   18 <factorial+0x18>
{% endhighlight %}

`-O2`:

{% highlight asm %}
lea    -0x1(%rdi),%edx
imul   %edi,%eax
mov    %edx,%edi
test   %edx,%edx
jne    20 <factorial+0x20>
{% endhighlight %}

At `-O2`, the compiler begins precomputing values and uses lea to reduce instruction latency. The conditional branch 
(`test` and `jne`) avoids unnecessary function calls by explicitly checking the termination condition.

Next, we see the compiler partially does some **loop unrolling** 

`-O1` Recursion is preserved:

{% highlight asm %}
call   18 <factorial+0x18>
{% endhighlight %}

`-O2` Loop structure replaces recursion:

{% highlight asm %}
imul   %edi,%eax
lea    -0x1(%rdi),%edx
sub    $0x2,%edi
jne    20 <factorial+0x20>
{% endhighlight %}

The recursion is transformed into a loop-like structure that uses the `jne` (jump if not equal) instruction to iterate 
until the base case is met. This eliminates much of the overhead associated with recursive function calls, such as 
managing stack frames.

More **redundant operations removed** from the code. Redundant instructions like saving and restoring registers are 
removed. This is particularly noticeable in how the return path is optimized.

`-O1`:

{% highlight asm %}
add    $0x8,%rsp
ret
{% endhighlight %}

`-O2`:

{% highlight asm %}
ret
{% endhighlight %}

`-O2` eliminates the need for stack pointer adjustments because the compiler reduces the stack usage overall.

Finally, we see some more sophisticated **conditional simplifications**.

`-O1`:

{% highlight asm %}
test   %edi,%edi
jne    7 <factorial+0x7>
{% endhighlight %}

`-O2`:

{% highlight asm %}
test   %edi,%edi
je     2e <factorial+0x2e>
{% endhighlight %}

Instead of jumping to a label and performing additional instructions, `-O2` jumps directly to the return sequence 
(`2e <factorial+0x2e>`). This improves branch prediction and minimizes unnecessary operations.

These transformations further reduce the number of instructions executed per recursive call, optimizing runtime 
efficiency while minimizing memory footprint.

## `-O3`

When we re-compile this code for `-O3`, we notice that the output code is identical to `-O2`. This suggests that the 
compiler found all of the performance opportunities in previous optimisation levels.

This highlights an important point: not all functions benefit from the most aggressive optimization level.

The factorial function is simple and compact, meaning that the optimizations applied at `-O2` (tail-recursion 
transformation, register usage, and instruction alignment) have already maximized its efficiency. `-O3` doesn’t 
introduce further changes because:

* The function is too small to benefit from aggressive inlining.
* There are no data-parallel computations that could take advantage of SIMD instructions.
* Loop unrolling is unnecessary since the tail-recursion has already been transformed into a loop.

For more complex code, `-O3` often shines by extracting additional performance through aggressive heuristics, but in 
cases like this, the improvements plateau at `-O2`.

# Conclusion

Recursive algorithms can often feel like a trade-off between simplicity and performance, but modern compilers 
significantly narrow this gap. By employing advanced optimizations such as tail-call elimination, inline expansion, and 
efficient stack management, compilers make it possible to write elegant, recursive solutions without sacrificing runtime 
efficiency.

Through the examples in this article, we’ve seen how these optimizations work in practice, as well as their limitations. 
Understanding these techniques not only helps you write better code but also deepens your appreciation for the compilers 
that turn your ideas into reality. Whether you’re a developer crafting algorithms or just curious about the magic 
happening behind the scenes, the insights from this exploration highlight the art and science of compiler design.