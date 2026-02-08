---
layout: post
title: Making a REPL with NASM and glibc
date: 2026-02-08
comments: false
categories: [ asm nasm glibc repl ]
---

In the [previous article]({% post_url 2026-01-26-getting-more-productive-with-nasm-and-glibc %}) we learned something important:

Assembly becomes dramatically more productive the moment you stop rewriting libc.

Printing text, formatting numbers, comparing strings, and handling input are already solved problems — and they’ve been solved extremely well.

Now we’re going to push that idea to its natural conclusion. We are going to write a *real* interactive program in pure assembly. A program that stays alive, reads commands, parses arguments, and performs actions.

In other words — a REPL.

By the end, this will work:

{% highlight plain %}
> help
commands: help add quit

> add 5 7
12

> add 1 2
3

> what
unknown command

> quit
bye
{% endhighlight %}

And we still won’t write a single syscall.

The full code listing for this article can be found [here](https://gist.github.com/tuttlem/f5a3d58d7501f0aaa7f1e7b4ecf43956). We will be covering this code, piece by piece.


## The Shape of the Program

Before writing any code, we need to understand the structure.

A REPL is just a loop:

1. print a prompt
2. read a line
3. decide what it means
4. run a handler
5. repeat

There is no magic here. High level languages don’t provide REPLs — they just hide loops.

In assembly, we simply write the loop ourselves.

## External Functions

We will use these glibc functions:

- `printf` — formatted output
- `getline` — dynamic input
- `strcmp` — command matching
- `atoi` — integer parsing
- `free` — memory ownership

Let's declare them.

{% highlight nasm %}
BITS 64
DEFAULT REL

extern printf
extern getline
extern strcmp
extern atoi
extern free

global main
{% endhighlight %}

Exactly like before, these symbols exist inside glibc and will be resolved at link time.

## Static Data

We now define the strings our program will use.

{% highlight nasm %}
section .rodata
prompt      db "> ", 0
bye_msg     db "bye", 10, 0
unk_msg     db "unknown command", 10, 0
help_msg    db "commands: help add quit", 10, 0
add_fmt     db "%d", 10, 0

cmd_help    db "help", 0
cmd_add     db "add", 0
cmd_quit    db "quit", 0
{% endhighlight %}

This is exactly like C string constants — null terminated and stored in read-only memory.

## Writable Storage

We now need somewhere to store input state.

`getline` allocates memory for us, but we must *own the pointer*.

{% highlight nasm %}
section .bss
lineptr     resq 1
linesize    resq 1
{% endhighlight %}

This is important.

`getline` does not return a string.

It fills a pointer *that we provide*.  
That pointer may be reallocated between calls.

So we must store it globally.

## Program Entry

We now write `main`.

{% highlight nasm %}
section .text
main:
  push rbp
  mov  rbp, rsp
{% endhighlight %}

We create a normal stack frame. Not strictly required — but keeps debugging sane and mirrors C expectations.

Now we initialise the buffer state.

{% highlight nasm %}
mov qword [lineptr], 0
mov qword [linesize], 0
{% endhighlight %}

This tells `getline`:

> I do not own a buffer yet — please allocate one.

## The REPL Loop

Here is the heart of the program.

{% highlight nasm %}
repl:
{% endhighlight %}

A label is all a loop really is.

### Printing the Prompt

{% highlight nasm %}
lea rdi, [rel prompt]
xor eax, eax
call printf wrt ..plt
{% endhighlight %}

We load the format string into `rdi`.

Why `xor eax, eax`?

Because `printf` is variadic.  
The System V ABI requires `rax` to contain the number of vector registers used — zero in our case.

C hides this rule. Assembly makes you honest.

### Reading a Line

{% highlight nasm %}
lea rdi, [rel lineptr]
lea rsi, [rel linesize]
mov rdx, [rel stdin]
call getline wrt ..plt
{% endhighlight %}

`getline` signature:

{% highlight c %}
ssize_t getline(char **lineptr, size_t *n, FILE *stream);
{% endhighlight %}

So we pass:

| register | value |
|------|------|
| rdi | pointer to buffer pointer |
| rsi | pointer to size |
| rdx | stdin |

This function may:

* allocate memory
* grow memory
* reuse memory

Which means:

We must eventually call `free`.

### Extract Command

We now compare the input against commands.

{% highlight nasm %}
mov rdi, [lineptr]
lea rsi, [rel cmd_help]
call strcmp wrt ..plt
test eax, eax
je do_help
{% endhighlight %}

`strcmp` returns zero when equal.

So we branch. This is effectively our `switch` and `case`.

### Unknown Command Fallback

{% highlight nasm %}
lea rdi, [rel unk_msg]
xor eax, eax
call printf wrt ..plt
jmp repl
{% endhighlight %}

This is our default case.

## Help Command

{% highlight nasm %}
do_help:
lea rdi, [rel help_msg]
xor eax, eax
call printf wrt ..plt
jmp repl
{% endhighlight %}

No surprises — just structured control flow.

Assembly is not chaotic.  
It just doesn’t auto-indent for you.

## Quit Command

{% highlight nasm %}
mov rdi, [lineptr]
lea rsi, [rel cmd_quit]
call strcmp wrt ..plt
test eax, eax
je do_quit
{% endhighlight %}

{% highlight nasm %}
do_quit:
  lea rdi, [rel bye_msg]
  xor eax, eax
  call printf wrt ..plt

  mov rdi, [lineptr]
  call free wrt ..plt

  xor eax, eax
  leave
  ret
{% endhighlight %}

Here we finally release memory ownership.

This is the most important rule in the entire article:

If libc allocates it, libc expects you to free it.

Assembly didn’t make this hard — ignoring ownership did.

## Add Command (Parsing Arguments)

Now the interesting part.

We skip `"add "` and parse numbers.

{% highlight nasm %}
do_add:
mov rbx, [lineptr]
add rbx, 4
{% endhighlight %}

We manually advance past `"add "`.

This is literally what C does internally. Now we process the first number.

{% highlight nasm %}
mov rdi, rbx
call atoi wrt ..plt
mov r12d, eax
{% endhighlight %}

`atoi` converts text to integer.

We store it in a preserved register. Now we'll look for the second parameter.

{% highlight nasm %}
find_space:
cmp byte [rbx], 0
je repl
cmp byte [rbx], ' '
je found_space
inc rbx
jmp find_space

found_space:
inc rbx
{% endhighlight %}

We manually walk the string.

This is what string parsing actually is:  
a loop and a condition.

{% highlight nasm %}
mov rdi, rbx
call atoi wrt ..plt
add eax, r12d
{% endhighlight %}

Now we have the result.

### Print Result

{% highlight nasm %}
mov esi, eax
lea rdi, [rel add_fmt]
xor eax, eax
call printf wrt ..plt
jmp repl
{% endhighlight %}

And the loop continues.

## Building

Same as before.

{% highlight bash %}
nasm -felf64 repl.asm -o repl.o
gcc repl.o -o repl
{% endhighlight %}

## What We Actually Built

We did not implement:

- input buffering
- dynamic allocation
- number parsing
- formatted output
- terminal handling

Yet this is undeniably a real interactive program.

The difference between C and assembly is not capability.

It is visibility.

C hides the machine.  
Assembly exposes it.  
glibc carries the weight in both cases.

## Conclusion

Assembly feels impossible when you try to do everything yourself.

But real programs were never written that way — not even in the 1970s.

They were written as small pieces of logic sitting on top of shared libraries.

That’s exactly what we built here.

