---
layout: post
title: Getting More Productive With NASM and glibc
date: 2026-01-26
comments: false
categories: [ asm nasm glibc ]
---

Writing "pure syscall" assembly can be fun and educational — right up until you find yourself rewriting `strlen`, `strcmp`, line input, formatting, and file handling for the tenth time.

If you're building tooling (monitors, debuggers, CLIs, experiments), the fastest path is often to write your core logic in assembly and call out to glibc for the boring parts.

In today's article, we'll walk through a basic example to get you up and running. You should quickly see just how thin the C language really is as a layer over assembly and the machine itself.

A full version of what we'll build here today can be found [here](https://gist.github.com/tuttlem/8eea88048f491a77eb56674bff074509).

## Hello, world

We'll start with a simple "Hello, world" style application.

{% highlight nasm %}
BITS 64
DEFAULT REL

extern puts
global main

section .rodata
msg db "Hello from NASM + glibc (puts)!", 0

section .text
main:
  ; puts(const char *s)
  lea   rdi, [rel msg]
  call  puts wrt ..plt      ; <-- PIE-friendly call via PLT

  xor   eax, eax            ; return 0
  ret
{% endhighlight %}

Let's break this down.

{% highlight nasm %}
BITS 64
DEFAULT REL
{% endhighlight %}

First, we tell the assembler that we're generating code for x86-64 using the `BITS` directive.  

`DEFAULT REL` changes the default addressing mode in 64-bit assembly from absolute addressing to **RIP-relative addressing**. This is an important step when writing modern position-independent code (PIC), and allows the resulting executable to work correctly with security features like Address Space Layout Randomisation (ASLR).

{% highlight nasm %}
extern puts
{% endhighlight %}

Functions that are implemented outside our module are resolved at link time. Since the implementation of `puts` lives inside glibc, we declare it as an external symbol.

{% highlight nasm %}
global main
{% endhighlight %}

The true entry point of a Linux program is `_start`. When you write a fully standalone binary, you need to define this yourself.

Because we're linking against glibc, the C runtime provides the startup code for us. Internally, this eventually calls our `main` function. To make this work, we simply mark `main` as global so the linker can find it.

{% highlight nasm %}
section .rodata
msg db "Hello from NASM + glibc (puts)!", 0
{% endhighlight %}

Here we define our string in the read-only data section (`.rodata`). From a C perspective, this is equivalent to storing a `const char *`.

{% highlight nasm %}
section .text
main:
{% endhighlight %}

This marks the beginning of our executable code and defines the `main` entry point.

{% highlight nasm %}
  lea   rdi, [rel msg]
  call  puts wrt ..plt
{% endhighlight %}

This is where we actually print the message.

According to the x86-64 System V ABI (used by Linux and glibc), function arguments are passed in registers using the following order:

- `rdi`
- `rsi`
- `rdx`
- `rcx`
- `r8`
- `r9`

Floating-point arguments are passed in XMM registers.

We load the address of our string into `rdi`, then call `puts`.

The `wrt ..plt` modifier tells NASM to generate a call through the **Procedure Linkage Table (PLT)**. This is required for producing position-independent executables (PIE), which are the default on many modern Linux systems. Without this, the linker may fail or produce non-relocatable binaries.

{% highlight nasm %}
xor   eax, eax
ret
{% endhighlight %}

Finally, we return zero from `main` by clearing `eax`. Control then returns to glibc, which performs cleanup and exits back to the operating system.

## Building

We first assemble the file into an object file:

{% highlight bash %}
nasm -felf64 hello.asm -o hello.o
{% endhighlight %}

Next, we link it using `gcc`. This automatically pulls in glibc and the required runtime startup code:

{% highlight bash %}
gcc hello.o -o hello
{% endhighlight %}

On many modern Linux distributions, position-independent executables are enabled by default. If you encounter relocation errors during linking, you can explicitly enable PIE support:

{% highlight bash %}
gcc -fPIE -pie hello.o -o hello
{% endhighlight %}

Or temporarily disable it while experimenting:

{% highlight bash %}
gcc -no-pie hello.o -o hello
{% endhighlight %}

The PLT-based call form shown earlier works correctly in both cases.

## Conclusion

Calling glibc from NASM is one of those "unlock" moments.

You retain full control over registers, memory layout, and calling conventions — while gaining access to decades of well-tested functionality for free.

Instead of rewriting basic infrastructure, you can focus your energy on the interesting low-level parts of your project.

For tools like debuggers, monitors, loaders, and CLIs, this hybrid approach often provides the best balance between productivity and control.

In the next article, we'll build a small interactive REPL in NASM using `getline`, `strcmp`, and `printf`, and start layering real debugger-style functionality on top.

Assembly doesn't have to be painful — it just needs the right leverage.

