---
layout: post
title: A simple example with gcc and objdump
date: 2015-01-12
comments: false
categories: [ "gcc", "objdump", "asm", "c" ]
---

In today's post, I want to present a dead-simple C program that we'll compile into an object file and then use <strong>objdump</strong> to give us some assembly code. I'll then take you through the generated assembly.

### Using objdump

According to its manpage, objdump is used to <em>dsplay information from object files</em>. It has a whole host of different switches that you can supply to interrogate object files, but we'll only have a very simple usage for it in this post.

I prefer Intel assembly syntax, so I'll specify `-M intel`. We want to disassemble the object file, so we'll use `-d`. It's really helpful to also have the original source code intermixed with the assembly code, so we'll turn that on with `-S`.

Your command should look something like this

{% highlight bash %}
objdump -d -M intel -S yourobjectfile.o
{% endhighlight %}

### Simple example

The most basic program to look at is one that does nothing but return `0` back to the operating system.

{% highlight c %}
int main(int argc, char *argv[]) {
return 0;
}
{% endhighlight %}

Compiling this unit (ensuring to specify `-g` to gcc for debug symbols) and then disassembling with objdump, we're given back the following:

{% highlight text %}
$ objdump -d -M intel -S inline.o

inline.o:     file format elf64-x86-64


Disassembly of section .text:

0000000000000000 <main>:

#include <stdio.h>

int main(int argc, char *argv[]) {
   0:	55                   push   rbp
   1:	48 89 e5             mov    rbp,rsp
   4:	89 7d fc             mov    DWORD PTR [rbp-0x4],edi
   7:	48 89 75 f0          	mov    QWORD PTR [rbp-0x10],rsi
return 0;
   b:	b8 00 00 00 00       mov    eax,0x0
  10:	5d                   pop    rbp
  11:	c3                   ret    
{% endhighlight %}

Whilst the whole block that gets dumped out is important, we're really only worried about the inner implementation of the `main` function call. The translation of this code is equally pretty simple.

{% highlight asm %}
push   rbp
mov    rbp,rsp
mov    DWORD PTR [rbp-0x4],edi
mov    QWORD PTR [rbp-0x10],rsi

mov    eax,0x0
pop    rbp
ret    
{% endhighlight %}

Dissecting this code, we can see that the program first sets up the stack frame for the two parameters passed into main, `argc` and `argv`.

So, we save the previous `rbp` to preserve its state.

{% highlight asm %}
push   rbp
mov    rbp,rsp
{% endhighlight %}

And in accorance with the [calling conventions for System V AMD64](http://en.wikipedia.org/wiki/X86_calling_conventions#System_V_AMD64_ABI)

> The first six integer or pointer arguments are passed in registers RDI, RSI, RDX, RCX, R8, and R9, while XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6 and XMM7 are used for floating point arguments.

Therefore, `argc` being of type `int` is therefore a `DWORD` and is passed via `edi`. `argv` is a pointer and is a `QWORD`; therefore it is passed using the 64 bit register `rsi`.

{% highlight asm %}
mov    DWORD PTR [rbp-0x4],edi
mov    QWORD PTR [rbp-0x10],rsi
{% endhighlight %}

Upon entry, we're just filling up those spots in the stack.

Exiting we're just setting our return value (which is always in the accumulator), restoring the pre-entry value that was in `rbp` and returning to the caller.

{% highlight asm %}
mov    eax,0x0
pop    rbp
ret    
{% endhighlight %}

Write another, more complex C program; disassemble it and see if you can follow along with the results.