---
layout: post
title: Executing shellcode in C
date: 2017-10-28
comments: false
categories: [ "shellcode", "c" ]
---

A fairly common technique when welding chunks of executable code together is to have the ability to flexibly execute that code. In today's article, I'll take you through extracting the shellcode for a function that you write along with the hosting C code that will execute the shellcode for you.

## Starting with a C function

First up, we'll create some shellcode to execute. This is just going to be a very simple function and we'll compile-only; no linking. Once we have an object file, we'll use `objdump` to view the dissassembly and extract the shell code.

Here's the function.

{% highlight c %}
int foo() {
  return 10;
}
{% endhighlight %}

Not the most exciting; but we should take note of the function's signature at this point. We're going to create a pointer that points to a function of this signature when we go to execute this code.

For reference, a function pointer that points to a function of this signature would be `int (*f)()`.

Compile, and dissassemble.

{% highlight bash %}
gcc -c -03 ex.c
objdump ex.o -d
{% endhighlight %}

## The shellcode

The output of this function will look something similar to this:

{% highlight text %}
ex.o:     file format elf64-x86-64


Disassembly of section .text:

0000000000000000 <foo>:
   0:	b8 0a 00 00 00       	mov    $0xa,%eax
   5:	c3                   	retq
{% endhighlight %}

`objdump` gives us the dissassembly to the right-hand side; but the hex breakdown to the left is what we're interested in. Concatenating all of these bytes together, we're given `b80a000000c3`. This is our shell code!

## Execution

This is all pretty simple.

* Setup the shellcode in an array
* Create a function pointer that points to the shellcode
* Execute that function

{% highlight c %}
#include <stdio.h>

unsigned char code[] = "\xb8\x0a\x00\x00\x00\xc3";

int main(int argc, char **argv) {
  int foo_value = 0;

  int (*foo)() = (int(*)())code;
  foo_value = foo();

  printf("%d\n", foo_value);
}
{% endhighlight %}

The variable `code` holds the shellcode, and you can see it in string form there. We've just taken the `objdump` output and prepared it for a `unsigned char` string. 

We create the function pointer called `foo` to point to that block of code. Execute the code, grabbing the return value and then print it to screen.

The only tricky part about this is compiling the host program. We need to specify two switches to allow our binary to run.

`-z execstack` and `--fno-stack-protector`. Without these our code will just segfault.

{% highlight bash %}
gcc -fno-stack-protector -z execstack test.c -o test
{% endhighlight %}

This code runs as expected now, pushing a `10` out the STDOUT through `printf`.


