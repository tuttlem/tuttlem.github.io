---
layout: post
title: Calling Assembly from C (32 bit)
date: 2012-11-25
comments: false
categories: [ "Assembly", "C", "32 bit", "Programming" ]
---

One of the biggest advantages of being able to write assembly code is to optimise any bits of your application that you want. That way you can maintain your code base in a half-sane language (like C) and roll your sleeves up to speed up the smaller parts that you want.

This blog post will show you how to call a routine that you've defined in assembly language from your C code. The example that I'll show has been done in a Linux environment using [NASM](http://www.nasm.us/) as the assembler and [GCC](http://gcc.gnu.org/) for the C compiler.

First of all, let's write our C program. This will simply add two integers and output the results using [printf](http://pubs.opengroup.org/onlinepubs/009695399/functions/printf.html).

{% highlight c %}
#include <stdio.h>

/** Forward declaration for our add function */
int add(int, int);

int main() {

   /* add some numbers */
   int x = add(5, 4);

   /* print the result out */
   printf("5+4=%d\n", x);

   return 0;
}
{% endhighlight %}

There isn't anything of great interest in here. We have a forward declaration for our add function. That's about it. Now we have to supply an implementation for our add routine. For this we'll be using assembly language.

{% highlight nasm %}
global add

section .text

add:
   mov   eax, [esp+4]    ; get the 1st param
   mov   ecx, [esp+8]    ; get the 2nd param
   add   eax, ecx        ; add them together
                         ; leaving the return value in eax

   ret
{% endhighlight %}

This is all pretty straight forward. We define a symbol "add" as global. In the code (or .text) section, we supply an implementation for it. The trickiest part here is being able to retrieve parameters that are passed in from the C level. You can see that we're addressing stack pointer to do so. 

Add takes two parameters. In this scenario (c-calling convention) the parameters to the function are pushed onto the stack in reverse order. So, the second parameter goes onto the stack first and then the first. Once all of the pushing has complete, the first parameter is at `[esp+4]` and the second is at `[esp+8]`. Remember - an integer (on this architecture) has 4 bytes (32 bits).

Return values are always left in `eax`. You'll see that after the arithmetic completes, it'll be `eax` that holds the answer. It's what will be given back to the caller.

Finally, all we need to do is compile these files and link together their object files. We do this from the Linux console with the following commands.

{% highlight bash %}
gcc -m32 -c -g add.c -o add.o
nasm -felf32 maths.asm -o maths.o
gcc -m32 add.o maths.o -o add
{% endhighlight %}

We're done. I'll be doing another one of these sorts of blog posts shortly demonstrating how we'll do this in the 64-bit arena.