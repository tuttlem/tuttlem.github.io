---
layout: post
title: Calling Assembly from C (64 bit)
date: 2012-11-27
comments: false
---

In a [previous post]({% post_url 2012-11-25-calling-assembly-from-c-32-bit %}), I spoke about calling assembly language routines from C in 32-bit land. In this post, I aim to show you the same technique only this time using a 64 bit architecture.

I'll be using the same technology stack as last time with [NASM](http://www.nasm.us/) as my assembler and [GCC](http://gcc.gnu.org/) as my C compiler. The code snippets that you'll see in this post have been compiled and linked together on the linux platform.

Alright then. On with the code!

The C-code is not different from the 32-bit version as mentioned in my previous post. Now <strong>that's</strong> portability for you!

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

The assembly language module has changed somewhat though.

{% highlight nasm %}
global add

section .text

add:
   mov   rax, rdi    ; get the first parameter
   add   rax, rsi    ; add the second parameter
                     ; leaving the return value in rax
   ret
{% endhighlight %}

Immediately you can see that a new set of registers are being used and the stack is no longer being used to take parameters in from external calls.

The change in calling convention can be studied in further detail [here](http://www.x86-64.org/documentation/abi.pdf), however it's just important to mention this:

> Once arguments are classified, the registers get assigned (in left-to-right order) for passing as follows:
> If the class is MEMORY, pass the argument on the stack.
> If the class is INTEGER, the next available register of the sequence %rdi, %rsi, %rdx, %rcx, %r8 and %r9 is used

So, the first argument is passed in RDI and the second in RSI. The return value remains the same being passed back to the caller using the accumulator register (RAX in 64-bit land). There are many resources to read up on when it comes to the 64 bit architecture; [Intel](http://www.intel.com/content/www/us/en/architecture-and-technology/microarchitecture/intel-64-architecture-general.html) and [Wikipedia](http://en.wikipedia.org/wiki/X86-64) are both excellent resources.

Finally, when it comes to building these files together, it's (again) very similar to its 32bit counterpart.

{% highlight bash %}
$ gcc -c add.c -o add.o
$ nasm -f elf64 maths.asm -o maths.o
$ gcc -m64 add.o maths.o -o add
{% endhighlight %}

We're done!