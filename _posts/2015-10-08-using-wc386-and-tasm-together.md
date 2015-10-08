---
layout: post
title: Using wcc386 and tasm together
date: 2015-10-08
comments: false
categories: [ "watcom", "wcc386", "tasm" ]
---

It's possible to use the [Watcom](https://en.wikipedia.org/wiki/Watcom_C/C%2B%2B_compiler) compiler to mix your code with modules compiled (or in this article's case, assembled) with other tools. In today's post, I'll take you through the simple process of creating a module using [Borland's Turbo Assembler]() and linking it with a simple C program.

### Creating a test

First thing to do, is to create an assembly module that we can test. For this module, we're going to take two numbers; add them together and send out the result.

{% highlight asm %}
; adder.asm
;
; Assembly module to add two numbers

.386p
.MODEL  FLAT

_TEXT SEGMENT DWORD PUBLIC 'CODE'
    ASSUME CS:_TEXT

    PUBLIC add_numbers

    add_numbers PROC NEAR
        push    ebp
        mov     ebp, esp

        ARG     A:DWORD, B:DWORD

        mov     eax, [A]
        mov     ecx, [B]
        add     eax, ecx

        mov     esp, ebp
        pop     ebp

        ret
    add_numbers ENDP

_TEXT ENDS
END

{% endhighlight %}

This is a basic module, with most of the stack-balancing work being handled for us by the `ARG` directive. In accordance with the 32bit ABI, we put the result in EAX at the end of execution. Producing an object file from this assembly source is relatively easy:

{% highlight text %}
C:\SRC> tasm /mx /zi /os adder.asm adder.obj
{% endhighlight %}

### Integrating with the module

Now that we've got an object file with our function in it, we'll create a very small, simple C program that will use this function. In order to do so though, we need to declare the function as an extern; as it is implemented externally to our C code:

{% highlight c %}
/* test.c
 *
 * Assembly module usage
 */
#include <stdio.h>

/* Here's our externally implemented function */
int add_numbers(int a, int b);

int main(int argc, char *argv[]) {
    printf("2 + 3 = %d\n", add_numbers(2, 3));
    return 0;
}
{% endhighlight %}

A breakthrough; this is not. This does integrate with our assembly module now. All that's left is to compile our C module and link all of the objects together:

{% highlight text %}
C:\SRC> wcc386 /d2 /3s test.c
C:\SRC> wlink system dos4g file test,adder name test
{% endhighlight %}

From there, we have a linked and read to run executable `test.exe`.



