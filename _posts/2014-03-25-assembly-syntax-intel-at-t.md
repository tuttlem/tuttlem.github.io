---
layout: post
title: Assembly Syntax: Intel & AT&T
date: 2014-03-25
comments: false
categories: [ "Assembly", "att", "intel", "comparison" ]
---

This post is just a little cheat sheet for myself on Intel & AT&T syntax.

A useful table mapping some simple instructions between the two syntaxes linked through from the [GCC-Inline-Assembly-HOWTO](http://www.ibiblio.org/gferg/ldp/GCC-Inline-Assembly-HOWTO.html#s3):

|       Intel Code               |      AT&T Code                       |
|--------------------------------|--------------------------------------|
| `mov     eax,1`                |  `movl    $1,%eax`                   |   
| `mov     ebx,0ffh`             |  `movl    $0xff,%ebx`                |   
| `int     80h`                  |  `int     $0x80`                     |   
| `mov     ebx, eax`             |  `movl    %eax, %ebx`                |
| `mov     eax,[ecx]`            |  `movl    (%ecx),%eax`               |
| `mov     eax,[ebx+3]`          |  `movl    3(%ebx),%eax`              | 
| `mov     eax,[ebx+20h]`        |  `movl    0x20(%ebx),%eax`           |
| `add     eax,[ebx+ecx*2h]`     |  `addl    (%ebx,%ecx,0x2),%eax`      |
| `lea     eax,[ebx+ecx]`        |  `leal    (%ebx,%ecx),%eax`          |
| `sub     eax,[ebx+ecx*4h-20h]` |  `subl    -0x20(%ebx,%ecx,0x4),%eax` |

Some important points to note:

* Source and destinations are flipped in opcodes.
	* Intel is dest, src
	* AT&T is src, dest
* AT&T decorates registers and immediates
	* Registers are prefixed with a "%"
	* Immediates are prefixed with a "$". This applies to variables being passed in from C (when you're inline).
* Intel decorates memory operands to denote the operand's size, AT&T uses different mnemonics to accomplish the same.
* Intel syntax to dereference a memory location is "[ ]". AT&T uses "( )".
