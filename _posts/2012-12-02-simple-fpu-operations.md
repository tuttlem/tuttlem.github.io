---
layout: post
title: Simple FPU Operations
date: 2012-12-02
comments: false
categories: [ "Assembly", "Programming", "FPU" ]
---

The CPU itself is an impressive piece of kit just by itself however it does struggle to do complex floating point mathematics. Chip designers noticed this deficiency pretty quickly and bolted on a floating-point arithmetic unit. It should be noted at this point that many, many other tutorial/article writers have gone into great depth explaining how to program FPU's and they have been great references to me in previous times:

* [Simply FPU](http://www.website.masmforum.com/tutorials/fptute/)
* [80387+ Coprocessor Programming](http://qlibdos32.sourceforge.net/tutor/tutor-fpu.php)
* [Intel Manuals](http://www.intel.com/content/www/us/en/processors/architectures-software-developer-manuals.html)

Anyway, this blog post is just going to show you a few nuggets that you can use straight away. It's always fun seeing this stuff in action. This snippet will sum (accumulate) together an array of doubles.

{% highlight nasm %}
; entry conditions
; edx points to an array of doubles
; ecx holds the number of items in the array

; set st0 to zero
fldz                            

next:

; add the next double to st0
fadd    qword [edx]             

; progress
add     edx, 8                  
dec     ecx                     
jnz     next                    

; at this point, st0 holds the sum
{% endhighlight %}

This following snippet will show you a simple addition, subtraction, multiplication and division. You'll notice a pretty distinct pattern in what to do in these situations. It'll follow:

* Load the first term (`fld`)
* Apply the operator (`fadd`,`fsub`,`fmul`,`fdiv`) with the second term
* Store the result into memory

{% highlight nasm %}
section .data

a:	dq	3.333333333	
b:	dq	4.444444444	
	
section .bss 		

c:	resq	1		

section .text

addb:				
	fld	qword [a] 	
	fadd	qword [b]	
	fstp	qword [c]	
	
subb:				
	fld	qword [a] 	
	fsub	qword [b]	
	fstp	qword [c]	
	
mulb:				
	fld	qword [a]	
	fmul	qword [b]	
	fstp	qword [c]	
	
diva:				
	fld	qword [b] 	
	fdiv	qword [a]	
	fstp	qword [c]	
{% endhighlight %}

With a bit of extra help from the C library, you can print out values that you're using in the FPU. The following snippet prints PI to the console.

{% highlight nasm %}
; compiled on linux 64 bit using the following
;
; nasm -f elf32 print.asm -o print.o
; gcc -m32 print.o -o print
;

[bits 32]

section .text

extern printf
global main

main:
   ; load pi into st(0)
   fldpi

   ; prepare some space on the stack
   sub   esp, 8
   ; to be able to push st(0) in there
   fstp  qword [esp]
   ; get the string format on the stack as well
   push  format
   ; print the string
   call  printf

   ; repair the stack
   ;   4 bytes memory address (for the format)
   ; + 8 bytes memory for the float
   ; =========
   ;  12 bytes
   add   esp, 12

   ; exit without error
   xor   eax, eax
   ret

section .data

format: db "%.20g",10,0
{% endhighlight %}

As I find other snippets in old pieces of code, I'll be sure to add them to this page.