---
layout: post
title: Hello, 64bit Assembly!
date: 2012-11-17
comments: 
categories: [ "Assembly", "Development", "Hello World", "64 bit", "Linux" ]
---

So, I'm a tragic for assembly language. I've finally gotten around to sharpening up my skills to attack Linux with some assembly language and why not upgrade the skills into the 64bit world at the same time.  

"Hello world" follows:  

{% highlight nasm %}
section .data
	hello:      db 'Hello world!',10    ; text plus a linefeed char
	helloLen:   equ $-hello             ; length of the string

section .text
	global _start
	
_start:
	mov   rax, 4            ; system call for "write" (sys_write)
	mov   rbx, 1            ; file descriptor (stdout = 1)
	mov   rcx, hello        ; offset to write
	mov   rdx, helloLen     ; number of bytes to write
	int   0x80              ; syscall
	
	mov   rax, 1            ; system call for exit (sys_exit)
	mov   rbx, 0            ; error code (no error = 0)
	int   0x80              ; syscall
{% endhighlight %}

Build at the command line with <b>nasm</b>:  

{% highlight bash %}
nasm -f elf64 hello.asm
ld -s -o hello hello.o
{% endhighlight %}

Some further reading: 

* <a href="http://docs.cs.up.ac.za/programming/asm/derick_tut/">http://docs.cs.up.ac.za/programming/asm/derick_tut/</a>
* <a href="http://software.intel.com/en-us/articles/introduction-to-x64-assembly/">http://software.intel.com/en-us/articles/introduction-to-x64-assembly/</a>