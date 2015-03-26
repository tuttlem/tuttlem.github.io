---
layout: post
title: 16 bit COM files
date: 2015-03-26
comments: false
categories: [ "16bit", "com", "dos" ]
---

[COM files](http://en.wikipedia.org/wiki/COM_file) are plain binary executable file format from the [MS-DOS](http://en.wikipedia.org/wiki/MS-DOS) era ([and before!](http://en.wikipedia.org/wiki/CP/M)) that provide a very simple execution model. 

The execution environment is given one 64kb segment to fit its code, stack and data segments into. This memory model is sometimes referred to as the "tiny" model.

In today's post, we're going to write a really simple program; compile it, disassemble it and dissect it. Here's our program that very helpfully prints "Hello, world!" to the console and then exits.

{% highlight asm %}
ORG 100h

section .text

start:
	mov		dx, msg
	mov		ah, 09h
	int		21h

	ret

section .data

	msg DB 'Hello, world!', 13, 10, '$'
{% endhighlight %}

Nothing of great interest here. The only thing worth a mention is the `ORG` directive. This tells the assembler (and therefore the execution environment once executed) that our program starts at the offset `100h`. There's some more information regarding 16bit programs with nasm [here](http://www.nasm.us/doc/nasmdoc8.html).

nasm's default output format is plain binary so, assembly is very simple:

{% highlight bash %}
$ nasm hello.asm -o hello.com
{% endhighlight %}

Running our program in [dosbox](http://www.dosbox.com/) and we're given our prompt as promised.  Taking a look at the binary on disk, it's seriously small. 24 bytes small. We won't have much to read when we dissassemble it!

Because this is a plain binary file, we need to give objdump a little help in how to present the information.

{% highlight bash %}
$ objdump -D -b binary -mi386 -Maddr16,data16 hello.com 
{% endhighlight %}

The full output dump is as follows:

{% highlight text %}
hello.com:     file format binary


Disassembly of section .data:

00000000 <.data>:
   0:	ba 08 01             	mov    $0x108,%dx
   3:	b4 09                	mov    $0x9,%ah
   5:	cd 21                	int    $0x21
   7:	c3                   	ret    
   8:	48                   	dec    %ax
   9:	65                   	gs
   a:	6c                   	insb   (%dx),%es:(%di)
   b:	6c                   	insb   (%dx),%es:(%di)
   c:	6f                   	outsw  %ds:(%si),(%dx)
   d:	2c 20                	sub    $0x20,%al
   f:	77 6f                	ja     0x80
  11:	72 6c                	jb     0x7f
  13:	64 21 0d             	and    %cx,%fs:(%di)
  16:	0a 24                	or     (%si),%ah
{% endhighlight %}

Instructions located from `0` through to `7` correspond directly to the assembly source code that we've written. After this point, the file is storing our string that we're going to print which is why the assembly code looks a little chaotic.

Removing the jibberish assembly language, the bytes directly correspond to our string:

{% highlight text %}
	"H"
   8:	48                   	
   	"e"
   9:	65                   	
   	"l"
   a:	6c                   	
   	"l"
   b:	6c                   	
   	"o"
   c:	6f                   	
   	", "
   d:	2c 20                	
   	"wo"
   f:	77 6f                	
   	"rl"
  11:	72 6c                	
  	"d!", 13
  13:	64 21 0d             	
  	10, "$"
  16:	0a 24                	
{% endhighlight %}

So, our string starts at address `8` but the first line of our assembly code; the line that's loading `dx` with the address of our string `msg` has disassembled to this:

{% highlight text %}
   0:	ba 08 01             	mov    $0x108,%dx
{% endhighlight %}

The address of `$0x108` is going to overshoot the address of our string by `0x100`! This is where the `ORG` directive comes in. Because we have specified this, all of our addresses are adjusted to suit. When DOS loads our COM file, it'll be in at `0x100` and our addresses will line up perfectly.

