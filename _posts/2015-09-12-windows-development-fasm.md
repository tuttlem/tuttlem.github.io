---
layout: post
title: Windows Development with FASM
date: 2015-09-12
comments: false
categories: [ "windows", "win32", "fasm", "assembly" ]
---

In this post, I'll walk through the steps required to bootstrap your development experience against the [Win32 API](https://msdn.microsoft.com/en-us/library/windows/desktop/ff818516%28v=vs.85%29.aspx) using the [Flat Assembler](http://flatassembler.net/).

### Prerequisites

Everything is provided for you when you download the *fasmw* package, including libraries, include files. *fasmw* even includes a linker pre-geared for windows as your target.

### Test program

Here's the code to present a message box.

{% highlight asm %}
include 'win32ax.inc'

.code

	start:

		invoke MessageBox, HWND_DESKTOP, 'This is a test', 'Hey, Hey!', MB_OK
		invoke ExitProcess, 0

.end start
{% endhighlight %}

Everything is provided to you through `win32ax.inc`. This can be swapped out easily enough with a 64 bit counterpart if you're targeting different architectures.

### Assembling and linking

Now that you've got your source file, `hello.asm` you can produce an object file with the following:

{% highlight text %}
C:\src> fasm hello.asm
{% endhighlight %}

That's all there is to it. You'll now have an exe available to you to run. The only gotcha is giving `fasm` a hint as to where your include files are, and you do this through the `INCLUDE` environment variable:

{% highlight text %}
SET INCLUDE=C:\fasmw\include
{% endhighlight %}

