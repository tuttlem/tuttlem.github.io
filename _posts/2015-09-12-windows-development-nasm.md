---
layout: post
title: Windows Development with NASM
date: 2015-09-12
comments: false
categories: [ "windows", "win32", "nasm", "assembly" ]
---

In this post, I'll walk through the steps required to bootstrap your development experience against the [Win32 API](https://msdn.microsoft.com/en-us/library/windows/desktop/ff818516%28v=vs.85%29.aspx) using the [Netwide Assembler](http://www.nasm.us/).

### Prerequisites

Before starting, you'll need some software. I've used the following software set, however you can use any linker and resource compiler that you like.

* [Nasm](http://www.nasm.us/)
* [ALink](http://alink.sourceforge.net/)
* [Gorc](http://www.godevtool.com/)
* [Win32 include file](ftp://ftp.szif.hu/pub/demos/tool/w32nasm.zip)

You'll use *Nasm* to reduce your assembly source code into [COFF](https://support.microsoft.com/en-us/kb/121460) object files. *Gorc* will take your resource scripts and produce linkable object files from these. Finally, *ALink* will bind all of your object files into a windows executable.

Finally, you're going to need a copy of the include file for the Win32 API. The API itself is huge; the number of constants and structures is mind boggling. The link above handles all of these for you.

### Test program

Probably the easiest thing to accomplish, is showing a message box. You need to show the message box and then return control back to Windows. You do this with calls to `MessageBoxA` and `ExitProcess`. The "A" in `MessageBoxA` as we're not dealing with the wide-char version of these functions.

Here's the code.

{% highlight asm %}
%include "win32n.inc"

extern MessageBoxA
import MessageBoxA user32.dll

extern ExitProcess
import ExitProcess kernel32.dll

segment .data USE32

	title	db "A message for you", 0
	message db "This is your first message", 0

segment	.bss USE32

segment .code USE32

..start:

	; show the message box
	push MB_OK
	push title
	push message
	push 0
	call [MessageBoxA]

	; return control back to windows
	push 0
	call [ExitProcess]
{% endhighlight %}

Functions are imported from the api using `import`, and are called in a very assembler-traditional fashion here. Taking a look at [the definition](https://msdn.microsoft.com/en-us/library/windows/desktop/ms645505%28v=vs.85%29.aspx) for the `MessageBox` function, we can see the order of parameters:

{% highlight c %}
int WINAPI MessageBox(
  HWND    hWnd,
  LPCTSTR lpText,
  LPCTSTR lpCaption,
  UINT    uType
);
{% endhighlight %} 

Arguments are pushed to the stack in reverse order.

### Assembling and linking

Now that you've got your source file, `hello.asm` you can produce an object file with the following:

{% highlight text %}
C:\src> nasm -i c:\nasm\include -f obj hello.asm
{% endhighlight %}

You can now link the object file into an executable with the following:

{% highlight text %}
C:\src> alink -c -oPE -subsys gui hello
{% endhighlight %}

Ready to go.

### Making things a little more high-level

You can make your assembly code a little more high-level by using the [nagoa+.inc](http://mathimaaran.angelfire.com/nagoa.zip) include file. This include file provides your programs with some really handy constructs (as well as the win32 api bindings), so function invocations now look like this:

{% highlight asm %}
call MessageBoxA, 0, message, title, MB_OK
call ExitProcess, 0
{% endhighlight %}

### Conclusion

This will get you started at least with *Nasm* in Windows development. [Here](http://rs1.szif.hu/~tomcat/win32/) is a great resource, full of links on assembly development.
