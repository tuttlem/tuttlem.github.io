---
layout: post
title: Windows Development with MASM
date: 2015-09-12
comments: false
categories: [ "windows", "win32", "masm", "assembly" ]
---

In this post, I'll walk through the steps required to bootstrap your development experience against the [Win32 API](https://msdn.microsoft.com/en-us/library/windows/desktop/ff818516%28v=vs.85%29.aspx) using the [Microsoft's Macro Assember](http://www.masm32.com/).

### Prerequisites

Downloading the package from the [masm32.com](http://www.masm32.com/) site will be everything that you need to produce applications using assembly language. It not only includes the assember `ml`, but also includes all of the library, include files and linker for you.

### Test program

Here's the code to present a message box.

{% highlight asm %}
; #######################

  .386
  .model flat, stdcall
  option casemap :none   ; case sensitive

; #######################

  include windows.inc
  include user32.inc
  include kernel32.inc

  includelib user32.lib
  includelib kernel32.lib

; #######################

.data

  szTitle  db "Your message", 0
  szMsg    db "First message box", 0

.code

start:

  invoke MessageBox, 0, offset szMsg, offset szTitle, MB_OK
  invoke ExitProcess, 0

end start
{% endhighlight %}

`invoke` goes a long way to cleaning up how any assembly program that is sub-routine heavy looks. The include files already do the externing of library symbols so that your code doesn't need to explicitly declare that you're using a particular function. 

### Assembling and linking

Now that you've got your source file, `hello.asm` you can produce an object file and executable with the following:

{% highlight text %}
C:\src> ml.exe /IC:\masm32\include /coff hello.asm /link /SUBSYSTEM:WINDOWS /LIBPATH:C:\masm32\lib
{% endhighlight %}

You can see that not only assembler switches are passed, but also linker switches which means you're assembling and linking all in one step.

### Conclusion

The bundle for masm32 that this article refers to is great in the amount of code that it offers you, but the provided assembler is well-dated. Microsoft have been keeping masm updated from their website with 64 bit versions available with their development tools.
