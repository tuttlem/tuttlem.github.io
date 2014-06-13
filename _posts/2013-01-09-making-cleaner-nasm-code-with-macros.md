---
layout: post
title: Making Cleaner NASM Code with Macros
date: 2013-01-09
comments: false
---

### Introduction

Cleaner, clearer code is better. It's easier to debug, it's easier to read, it's just plain - better. Assembly code isn't known for its ability to allow the developer to make their intentions clear in its source code, but we can get closer with some carefully craft macros. Macros are symbols that you can use in your code to represent a block of code. Macros are allowed to take parameters which makes them an extremely flexible and valuable tool in your arsenal. These symbols that you use in your code are swapped out by nasm at the time of assembly for the blocks of code that they represent. If you want to go further in depth to the nasm pre-processor and macros, check it out in the manual [here](http://www.nasm.us/doc/nasmdoc4.html).

Today's post will be focused on cleaning up the code that we'd written in [this]({ post_url 2013-01-09-strlen-implementation-in-nasm }) previous article to look a little more human.

### Revisiting write and strlen

In the previous article "[strlen() implementation in NASM]({ post_url 2013-01-09-strlen-implementation-in-nasm })", we'd put together a couple of ways to take the length of a string. This article will assume that we're already using this code. With this in mind, we can put together a general purpose print function that will display a zero terminated string with the following.

{% highlight nasm %}
; _print
;
; input
; rdi points to the zero terminated string that 
;     we're printing
;
; output
; none

_print:

  push  rcx       ; start off by preserving the registers
  push  rdx       ; that we know that we'll trash in this
  push  rax       ; proc
  push  rbx

  mov   rcx, rdi  ; rcx = string memory location
  call  _strlen   ; calculate the string's length
  mov   rdx, rax  ; rdx = string length
  mov   rax, 4    ; write() is syscall 4
  mov   rbx, 1    ; we're writing to stdout
  int   0x80      ; execute the call

  pop   rbx       ; restore all of the registers
  pop   rax
  pop   rdx
  pop   rcx

  ret             ; get out
{% endhighlight %}

Ok, that's a nice and neat little bundle. Now, everytime that we want to call this function, we need to write code that looks like the following.

{% highlight nasm %}
mov  rdi, message    ; load our string into rdi
call _print          ; write the message
{% endhighlight %}

Which, isn't too bad I guess. We can make it look better though. Consider the following code that wraps this code into a macro.

{% highlight nasm %}
; print - prints a null terminated string
%macro print 1
  push  rdi         ; save off rdi

  mov   rdi, %1     ; load the address of the string
  call  _print      ; print the string

  pop   rdi         ; restore rdi
%endmacro
{% endhighlight %}

The syntax here may look a little alien to begin with, but it'll all make sense in a minute. So, we start the macro block off with a `%macro` directive. What follows is the name of the macro, in this case `print` and after that is the number of parameters that this macro will expect (we want one parameter, being the string to print). We have the print code between the directives. You'll see that `rdi` gets loaded with `%1` which just means "replace `%1` with the first thing passed to this macro". To finish up your macro, you have `%endmacro`. With that macro defined, you can now print a message to screen by doing this.

{% highlight nasm %}
print message
{% endhighlight %}

This is starting to look a little higher-level now. A bit more "human" on the eyes. Another nifty trick that I'd picked up a while ago was a macro for defining strings. In all of the examples we've seen so far, you'd declare strings in the data segment with the following syntax.

{% highlight nasm %}
section .data

  message db "Hello, world", 0
{% endhighlight %}

This is perfectly fine, however we can wrap this string declaration up into a macro of its own as well allowing us to define strings where ever we are. We need to be careful though. Defining a string in the code segment without the appropriate jumps is dangerous as we run the risk of executing the string data. The following macro does this safely.

{% highlight nasm %}
; sz - defines a zero terminated string
%macro sz 2
  jmp %1_after_def    ; jump over the string that we define
  %1 db %2, 0         ; declare the string
  %1_after_def:       ; continue on
%endmacro
{% endhighlight %} 

You can see that we've declared a macro that expects two parameters. The first parameter is the name of the variable that we declare. This name is also used to formulate the labels that we jump to so that they are unique between string definitions. The second parameter is the actual string data itself. Now that we have both of these macros defined, the following code is perfectly legal and works a treat.

{% highlight nasm %}
sz message "This is much more interesting than Hello, World!"
print message
{% endhighlight %}

Well, this is only the start of what you can accomplish with macros. An exercise to the reader would be to implement your own version of `print` that prints a new line after it prints the string - you never know, you might even want to call it "println"!

Enjoy.