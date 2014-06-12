---
layout: post
title: strlen() implementation in NASM
date: 2013-01-08
comments: false
---

### Introduction

Seeing so many "Hello, world" concepts for getting up an running in Assembly has annoyed me a little bit. I see people using the `$ - msg` macro to calculate the length of their string at assemble time. In today's post, I'll show you how to measure the length of your string at runtime so you'll be able to provide the [write](http://en.wikipedia.org/wiki/Write_(system_call)) syscall's third parameter a little more flexibly.

### The logic

The logic behind this procedure is dead-simple. Test the current byte for being null, if it is get out now if its not keep counting! Here's how the code looks.

{% highlight nasm %}
_strlen:

  push  rcx            ; save and clear out counter
  xor   rcx, rcx

_strlen_next:

  cmp   [rdi], byte 0  ; null byte yet?
  jz    _strlen_null   ; yes, get out

  inc   rcx            ; char is ok, count it
  inc   rdi            ; move to next char
  jmp   _strlen_next   ; process again

_strlen_null:

  mov   rax, rcx       ; rcx = the length (put in rax)

  pop   rcx            ; restore rcx
  ret                  ; get out
{% endhighlight %}

This is just straight-forward memory testing, no great advancements in computer science here! The function expects that the string that requires testing will be in the `rdi` register. To actually use this function in your application though, you'll need to transport the result (which sits in rax by the time the function has completed execution) into the register that `write` expects its length parameter. Here's how you use your new strlen function (in the Debian scenario).

{% highlight nasm %}
; strlen(hello)
mov   rdi, hello    ; rdi is the string we want to 
                    ; get the length of

call  _strlen       ; get the length!

mov   rdx, rax      ; rdx now holds the string length
                    ; ready for our write syscall

; write(fd, buf, len)
mov   rax, 4        ; syscall 4 == write
mov   rbx, 1        ; fd = 1 == stdout
mov   rcx, hello    ; the string to write
int   0x80          ; print the string
{% endhighlight %}

So you can see that this is quite straight forward. We setup rdx before we setup the rest of the registers. We could have done this the other way around - to be on the safe side, I've done it this way as you never know what registers get mowed over in people's functions. I tried to help this also in the `_strlen` implementation by saving the only work register that I use `rcx`. Anyway, that's how you measure your string.

### A more optimal way?

After completing this article, I'd thought about the "brute-forcish" way that I'd crunched out the numbers to derive a string's length and thought to myself, what if I could just scan the string of bytes - find the null character and subtract this found index from the original starting point. Mathematically I would have calculated the distance in bytes between the start of the string and the `NULL` character, ergo the string length. So, I've written a new string length implementation that does just this and here it is.

{% highlight nasm %}
_strlen2:

  push  rbx                 ; save any registers that 
  push  rcx                 ; we will trash in here

  mov   rbx, rdi            ; rbx = rdi

  xor   al, al              ; the byte that the scan will
                            ; compare to is zero

  mov   rcx, 0xffffffff     ; the maximum number of bytes
                            ; i'm assuming any string will
                            ; have is 4gb

  repne scasb               ; while [rdi] != al, keep scanning

  sub   rdi, rbx            ; length = dist2 - dist1
  mov   rax, rdi            ; rax now holds our length

  pop   rcx                 ; restore the saved registers
  pop   rbx

  ret                       ; all done!
{% endhighlight %}

It may look longer than the first implementation however this second implementation uses [SCASB](http://courses.engr.illinois.edu/ece390/archive/spr2002/books/labmanual/inst-ref-scasb.html) which will be heaps more optimal than my hand-rolled loop.

Enjoy.