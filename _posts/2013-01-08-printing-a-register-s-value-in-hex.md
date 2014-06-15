---
layout: post
title: Printing a Register's Value in Hex
date: 2013-01-08
comments: false
categories: [ "Assembly", "Programming", "Print", "Register Value" ]
---

### Introduction

Putting some of the knowledge we've picked up in this [previous post]({% post_url 2013-01-08-strlen-implementation-in-nasm %}), today'ss post is going to be about getting the value that sits in a register out on screen. This post will assume that we're not going to lean on a call like `printf`, we're going to do it by hand.

### How to attack the problem?

The solution that I present may immediately strike your eye as verbose & long-winded and this is on purpose. We're going to build something that works to begin with then we can do an analysis of what we've written and optimise it later. I've split the larger problem of printing a register's value (in this case we're printing `RAX`) into a few smaller problems so as we knock off each problem, we get closer to an overall result. The sub-problems that I have cut this into are:

* Printing a nibble (half a byte or 4bit value)
* Printing a byte
* Printing the register value

So you can see that we're going to implement a solution by solving these smaller issues top to bottom. Let's take a look at some code.

### Characters and Nibbles

We're going to print a nibble. A nibble is 4 bits of data spanning values from 0 up to F hexadecimal or 0 up to 15 in decimal. The plan of attack is to isolate this 4 bits in such a way that we can use it as an offset into a string of characters organised from 0 up to F. Here's the code.

{% highlight nasm %}
section .data                        
                                     
   hex_chars   db "0123456789ABCDEF"  ; our hex char lookup string
                                     
section .text                        

; it's assumed that the lower 4 bits of al contains
; the nibble that we want to print. rax really needs
; to be zeroed out elsewhere for this to work

_print_nibble:          
                        
  push  rsi                 ; save off any of the register that 
  push  rax                 ; we know that we'll destroy in this
  push  rdi                 ; procedure
  push  rdx            
                        
  mov   rsi, hex_chars      ; load the base address
  add   rsi, rax            ; offset the base address by the value
                            ; we want to print, therefore indexing
                            ; the character

  mov   rax, 0x2000004      ; write() syscall
  mov   rdi, 1              ; write out to stdout
  mov   rdx, 1              ; write 1 byte!
  syscall                   ; make the call
                        
  pop   rdx                 ; restore all of the registers that
  pop   rdi                 ; we saved at the start
  pop   rax            
  pop   rsi            
                        
  ret                       ; get out
{% endhighlight %}

The code is documented pretty well, and you can see that the crux of the work is just offsetting the base address of the string by the nibble that we want to print. Nifty. Keep in mind that the registers used here are assuming that you're compiling for OSX. If you are compiling for another type of unix make sure that the parameters are being passed through the correct registers, otherwise you'll be segfaulting all the way to the pub!

### Stepping up to a byte

Now we want to chain two `_print_nibble` calls together so that we can print an entire byte out on the screen (0 up to FF). We've already got a procedure that prints the lower 4 bits of al out to the screen, all we really need to do is be creative with al so we can print the higher 4 bits first then the lower 4 bits so that the number comes out to the console in the right order! Here's the code.

{% highlight nasm %}
_print_al:              
                        
  push  rbx           ; we know that rbx will get a touch
                      ; up here, so save it off
  
  mov   bl, al        ; take a copy of the whole byte and 
  shr   al, 4         ; move the top 4 bits into the lower
                      ; 4 bits of al ready for printing
                        
  call  _print_nibble ; print the lower 4 bits of al 
                        
  mov   al, bl        ; restore al again 
  and   al, 0xf       ; isolate the lower 4 bits of al
                        
  call  _print_nibble ; print the lower 4 bits of al 
                        
  pop   rbx           ; restore rbx
                        
  ret                 ; get out 
{% endhighlight %}

This function holds the same assumption as printing a nibble. There can't be any junk in the higher bits (from `al`) of `rax` otherwise this solution will turn to mud.

### Going the whole hog!

We're now able to print any byte we would like, so lets string 8 bytes together to make a 64bit integer that we can print. Again, it's all about shuffling the value that we want to print around correctly so that the number is written to the console in the correct order. It might be confusing to see pushes and pops inside of the loop that I'll present, but I re-use these registers to calculate things on the fly. Again, I've commented this code pretty verbosely so it should read like a bedtime story. Here's the code.

{% highlight nasm %}
_print_rax:          
                     
  mov   rcx, 8      ; there are 8 bytes to print      
                     
_next_byte:          
                     
  push  rax         ; store off the value to print
  push  rcx         ; store off the byte count

  dec   rcx         ; make rcx zero based

  shl   rcx, 3      ; transform rcx so that it will
                    ; hold the number bits that we
                    ; shift out value by so we can
                    ; isolate the correct byte

  shr   rax, cl     ; isolate the correct byte in al     
  and   rax, 0xff   ; make sure there is nothing in 
                    ; the upper parts of rax
                     
  call  _print_al   ; print the value in al
                     
  pop   rcx         ; restore the counter 
  pop   rax         ; restore the value we're printing
                     
  dec   rcx         ; move onto the next byte
  jnz   _next_byte  ; process the next byte if we 
                    ; haven't yet finished

  ret               ; get out!               
{% endhighlight %}

The key is byte isolation. Using `rcx` we can count from the top byte down to the bottom with creative shifting. Now that we've implemented all of this code, we can print some boobies to the screen. This is the moment you've been waiting for.

{% highlight nasm %}
_start:                          
                                 
  mov   rax, 0xb000b135b000b135  ; the value to print 
  call  _print_rax               ; print them to screen
                                 
                                 
  mov   rax, 0x2000001           ; exit syscall
  mov   rdi, 0                  
  syscall                       
{% endhighlight %}

The output of which should just print "B000B135B000B135" to the console. Yes, there's boobies in the article, see! Whilst this may not appear to be the most useful function right now, it'll serve as a very useful debugging tool for us in the future.