---
layout: post
title: Making a Big Math Library
date: 2013-01-28
comments: false
---

### Introduction

In most of your day to day applications, numbers that fit within 8, 16, 32 and 64 bits are enough. Sometimes you may need to stretch out into your floating point processor to include some more precision and start looking at 80 bit numbers. Today's post, I want to start on a library in assembly language that works on even bigger numbers. There are going to be some caveats or conditions that we'll establish up front, but they will be big numbers! The design considerations for this library will be as follows:

* Unsigned integers
* Fixed size

With this information, we can now start to build our library.

### The big number data type

Well, it's not really that special at all. We just need to define an array of quad-words to the length that we want. So, an example would look like this.

{% highlight nasm %}
bignum_size EQU 10        ; bignums will be 640bits in size
                                            
num_a resq bignum_size    ; make our bignum variable
{% endhighlight %}

That's all pretty simple. A number is just an array of a pre-defined length. With a length to 10, at 64 bits per array index we've got ourselves a 640 bit number. Quite large. In fact, we're talking numbers up to 4.562441e+192. Now that's impressive.

### Lets operate!

We've got a very impressive data type that we can declare where ever we want, but how do we use it? We need to define all of the management, arithmetic, logic and printing ourselves. First of all though, we'll want to be able to zero out our number, which is just continually writing zeros over our memory buffer (much like a `memset`).

{% highlight nasm %}
; --------------------------------------------
; bignum_zero                                 
; initialize a bignum to zero                 
;                                             
; input                                       
; rdi - address of the number                 
; --------------------------------------------
                                              
_bignum_zero:                                 
  push  rax               ; save any registers                                  
  push  rcx               ; that we'll mow over                   
  push  rdi                                  
                                              
  xor   rax, rax          ; the value we want to set to                   
  mov   ecx, bignum_size  ; the number of quads to write                   
  rep   stosq             ; clear out our number                   
                                              
  pop   rdi               ; restore any registers that                   
  pop   rcx               ; we saved                   
  pop   rax                                  
                                              
  ret                                        
{% endhighlight %}

Excellent. We can initialize our big number now to zero so that we have a starting point. Next, we'll want to be able to debug any problems that we may have while building this library, so its going to be of great benefit to be able to see what the value is of our number. The following code leans on some assembly I had written in a [previous artivle]({ post_url 2013-01-08-printing-a-register-s-value-in-hex }) which displays the value in `RAX`. Here's how we'll print our big number to screen.

{% highlight nasm %}
; --------------------------------------------
; bignum_print                                
; prints a bignum to the console             
;                                             
; input                                       
; rsi - address of the number                 
; --------------------------------------------
                                              
_bignum_print:                                
  push  rax                 ; save off any values                         
  push  rbx                                  
  push  rcx                                  
                                              
  mov   rcx, bignum_size    ; we'll print all blocks

next_block:                                   

  mov   rbx, rcx            ; setup our base pointer                             
  dec   rbx                 ; to point to the block corresponding                 
  shl   rbx, 3              ; to our counter rcx

  mov   rax, [rsi + rbx]    ; get the value in rax                 
                                              
  call  _print_rax          ; print it                 
                                              
  dec   rcx                 ; move onto the next block                 
  jnz   next_block          ; keep printing                 
                                              
  pop   rcx                 ; restore any saved registers                 
  pop   rbx                                  
  pop   rax                                  
  ret                                        
{% endhighlight %}

Most obvious thing to note here is that we're moving through the bignumber back-to-front so that it's presented on screen in a human manner. We can test what we have so far is working by writing a small piece of test code.

{% highlight nasm %}
; zero out our number
mov   rdi, num_a   
call  _bignum_zero  

; print it to screen
mov   rsi, num_a   
call  _bignum_print
{% endhighlight %}

Which, rather uninterestingly will just spit out a massive list of zeros.

{% highlight text %}
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
{% endhighlight %}

So, lets do something a little more interesting. The smallest pieces of arithmetic that I think we can implement for these numbers is increment and decrement.

### Increment

This is straight forward. Add 1 to the lowest part of the number. If that part of the number just "clocked", or reset to zero - we must carry our value onto a higher block.

{% highlight nasm %}
; --------------------------------------------
; bignum_inc                                  
; increments a big number by 1                
;                                             
; input                                       
; rdi - address of the number                 
; --------------------------------------------
                                              
_bignum_inc:                                  
  push  rcx                       ; save off any registers                                  
  push  rbx                                  
  push  rax                                  
                                              
  mov   rcx, bignum_size          ; worst case, we'll have to
                                  ; go through all blocks

  xor   rbx, rbx                  ; reset our base to be zero                             
                                              
_bignum_inc_carry:                            
  mov   rax, qword [rdi + rbx]    ; get this block into rax           
  inc   rax                       ; increment it
  mov   qword [rdi + rbx], rax    ; store it back           
  cmp   rax, 0                    ; check if we "clocked"           
  jne   _bignum_inc_done          ; if we didn't, we're finished           
                                              
  add   rbx, 8                    ; move the base onto the next block           
  dec   rcx                       ; 1 less block to process           
  jnz   _bignum_inc_carry         ; continue until we're finished           
                                              
_bignum_inc_done:                             

  pop   rax                       ; restore any of the saved values           
  pop   rbx                                  
  pop   rcx                                  
  ret                                        
{% endhighlight %}

Alright, we've got incrementation going on here. The most interesting case for us to test is when a block is sitting on the carry boundary or sitting at `0xffffffffffffffff`. After we increment that, we should see that clear out to zero and the next block to get a 1.

{% highlight nasm %}
mov   rdi, num_a                     
call  _bignum_zero                   
mov   qword [rdi], 0xffffffffffffffff   ; setup out number on the boundary
                                     
mov   rsi, num_a                     
call  _bignum_print                     ; print it out
                                     
mov   rdi, num_a                        ; push it over the boundary with
call  _bignum_inc                       ; an increment 
                                     
mov   rsi, num_a                        ; print out the new number
call  _bignum_print                  
{% endhighlight %}

Which results in the following.

{% highlight text %}
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000FFFFFFFFFFFFFFFF
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010000000000000000
{% endhighlight %}

Those massive trails of zeros are getting annoying. We can clean them up later by making our print routine a little smarter. But you can see that we've successfully incremented over a boundary so that our next higher block gets a one.

### Decrement

This is pretty much as straight forward as incrementing, on this time we're not going to carry to a higher quad-word, we're going to borrow from it if we do cross a boundary. Here's the code for a decrement.

{% highlight nasm %}
; --------------------------------------------
; bignum_dec                                  
; decrements a big number by 1                
;                                             
; input                                       
; rdi - address of the number                 
; --------------------------------------------
                                              
_bignum_dec:                                  
  push  rcx                       ; save off any registers                                  
  push  rbx                                  
  push  rax                                  
                                              
  mov   rcx, bignum_size          ; worst case, we'll need
                                  ; to go through all blocks

  xor   rbx, rbx                  ; clear out the base                             
                                              
_bignum_dec_borrow:                           
  mov   rax, qword [rdi + rbx]    ; get the current quad           
  dec   rax                       ; decrement it           
  mov   qword [rdi + rbx], rax    ; store it back in the number           
  cmp   rax, 0xffffffffffffffff   ; check if we went across a boundary           
  jne   _bignum_dec_done          ; if not, get out now           
                                              
  add   rbx, 8                    ; move onto the next block           
  dec   rcx                       ; not done yet?           
  jnz   _bignum_dec_borrow        ; keep processing borrows           
                                              
_bignum_dec_done:                             
  pop   rax                       ; restore the saved registers           
  pop   rbx                                  
  pop   rcx                                  
  ret                                        
{% endhighlight %}

This really is the increment routine just with the numeric direction reversed. Making sure that we are on the right track, we can test out the decrement across a boundary again.

{% highlight nasm %}
mov   rdi, num_a                        ; setup our number right                     
call  _bignum_zero                      ; on the boundary
mov   qword [rdi], 0xffffffffffffffff
                                     
mov   rsi, num_a                        ; print its current state
call  _bignum_print                  
                                     
mov   rdi, num_a                        ; increment over the boundary
call  _bignum_inc                    
                                     
mov   rsi, num_a                        ; print its state
call  _bignum_print                  
                                     
mov   rdi, num_a                        ; decrement it over the boundary
call  _bignum_dec                    
                                     
mov   rsi, num_a                        ; print its state
call  _bignum_print                  
{% endhighlight %}

The output of which looks like this.

{% highlight text %}
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000FFFFFFFFFFFFFFFF
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000FFFFFFFFFFFFFFFF
{% endhighlight %}

Again, it's pretty hideous with the trailing zeros, but you can see this in action now. Back and forth, across boundaries - no sweat.

### Conclusion

Well, this is just the tip of the iceberg for this particular library. I'd hope to demonstrate logical testing, addition, subtraction, multiplication and division as well shortly. The best part about these routines is that if you wanted to operate on 6400 bit numbers, all you have to do is change the constant `bignum_size` that I'd set earlier to 100.