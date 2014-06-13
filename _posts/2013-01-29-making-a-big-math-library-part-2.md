---
layout: post
title: Making a Big Math Library (Part 2)
date: 2013-01-29
comments: false
---

### Introduction

In the [previous post]({% post_url 2013-01-28-making-a-big-math-library %}), we had setup a basic array data type to act as a very big number. We implemented an initialise, increment and decrement function. Today we'll extend on this library by adding some zero testing. It is of interest to us when our numbers are zero and when they aren't.

### Scanning our number

The plan of attack is to use a scan string quad-word `scasq`. We're going to enumerate our array, testing each block for zero. If we make it to the end of the array, we have a zero number. If our scanning got interrupted by a non-zero number, our overall number couldn't possibly be zero.

Here's the code.

{% highlight nasm %}
; --------------------------------------------
; bignum_is_zero                              
; determines if a big number is zero          
;                                             
; input                                       
; rsi - address of the number                 
; --------------------------------------------
                                              
_bignum_is_zero:                              
  push  rcx                   ; save off any registers that                                   
  push  rbx                   ; we'll use in this function
  push  rdi                                  
                                              
  mov   rdi, rsi              ; setup the destination pointer
                              ; for the scan operation
  mov   rcx, bignum_size      ; the number of blocks that we'll scan
  xor   rax, rax              ; the value that we'll be scanning for                   
  repe  scasq                 ; while we're still finding the value in
                              ; rax at [rdi], keep scanning
                                              
  cmp   rcx, 0                ; if we exhausted our counter at the end
                              ; of the scan, we have a zero number

  je    _bignum_is_zero_yes   ; jump to a true state               
                                              
  xor   rax, rax              ; rax = 0 = false, non-zero               
  jmp   _bignum_is_zero_done                 
                                              
_bignum_is_zero_yes:                          
  mov   rax, 1                ; rax = 1 = true, zero               
                                              
_bignum_is_zero_done:                         
                                              
  pop   rdi                   ; restore our saved registers               
  pop   rbx                                  
  pop   rcx                                  
                                              
  ret                                        
{% endhighlight %}

Now that we've got this function, we can wrap it up with a print so we can visually see if a number is zero or not. This will also give you basic usage of the function above.

{% highlight nasm %}
print_zeroness:                                 
  ; define the strings that we'll show to screen
  defsz zero_msg, "The number was zero"        
  defsz non_zero_msg, "The number was not zero"
  
  ; test the number that's at [esi]
  call  _bignum_is_zero            

  ; check if it "is zero", 
  cmp   rax, 1             

  ; jump over if it wasn't
  jne   print_zeroness_not_zero                
  
  ; print the "was zero" message
  print zero_msg                               
  jmp   print_zeroness_done                    
                                                
print_zeroness_not_zero:                        
  
  ; print the "wasn't zero" message
  print non_zero_msg                           
                                                
print_zeroness_done:                            
  ret                                          
{% endhighlight %}

### In action

Now that we've got a little helper function to wrap up our is-zeroness and reporting on this to the console, we're free to test it out as follows.

{% highlight nasm %}
mov   rdi, num_a      ; zero out our number     
call  _bignum_zero   
                        
mov   rsi, num_a      ; test if it is zero
call  print_zeroness 
                        
mov   rdi, num_a      ; increment our number
call  _bignum_inc    
                        
mov   rsi, num_a      ; re-test if it's zero
call  print_zeroness 
{% endhighlight %}

To which we end up with a result looking like this.

{% highlight bash %}
$  bigmath  ./bigmath
The number was zeroThe number was not zero                               
{% endhighlight %}

As expected. Initially the number was zero, after incrementation is was not.

There's some basic logic testing for your big number.