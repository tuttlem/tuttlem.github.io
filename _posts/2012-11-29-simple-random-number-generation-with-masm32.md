---
layout: post
title: Simple random number generation with MASM32
date: 2012-11-29
comments: false
categories: [ "Assembly", "MASM32", "Programming", "random", "random numbers"]
---

Here's a little tid-bit that I'd like to make note of. In a previous MASM32 windows development tutorial, I needed a random number generator and was able to dig this one up from the archives.

First of all, you need to make sure we're using the 586 instruction set - at least!

{% highlight nasm %}
; minimum instruction set requirement
.586
{% endhighlight %}

Shortly, it will become clear why we need the 586 instruction set. To support the random number generator, we need a bit of state in the shape of two double-words.

{% highlight nasm %}
; calculation state
prng_x  DD 0

; current seed
prng_a  DD 100711433
{% endhighlight %}

These two variables allow the random number generator to know where it's up to for the next iteration. Changing the initial seed of course will change the sequence of random values that are produced by this routine. The actual function that produces the random number look like this:

{% highlight nasm %}
PrngGet PROC range:DWORD

    ; count the number of cycles since
    ; the machine has been reset
    rdtsc

    ; accumulate the value in eax and manage
    ; any carry-spill into the x state var
    adc eax, edx
    adc eax, prng_x

    ; multiply this calculation by the seed
    mul prng_a

    ; manage the spill into the x state var
    adc eax, edx
    mov prng_x, eax

    ; put the calculation in range of what
    ; was requested
    mul range

    ; ranged-random value in eax
    mov eax, edx

    ret

PrngGet ENDP
{% endhighlight %}

There it is. That first instruction [RDTSC](http://ref.x86asm.net/coder32-abc.html#R) is what we need to turn the 586 instruction set on for. This mnemonic stands for Read Time-Stamp Counter. It will take the number of cycles since the machine has been reset and put this count into the `EDX`:`EAX` pair (as the time stamp counter is a 64bit value). From there some calculations are performed to ensure the consistency of state between random value retrieves and also capping to the requested range.

Simple and it works too!
