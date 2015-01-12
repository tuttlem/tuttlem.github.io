---
layout: post
title: Fixed point numbers
date: 2015-01-10
comments: false
categories: [ "fixed", "math", "asm" ]
---

In today's post, I'm going to go over a very old technique for working with fractional numbers. [Fixed point numbers](http://en.wikipedia.org/wiki/Fixed-point_arithmetic) are defined by dividing an integral data type (8, 16, 32 or 64 bit) word into a whole part and fractional part.

### The idea

So, what you do is you need to split your number into a whole part and fractional part. This means you dedicate some bit-space of your word to both. For a simple example, let's work with an byte (traditioanlly 8 bits) and split the number evenly giving the whole part 4 bits as well as the fractional part.

{% highlight text %}
0 0 0 0 0 0 0 0 
------- -------
   |       |
 Whole   Frac
{% endhighlight %}

Above, we see our number represented in binary with all 8 bits laid out. The upper 4 bits are dedicated to the whole part, the lower 4 bits to the fractional. 

### The whole part

In this scenario (of a 4:4) split, you've probably already worked out that the maximum whole value we can hold is 15. Some example numbers as they appear might be:

{% highlight text %}
0 0 0 1 0 0 0 0 = 1.0
0 1 0 1 0 0 0 0 = 5.0
1 1 1 1 0 0 0 0 = 15.0
{% endhighlight %}

First thing you'll notice is, every number is effectivly shifted 4 bits to the left. This is how you load a fixed point number into memory. You shift the original integer to the left by the number of bits you have allocated for the fractional part. 

Nothing overly special here. The fractional part is a little different to think about, but still makes sense.

### The fractional part

As above, we've dedicated 4 bits to our fractional part as well. With binary numbers, the value of the bit slots go up in powers of two; going in the opposite direction they also go down in powers of two. With normal numbers, bit slot 0 actually corresponds to 2^0. By sliding this bit slot up by four, we effectivly start counting negative powers into the fractional part:

{% highlight text %}
0 0 0 0 1 0 0 0 = 2^(-1) = 0.5
0 0 0 0 0 1 0 0 = 2^(-2) = 0.25
0 0 0 0 0 0 1 0 = 2^(-3) = 0.125
0 0 0 0 0 0 0 1 = 2^(-4) = 0.0625
{% endhighlight %}

Using this knowledge, we can start to treat the lower 4 bits as the fractional part of the number. Even bit shifting (multiplication by 2) works as you'd expect:

{% highlight text %}
0 0 0 0 1 0 0 0 = 2^(-1) = 0.5

0 0 0 1 0 0 0 0 = 2^(0)  = 1

0.5 * 2 = 1
{% endhighlight %}

All we did was shift the first number by 1 slot to the left. This has the overall effect of multiplying the number by 2 - makes sense, we're in binary after all. 

Ok, let's take a look at how some simple operations work in code.

### Show your working

Working with a 4:4 fixed point number is fine for pen-and-paper type stuff, but we'll want to scale that up once we start to work with code. We'll target a 16 bit number now, using the lower 6 bits for fractionals so the number is now a 10:6.

First off, we'll want to be able to load our numbers. As above, this is as simple as shifting the numbers left by the number of bits we have dedicated to the fractional portion of our number. In this case, 6.

{% highlight asm %}
mov		ah, 5		
shl		ah, 6
{% endhighlight %}

Of course, to get our whole number back again; all we do is shift a fixed point number to the right by the same number of bits again. Getting the fractional part of the number back out, again, depends on how many bits you have dedicated to the number's precision (or fractional part). Because we've dedicated 6, this means the smallest number (and therefore the most accurate precision) we can use is actually 0.015625. We only have 6 bits precision, which gives us a range of 0-63. 

This formula is what you'd use to get the fractional part of our number:

{% highlight text %}
2^(-(number-of-bits)) * (lower-number-of-bits)
{% endhighlight %}

So, if we had the number 0.25 which would be represented by `0000000000010000`, we can extract 0.25 from this using the formula above:

{% highlight text %}
lower 6 bits = 010000 = 16
2^(-6) * 16 = 0.25
{% endhighlight %}

Addition and subtraction are handled just like normal. Nothing special needs to happen here. Multiplication and Division both require a little more effort.

Once you've performed your multiplication, you need to adjust the product by the number of fractional bits to the right. 

{% highlight asm %}
mov     ax, 10      ; 10 * 10
shl     ax, 6
mov     bx, 10      
shl     bx, 6

imul    bx          ; perform the multiplication
                    ; result will now be in dx:ax

add     ax, 32      ; add the bit boundary
adc     dx, 0       ; adjust for any carry

shrd    ax, dx, 6   ; shift the top 6 bits of dx into ax
{% endhighlight %}

Division requires a little more care as well. We pre-prepare the numbers by shifting them by the whole part prior to operation.

{% highlight asm %}
mov     dx, 10      ; 10 / 4
shl     dx, 6
mov     bx, 4
shl     bx, 6

xor     ax, ax		  ; ax = 0      
shrd    ax, dx, 10  ; shift the top 10 bits of dx into ax
sar     dx, 10      ; shift and rotate 10 

idiv    bx			  ; perform the division
{% endhighlight %}

Whilst these operations are more complex than their natural analogues, you can see that there's not that much too them.

### Conclusion

All of this information is <strong>old</strong>, and that's the point. Make sure you check out the [fixed point writeup](http://www.textfiles.com/programming/fix1faq.txt) hosted by [textfiles.com](textfiles.com).

