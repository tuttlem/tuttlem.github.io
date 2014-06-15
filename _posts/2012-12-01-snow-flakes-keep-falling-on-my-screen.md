---
layout: post
title: Snow flakes keep falling on my... screen?
date: 2012-12-01
comments: false
categories: [ "mode 13", "assembly", "demo", "snow flake" ]
---

A very simple effect this time around. It's snow flakes. The operating premise for the effect is very simple and goes like this:

* Generate 1 new snow flake at the top of the screen at every frame
* A snow flake has an absolute floor of the last line in video memory
* A snow flake should come to rest if it lands on top of another

![Snowflakes](http://1.bp.blogspot.com/-pE0K1mfiABg/ULoEn3kZ6bI/AAAAAAAAAiQ/BVre5BA5iy0/s400/Screen+Shot+2012-12-01+at+11.10.27+PM.png)

That's it! So, immediately we need a way to get random numbers. We're using a 320x200 screen here and my dodgy routine for getting random numbers only returns us 8 bit numbers (which gets us to 255). We need to add some more width to these numbers if we expect to be able to randomize across the whole 320 column positions. Calling the random port twice and adjusting the resolution of the second number should do it for us, such that:

8 bits (256) and 6 bits (64) will give us 320 - or the ability to add using 14 bits worth of numbers, which in this configuration takes us up to 320. Perfect.

Here's the code!

{% highlight nasm %}
get_random:
    ; start out with ax and bx = 0
	xor	ax, ax
	xor     bx, bx

    ; get the first random number and
    ; store it off in bl
	mov	dx, 40h
	in	al, dx
	mov	bl, al

    ; get the second random number and
    ; store it off in al, but we only 
    ; want 6 bits of this number
	mov	dx, 40h
	in	al, dx
	and     al, 63

    ; add the two numbers to produce a
    ; random digit in range
	add	ax, bx
	
	ret
{% endhighlight %}

Excellent. We can span the breadth of our screen with random flakes. Now it's time to progress them down the screen. Here's the main frame routine to do so.

{% highlight nasm %}
no_kbhit:

    ; put a new snowflake at the top 
    ; of the screen
	call	get_random
	mov	di, ax
	mov	byte ptr es:[di], 15

decend:

    ; we can't move snowflakes any further
    ; than the bottom of the screen so we
    ; process all other lines
	mov	di, 63680
	mov	cx, 63680
	
next_pixel:

    ; test if there is a snowflake at the 
    ; current location
	mov	al, es:[di]
	cmp	al, 0
	je	no_flake
	
    ; test if there is a snowflake beneath
    ; us at the moment
	mov	al, es:[di+320]
	cmp	al, 0
	jne	no_flake
	
    ; move the snowflake from where we are 
    ; at the moment to one line below us
	xor	al, al
	mov	byte ptr es:[di], al
	mov	al, 15
	mov	byte ptr es:[di+320], 15
	
no_flake:

    ; move our way through video memory
	dec	di
	dec	cx
	jnz	next_pixel

    ; check for a keypress
	mov	ah, 01h
	int	16h
	jz	no_kbhit
{% endhighlight %}

The code itself above is pretty well commented, you shouldn't need me to add much more here. There are a couple too many labels in the code, but they should help to add readability. I'll leave it as an exercise to the reader to implement different speeds, colours and maybe even some horizontal movement (wind). Cool stuff.