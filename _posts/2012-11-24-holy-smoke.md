---
layout: post
title: Holy smoke!
date: 2012-11-24
comments: false
---

Well, not really.

Nostalgia got the better of me today in the form of some good old mode 13 demo code. All the hours I'd blown previously developing little tid-bit apps like this and I never saved off any of my code. Thankfully, I have a good memory and whilst my assembly skills aren't "top shelf", they're certainly up to the task of re-creating this sort of effect.

### Smoke, Fire, Flame?

They're all the same. They work of the same principal. 

* Set a palette that suits your effect (yellows, reds, orange for fire), (black to white for smoke)
* Create some noise as far south as you can on the video buffer
* Blur the pixels out on the screen making your resulting pixels ascend and decay

Easy!

### On to the code ...

So, I'll present this little demo in a couple of chunks and explain them as I show them. The code is pretty well documented anyway so that reading it line for line should be very self-explanitory.

{% highlight nasm %}
.model small
.stack 100h

.code
start:

	mov	ax, 0013h	; set 320x200x256 mode
	int	10h
	
	mov	ax, 0a000h	; we can't directly address ES so 
	mov	es, ax          ; we do so through AX. ES = A000
	
	call	setup_palette	; setup a palette with greyscale 
				; to support the smoke effect

no_kbhit:

	call	randomize_lines	; draw some random pixels 
	call	bloom		; average out the video buffer
	
	mov	ah, 01h		; test for a key press
	int	16h
	jz	no_kbhit	; continue running if no key was hit
	
	mov	ax, 0003h	; set text mode
	int	10h
	
	mov	ax, 4c00h	; return control back to dos
	int	21h

; -- subroutines removed for brevity

end start

end
{% endhighlight %}

This is the main program. It needs to drop us into the required video mode, make sure we don't want to quit (i.e. was there a key hit?), actually perform the effect (the loop of random/average) and then clean up (send us back to text mode, return control to dos).

### Giving the effect some colour

Setting up the palette in this type of routine really does determine the "type" of routine that it is. As I'd said above:

* Purely greyscale will give you a smokey effect on screen
* Gradients running through black, red, orange, yellow, white will give you fire/flame
* Black, blue up to purple will give you a cool alcohol type fire
* Greens will give you something alien

The idea is to experiment with palette creation to see what comes out best for you. Here's how I setup a greyscale palette.

{% highlight nasm %}
setup_palette:

	mov	cx, 255			; 256 colour indicies to set
	
next_colour_idx:
	mov	al, 255			; setup al so that we're setting
	sub	al, cl			; colour indicies from low to high
	
	mov	dx, 3c7h		; this port selects the colour index
					; that we'll set r,g,b for
	out	dx, al
	
	mov	dx, 3c9h		; this port sets the r,g,b components
						; for the selected index
							
	shr	al, 2			; rgb intensities are in range of 0..63
					; so, divide by 4 to adjust
							
	out	dx, al			; set the red
	out	dx, al			; set the green
	out	dx, al			; set the blue

	dec	cx			; move onto the next colour
	jnz	next_colour_idx
	
	ret
{% endhighlight %}

So, just a touch of VGA theory here. Unlike today's video modes, the 256 colour VGA supported 256 indices that each had an RGB intensity set ranging (0..63) each. Sometime, I don't know how we ever used this video mode, but we got by -- and made some damn cool stuff using it. So, software port `3c7` takes a colour index. 3c8 can be used to read the colour intensities (not used in this program). `3c9` is the port we use to write (r,g,b) intensities. Dividing by 4 allows me to interpolate 0..255 against 0..63 so that 0 is the colour with least intensity up to 255 which has the greatest.

###(not so) Random

Getting psuedo random numbers with nothing in the toolbox is difficult. The method that I've used here is to constantly read from [software port 40h](http://www.inversereality.org/tutorials/interrupt%20programming/timerinterrupt.html) which is tightly coupled with the timer interrupt but it keeps a fairly steady count. The uniformity of these numbers actually provides a very tame smoke effect, you're not going to see much chaos.

{% highlight nasm %}
randomize_lines:
	
	mov	cx, 640			; we're going to set two rows of pixels
					; at the bottom of the screen to be 
					; random colours, so that's 640 pixels
							
	mov	di, 63360		; we're going to start writing these 
					; pixels on the last two lines so that's
					; 64000 - 640
							
next_rand_pixel:

	mov	dx, 40h			; we get quasi-random values from port 40h
	in	al, dx
	
	stosb				; store the pixel on screen
	dec	cx			; move onto the next pixel
	jnz	next_rand_pixel
	
	ret
{% endhighlight %}

So, following along with the effect we only randomize the last two rows of the video array. Simple.

### Airbrush, Airbrush!!

It's just an averaging effect. We take the average of the current pixel, left, right and top most. We then re-set the pixel back into video memory 1 pixel above our current location. Therefore, we have no interest in trying to process the top-most row of video memory.

The only other part that is a little awkward to look at in this code block will be the `ADC` instructions. We're dealing with bytes (values of 0..255). We're adding 4 of these together so we're going to quickly overflow a byte sized register. `ADC` (or add with carry adjustment) allows us to overflow this information into `AX`'s higher-order byte (`AH`). When it comes time to divide (or take the arithmetic average) of this pixel's intensity, we'll be able to perform this operation on the word sized `AX` register. Neat. Check it out:

{% highlight nasm %}
bloom:

	mov	cx, 63680		; we average every pixel except the top row
	mov	di, 320			; we start at the 2nd row
	
next_avg:
	xor	ax, ax			; clear out our accumulator
	
	mov	al, es:[di]		; get the current pixel
	
	add	al, es:[di-1] 	; add the pixel to the left
	adc	ah, 0			; adjust for overflow
	
	add	al, es:[di+1] 	; add the pixel to the right
	adc	ah, 0			; adjust for overflow
	
	add	al, es:[di-320]	; add the pixel above
	adc	ah, 0			; adjust for overflow
	
	shr	ax, 2			; divide by 4 to get the average
	
	cmp	al, 0			; can we dampen?
	jz	no_damp			; jump over if we can't
	
	dec	al			; dampen the colour by 1
	
no_damp:
	mov	es:[di-320], al	; put the averaged pixel 1 pixel above
	
	inc	di			; next pixel
	dec	cx			; keep count of how many we've got left
	jnz	next_avg
	
	ret
{% endhighlight %}

So, when you put it all together (and run it in DosBox) you'll get something that looks like this:

![Smoke application running](http://2.bp.blogspot.com/-0SUcxXs8a_k/ULCQvxj-8qI/AAAAAAAAAhQ/hlD_2uNejwE/s400/Screen+Shot+2012-11-24+at+7.16.37+PM.png)

Well.. I'm feeling all nostalgic now. Might go and fire up DosBox and play a couple of games of Double Dragon.