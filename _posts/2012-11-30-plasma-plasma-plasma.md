---
layout: post
title: Plasma, plasma, plasma!
date: 2012-11-30
comments: false
categories: [ "C", "Programming", "Mode 13", "plasma" ]
---

Kicking back into old, old, old school mode, I had found another cool effect laying around that seemed to work really well in dosbox. It's a plasma (if you couldn't tell from the title). Plasmas are the cool, blobby sort of shapeless eye-grabbers that are seriously cool and simple. The basic mathematical thory of the plasma is simple.

* 4 state counters for the program track where you're up to overall
* 4 state counters per frame render track where you're up to for that frame
* Each pixel is the result of these 4 cosine wave intersections. You can include the x and y counting dimensions to add the 6 intersections.

That's it really. There's a little special sauce to make the plasma move and mutate but they really have no bearing over the generation of the effect itself.

### Cosine? Easy, I'll just call cosf()!

Well, not quite. This is a demo routine that will entirely written in assembly language (8086 assembly language to be exact) and as such we won't have the luxury of a math library (or math-coprocessor) to do the work of finding out cosine values. So, we must pre-calculate. A small C application gives us all the table pre-calculation we'll need for this application. It's good to keep this application handy to re-pre-calculate this table to taste. If you like a little extra calculation to go into your cos table, that is. Me, I like nerdy numbers. So, according to this cos table, there are 256 degress in a circle (see what I did there) and the top of the cos curve (1.0) is 255 moving through the centre point (0.0) at 127 all the way down to the bottom point (-1.0) at 0.

Here's the code to generate that table.

{% highlight c %}
#include <stdio.h>
#include <math.h>

#define PI_BY_2		(3.14159f * 2)

int main(int argc, char *argv[]) {

	int theta = 0, count = 0, count2 = 0;
	unsigned char values[256];

	for (theta = 0; theta <= 255; theta ++) {
		float angle = ((float)theta / 256.0f) * PI_BY_2;
		values[theta] = (unsigned char)((cosf(angle) * 127.0f) + 127.0f);
	}

	printf("costab DB ");

	for (count = 0; count < 32; count ++) {
		for (count2 = 0; count2 < 8; count2 ++) {
			printf("%03xh, ", values[(count << 3) + count2]);
		}

		printf("\n       DB ");
	}

	return 0;
}
{% endhighlight %}

Here is the table that is generated when running this code.

{% highlight nasm %}
costab DB 0feh, 0fdh, 0fdh, 0fdh, 0fdh, 0fdh, 0fch, 0fch
       DB 0fbh, 0fah, 0fah, 0f9h, 0f8h, 0f7h, 0f6h, 0f5h
       DB 0f4h, 0f3h, 0f1h, 0f0h, 0efh, 0edh, 0ebh, 0eah
       DB 0e8h, 0e6h, 0e5h, 0e3h, 0e1h, 0dfh, 0ddh, 0dah
       DB 0d8h, 0d6h, 0d4h, 0d1h, 0cfh, 0cdh, 0cah, 0c8h
       DB 0c5h, 0c2h, 0c0h, 0bdh, 0bah, 0b8h, 0b5h, 0b2h
       DB 0afh, 0ach, 0a9h, 0a6h, 0a3h, 0a0h, 09dh, 09ah
       DB 097h, 094h, 091h, 08eh, 08bh, 088h, 085h, 082h
       DB 07fh, 07bh, 078h, 075h, 072h, 06fh, 06ch, 069h
       DB 066h, 063h, 060h, 05dh, 05ah, 057h, 054h, 051h
       DB 04eh, 04bh, 048h, 045h, 043h, 040h, 03dh, 03bh
       DB 038h, 035h, 033h, 030h, 02eh, 02ch, 029h, 027h
       DB 025h, 023h, 020h, 01eh, 01ch, 01ah, 018h, 017h
       DB 015h, 013h, 012h, 010h, 00eh, 00dh, 00ch, 00ah
       DB 009h, 008h, 007h, 006h, 005h, 004h, 003h, 003h
       DB 002h, 001h, 001h, 000h, 000h, 000h, 000h, 000h
       DB 000h, 000h, 000h, 000h, 000h, 000h, 001h, 001h
       DB 002h, 003h, 003h, 004h, 005h, 006h, 007h, 008h
       DB 009h, 00ah, 00ch, 00dh, 00eh, 010h, 012h, 013h
       DB 015h, 017h, 018h, 01ah, 01ch, 01eh, 020h, 023h
       DB 025h, 027h, 029h, 02ch, 02eh, 030h, 033h, 035h
       DB 038h, 03bh, 03dh, 040h, 043h, 045h, 048h, 04bh
       DB 04eh, 051h, 054h, 057h, 05ah, 05dh, 060h, 063h
       DB 066h, 069h, 06ch, 06fh, 072h, 075h, 078h, 07bh
       DB 07eh, 082h, 085h, 088h, 08bh, 08eh, 091h, 094h
       DB 097h, 09ah, 09dh, 0a0h, 0a3h, 0a6h, 0a9h, 0ach
       DB 0afh, 0b2h, 0b5h, 0b8h, 0bah, 0bdh, 0c0h, 0c2h
       DB 0c5h, 0c8h, 0cah, 0cdh, 0cfh, 0d1h, 0d4h, 0d6h
       DB 0d8h, 0dah, 0ddh, 0dfh, 0e1h, 0e3h, 0e5h, 0e6h
       DB 0e8h, 0eah, 0ebh, 0edh, 0efh, 0f0h, 0f1h, 0f3h
       DB 0f4h, 0f5h, 0f6h, 0f7h, 0f8h, 0f9h, 0fah, 0fah
       DB 0fbh, 0fch, 0fch, 0fdh, 0fdh, 0fdh, 0fdh, 0fdh
{% endhighlight %}

<em>Ooooh aaahhh</em>, that's a nerdy cosine table! Now that we've solved all of the world's mathematical problems here, it's on to the effect! Just getting 4 counters to run over this cosine table and intersect with each other can produce a mesmerising result. Without setting a palette (the standard vga palette is a bit: ewwwwww), here's how the effect looks:

![Plasma on standard VGA](http://1.bp.blogspot.com/-qsvAge9ukJU/ULindu0nksI/AAAAAAAAAhw/uky0JwHqooA/s400/plasma.png)

Feel like you're at Woodstock yet? So, the effect really spans across two smaller functions, which I've tried to comment as best I can below. Here's drawing a single frame:

{% highlight nasm %}
plasma_frame:

	; jump over the local variables
	jmp plasma_frame_code

	temp_phase_1 DB	0
	temp_phase_2 DB 0
	temp_phase_3 DB 0
	temp_phase_4 DB 0

	y_loc 		 DW 0

plasma_frame_code:	

	; setup where we'll draw to
	xor		di, di

	; setup a pointer to our cos table
	lea		si, costab

	; iterate over every pixel
	mov		cx, 64000

	; setup temp state into 3 and 4
	mov		al, phase_3
	mov		temp_phase_3, al
	
	mov		al, phase_4
	mov		temp_phase_4, al
	
reset_1_and_2:

	; re-setup temp state into 1 and 2
	mov		al, phase_1
	mov		temp_phase_1, al
	
	mov		al, phase_2
	mov		temp_phase_2, al
	
	; save our overall progress
	push	cx
	; process the next row of pixels
	mov		cx, 320

plasma_frame_pixel:

	; calculate the pixel value
	; col = costab[t1] + costab[t2] + costab[t3] + costab[t4] 

	xor		bx, bx
	
	mov		bl, temp_phase_1
	mov		al, ds:[si + bx]
	
	mov		bl, temp_phase_2
	add		al, ds:[si + bx]
	adc		ah, 0
	
	mov		bl, temp_phase_3
	add		al, ds:[si + bx]
	adc		ah, 0
	
	mov		bl, temp_phase_4
	add		al, ds:[si + bx]
	adc		ah, 0

	; draw the pixel
	mov		es:[di], al

	; adjust counter 1
	mov		al, temp_phase_1
	add		al, 2
	mov		temp_phase_1, al
	
	; adjust counter 2
	mov		al, temp_phase_2
	sub		al, 1
	mov		temp_phase_2, al

	; move onto the next pixel
	inc		di
	dec		cx
	jnz		plasma_frame_pixel

	; adjust the y location by 1
	inc		y_loc

	pop		cx
	
	; adjust counter 3
	mov		al, temp_phase_3
	add		al, 2
	mov		temp_phase_3, al
	
	; adjust counter 4
	mov		al, temp_phase_4
	sub		al, 1
	mov		temp_phase_4, al		
	
	sub		cx, 320
	jnz		reset_1_and_2
	
	ret
{% endhighlight %}

Drawing a single frame isn't too difficult at all. It's important to remember that `es:[di]` is pointing to the vga buffer to draw to where as `ds:[si]` is pointing at the cosine table. We're using `bx` as a base pointer to offset `si` such that it acts as our array index. Neat-O!

Between frame draws, we need to make the plasma MOVE!!.. This is just some simple additions or subtractions. Using random values adds a sense of entropy to the process making the plasma move in an almost unpredictable way. It's a little more organic this way. I haven't done it this way though. The code you'll see below moves the plasma by fixed amounts per frame. Still gives it some movement.

{% highlight nasm %}
move_plasma:

	mov		al, phase_1
	add		al, 2	
	mov		phase_1, al
	
	mov		al, phase_2
	add		al, 1	
	mov		phase_2, al
	
	mov		al, phase_3
	sub		al, 1	
	mov		phase_3, al
	
	mov		al, phase_4
	add		al, 2	
	mov		phase_4, al

	ret
{% endhighlight %}

Wrapping those two calls in a loop that waits for a key to be pressed is all you should need to draw a plasma to the screen.

Things for you to do:

* Change the cosine table generation to produce a more interesting cosine curve
* Apply a palette to take the 60's-ness out of the default palette
* Apply a palette that you can cycle through (like 1024 or 2048 entries in size) so that the palette (and therefore the plasma) will morph colour as frames progress
* Add randomness.

Have fun.
