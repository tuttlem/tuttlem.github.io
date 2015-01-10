---
layout: post
title: Mandelbrot set
date: 2012-12-01
comments: false
categories: [ "C", "Programming", "mandelbrot" ]
---

Another cool routine built off of some relatively simple mathematics is the mandelbrot set. Wikipedia has a really good write up if you're a little rusty on the ins and outs of a [mandelbrot set](http://en.wikipedia.org/wiki/Mandelbrot_set).

![Mandelbrot]({{ site.url }}/assets/mandelbrot.png)

This tutorial assumes that you've already got a video display ready with a pointer to your buffer. We'll just focus on the function that makes the doughnuts. Here's the code, explanation to follow. This code has been lifted out of a file that I had in an old dos program. It was written using turbo C, but will port over to anything pretty easily.

Anyway, on to the code!

{% highlight c %}
void mandelbrot_frame(double zoom, int max_iter, int xofs, int yofs) {
	double zx = 0, zy = 0, cx = 0, cy = 0;

    /* enumerate all of the rows */
	for (int y = 0; y < 200; y ++) {

        /* enumerate all of the columns */
		for (int x = 0; x < 320; x ++) {

            /* initialize step variables */
			zx = zy = 0;
			cx = (x - xofs) / zoom;
			cy = (y - yofs) / zoom;

			int iter = max_iter;

            /* calculate the iterations at this particular point */
			while (zx * zx + zy * zy < 4 && iter > 0) {
				double tmp = zx * zx - zy * zy + cx;
				zy = 2.0 * zx * zy + cy;
				zx = tmp;

				iter --;
			}

            /* plot the applicable pixel */
			video[x + (y * 320)] = (unsigned char) iter & 0xff;
		}
	}
}
{% endhighlight %}

So, you can see this code is very simple - to the point. We need to investigate all of the pixels (in this case 320x200 - or 64,000) and we calculate the iteration intersection at each point. The variables passed into the function allows the caller to animate the mandelbrot. `zoom` will take you further into the pattern, `xofs` and `yofs` will translate your position by this (x,y) pair. `max_iter` just determines how many cycles the caller wants to spend working out the iteration count. A higher number means that the plot comes out more detailed, but it slower to generate.
