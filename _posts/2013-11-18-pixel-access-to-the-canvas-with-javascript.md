---
layout: post
title: Pixel Access to the Canvas with Javascript
date: 2013-11-18
comments: false
---

### Introduction

Gaining pixel-level access using the HTML canvas opens up some possibilities for some frame-buffer style rasterisation. Today's post will focus on the code required to get you access to this array. Here's the code on how to get started:

{% highlight js %}
// create the canvas object
var canvas = document.createElement("canvas");

// maximise the canvas to stretch over the window
canvas.width = window.innerWidth;
canvas.height = window.innerHeight;

// get the 2d drawing context for 
var cxt = canvas.getContext("2d");
// get the image data
var imageData = cxt.createImageData(width, height);

// save off the dimensions for later use
var width = canvas.width;
var height = canvas.height;

// get the canvas on the page
document.body.appendChild(canvas);
{% endhighlight %}

First of all, we programmatically create our canvas object using `document.createElement`. Using the inner dimensions of the window, we can then set the canvas' size. Of course this can be custom set to the dimensions you require - I just like to take over the whole window! Using the `canvas` object, we then pull out the drawing context with `getContext`. The next part, using `createImageData` we then get a reference to the frame-buffer to draw to. This gives us read/write access to the canvas through an array. Finally, we'll take note of the width and height (this will come in handy later) and then pop the canvas onto the page.

### Frame-buffer structure

So, I say "frame-buffer" - but it's just an array. It's quite nicely laid out such that pixels start at every 4 elements within the array. The first element being the red component, second is green, third is blue and the fourth is the alpha. Calculating an offset into the array is a piece of cake. For example, take the following piece of code which will allow you to set a single pixel on the frame-buffer.

{% highlight js %}
var setPixel = function(x, y, r, g, b, a, buffer) {
  // calculate the start of the pixel
  var offset = ((y * width) + x) * 4;
  
  // set the components
  buffer[offset] = r;
  buffer[offset] = g;
  buffer[offset] = b;
  buffer[offset] = a;
};
{% endhighlight %}

The main part to focus on here is the calculation of the offset. Above, I said it was important to take note of the dimensions - we're only using the width here. This is pretty straight forward calculation of an offset within a linear data segment with Cartesian co-ordinates.

### Flip out!

Now that we've drawn all of the data to the image buffer (frame-buffer), we need a way to get it back onto our canvas. This is simply done using `putImageData`.

{% highlight js %}
cxt.putImageData(imageData, 0, 0);
{% endhighlight %}

That's it for now. 
