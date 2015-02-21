---
layout: post
title: Virtual buffer for liquid crystal displays
date: 2015-02-22
comments: false
categories: [ "arduino", "lcd" ]
---

Today's post will be about a [double buffer](http://en.wikipedia.org/wiki/Multiple_buffering) implementation that I'm working on for the [Liquid Crystal display module](http://arduino.cc/en/Tutorial/LiquidCrystal) with the [Arduino](http://www.arduino.cc/) micro-controller.

### What I'm using?

To write this article, I'm using the following hardware:

* [Freetronics Eleven](http://www.freetronics.com.au/products/eleven#.VOiUZDUvBhE)
* [Freetronics 16x2 LCD Shield](http://www.freetronics.com.au/products/16x2lcd#.VOiUojUvBhE)

For all of the software development that I've done here, I'm using the standard [Arduino IDE](http://arduino.cc/en/main/software) available from their site.

### Using the module "normally"

Having a [look at the documentation](http://arduino.cc/en/Reference/LiquidCrystal) for the Liquid Crystal library, there's already a comprehensive implementation of filling, scrolling and blinking functions available. 

The idea of this article isn't to re-implement this library, it's to build on it to add functionality.

The usual setup of the library would looks something similar to this:

{% highlight cpp %}
#include <LiquidCrystal.h>

LiquidCrystal lcd( 8, 9, 4, 5, 6, 7 );

void setup() {
  lcd.begin(16, 2);
}
{% endhighlight %}

This initializes our LCD module, ready for us to start writing to it:

{% highlight cpp %}
lcd.print("Hello, world!");
{% endhighlight %}

Nothing really special here, and rather than trying to piece together the "Hello, World" example from this code here, you'd be better off checking out the [samples section](http://arduino.cc/en/Tutorial/LiquidCrystal) for this module.

### What's the idea?

Given that we have a display of 16 characters in width by two characters in height, we're rather limited in what we can do. What I want to do is expand the 16x2 interface to have a "virtual canvas" to work with of any arbitrary size that you can just use the 16x2 LCD as a window into the canvas.

To begin with, we'll create some constants to assert our assumptions!

{% highlight cpp %}
#define LCD_WIDTH      16
#define LCD_HEIGHT     2
#define LCD_SIZE       (LCD_WIDTH * LCD_HEIGHT)
{% endhighlight %}

These are pretty straight forward. All of the constants come in handy. Bounds checking is where they really shine, but we also specifically use `LCD_HEIGHT` as a pitch-type variable that helps us calculate how far into a flat-memory array we need to be to start writing. More on this later.

### What do we want to do?

I think initially it'd be nice to:

* Define a virtual area (that's larger than our physical display)
* Move the "window" to any point in the virtual area
* Render the view at the window co-ordinates to the physical display
* Print to arbitrary points of the buffer

I've created a class, `LCDDoubleBuffer` that should wrap all of this functionality up. I'm going to start going through the code from here on.

### Define the virtual area

Give a `LiquidCrystal` reference and buffer dimensions, we can start to initialize our double buffer:

{% highlight cpp %}
LCDDoubleBuffer(LiquidCrystal *lcd, const int width, const int height) {
	this->_width = width;
	this->_height = height;
	this->_size = width * height;
	this->_lcd = lcd;

	this->_win_x = 0;
	this->_win_y = 0;

	this->_buffer = new char[this->_size];
}
{% endhighlight %}

`_width`, `_height` and `_size` all manage the internal data structure for us so that we <em>remember</em> how big our virtual area is.

`_lcd` is our link back to the "real-world" for us to perform some physical side-effects on our display.

`_win_x` and `_win_y` are to co-ordinates to the top left-hand corner of the window that we'll display on the LCD.

Finally, `_buffer` is our virtual canvas. <strong>Be careful here!</strong> Looking at the [memory specs](http://arduino.cc/en/Tutorial/Memory), you only get a whopping 2k SRAM to play with. With a `char` taking up 1 byte, you can see that you'll quickly go over this if you're not careful.  

### Working with the buffer

Well, it's pretty simple. These are all string operations, and they can be reduced even further than this to say that they're just memory operations.

To clear the buffer out, we use [memset](http://linux.die.net/man/3/memset). 

{% highlight cpp %}
void clear() {
	memset(this->_buffer, ' ', sizeof(char) * this->_size);
}
{% endhighlight %}

To write a string at a given location, we use [memcpy](http://linux.die.net/man/3/memcpy). <strong>Be careful here!</strong> The code that I present below doesn't perform any bounds checking on where we are in the buffer. This is <em>buffer under-and-overrun city, right here</em>.

{% highlight cpp %}
void print(const int x, const int y, const char *s) {
	char *o = this->_buffer + (x + (y * this->_width));
	memcpy(o, s, strlen(s));
}
{% endhighlight %}

`o` which is our offset pointer into the buffer is adjusted by calculating the linear address from `x` and `y` which is simply:

{% highlight text %}
linear = x + (y * width)
{% endhighlight %}

### Rendering the window

This is pretty brute-force. We enumerate over every <strong>valid</strong> cell defined for our LCD and write out the corresponding character.

{% highlight cpp %}
void render() {
	int buf_y = this->_win_y;
	for (int y = 0; y < LCD_HEIGHT; y ++) {
	  
		int buf_x = this->_win_x;

		for (int x = 0; x < LCD_WIDTH; x ++) {

			this->_lcd->setCursor(x, y);

			if (buf_y >= 0 && buf_y < this->_height &&
			    buf_x >= 0 && buf_x < this->_width) {
				int ofs = buf_x + (buf_y * this->_width);
				this->_lcd->print(this->_buffer[ofs]);
			} else {
				this->_lcd->print(" ");
			}

			buf_x ++;
		}

		buf_y ++;
	}

}
{% endhighlight %}

That big-fat-if-statement in the middle is protecting us from colouring outside the lines. When we are outside the bounds, we'll write a clearing character (in this case a whitespace).

### Actually using the code

We've got a double-buffer defined, ready to go. To test it out, I've decided to spice-up the "Hello, World" example and get it scrolling on the display using [cosine](http://en.wikipedia.org/wiki/Trigonometric_functions) values so it should ease-out and ease-in; back and forth.

Don't get too excited though. It's not brilliant.

{% highlight cpp %}
// create the LCD reference and double buffer
LiquidCrystal lcd( 8, 9, 4, 5, 6, 7 );
LCDDoubleBuffer dbuf(&lcd, 32, 32);

void setup() {
	Serial.begin(9600);

	lcd.begin(16, 2);

	// put "hello world" in the top corner
	dbuf.clear();  
	dbuf.print(0, 0, "Hello, world!");
}

float degs = 0.0f;

void loop() {
	// calculate and flatten to an int
	float a = -3 + (cos(degs) * 5);
	int x = (int)a;

	// move in position and render
	dbuf.moveTo(x, 0);
	dbuf.render();

	delay(50);

	// move along the cos curve
	degs += 0.1f;

	// 360 degrees == 0 degrees
	if (degs > 360.0f) {
		degs = 0.0f;
	}  
}
{% endhighlight %}

And there you have it. The full source of this demo is available [here](https://gist.github.com/tuttlem/0b413109c8d5592b9065) as a gist in my GitHub repository.

