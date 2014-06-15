---
layout: post
title: Macro-ing yourself out of the boilerplate
date: 2012-12-07
comments: false
categories: [ "C", "Programming", "macros" ]
---

Just working on a "build your own clone" style tutorial and going over some older code here, I thought it'd be interesting to note some funky work I'd done with macros. When you're working with 2-d array style states you get very good a writing for-loops over and over and over and ... So, thinking about this in a very generic sense - take a look at this macro

{% highlight c %}
#define FIELD_WIDTH  12
#define FIELD_HEIGHT 18

/* grabs an element from the field */
#define field_block(x, y) (_field_blocks[(y * FIELD_WIDTH) + x])

/* iterates over the field */
#define enum_field_blocks(x, y, o, fn)     \
   o = 0;                                  \
   for (y = 0; y < FIELD_HEIGHT; y ++) {   \
      for (x = 0; x < FIELD_WIDTH; x ++) { \
         fn                                \
         o ++;                             \
      }                                    \
   }                                       \
{% endhighlight %}

Working your way through the `enum_field_blocks` macro here, the parameters are:

* A horizontal axis iterator `x`
* A vertical axis iterator `y`
* A memory location (array index) iterator `o`
* and `fn`

So, what's `fn`? `fn` here allows the developer to specify what they want to execute inside the nested for loops:

{% highlight c %}
enum_field_blocks(x, y, offset, {
   _field_blocks[offset] = NULL;
});
{% endhighlight %}

If you're thinking "why didn't he just use `memset` to set all of the items in this array to `NULL`" - you've missed the point. The example is quite a bad one, I agree, but it does demonstrate that you can write any code that you'd like and have it execute for every item in the specific array cutting down on boilerplate that you have to write (just get the C pre-processor to do it for you).

Super.