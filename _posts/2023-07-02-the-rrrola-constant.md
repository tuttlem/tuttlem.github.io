---
layout: post
title: The Rrrola Constant
date: 2023-07-02
comments: false
categories: [ "" ]
---

When you're writing demos in 320x200 mode, you can quickly estimate X and Y coordinates from a screen offset with one simple multiply. That value that you use is `0xcccd`, and it's called the Rrrola Constant.

To start this, we need to adjust our video address. Remember, this is an estimate (it's good enough); but it does need a bit more "help" in the setup.

{% highlight asm %}
  push  0xa000 - 10			
  pop   es				


top:
  xor   di, di
  mov   cx, 64000
{% endhighlight %}

Right now, `es:[di]` is pointing to the start of video memory (adjusted).

Now we perform the multiply

{% highlight asm %}
pat:
  mov  ax, 0xcccd
  mul  di				
{% endhighlight %}

At this point the `(x, y)` pair is now available to us in `(dh, dl)`. This is really handy for use in your rendering functions.

In this example, we just make a pixel that's `x xor y`.

{% highlight asm %}
  xor  dh, dl
  mov  al, dh
  stosb
  loop pat
{% endhighlight %}

This works because the offset into the video buffer is worked out as `(y * 320) + x`. Multiplying this formula out by `Oxcccd` we end up with `(y * 0x1000040) + (x * 0xcccd)`

The top byte is `y * 0x1000000`. The next byte along is now `(x * 0xcccd / 0x10000)` which approximates to `(x * 256/320)`, which is useful to us. The lower two bytes from the product are garbage.

### Full example

The following is a `.com` demo-style example which uses the above technique:

{% highlight asm %}
  org  100h

start:
  ; setup 320x200x256
  mov  ax, 0x0013
  int  0x10

  ; adjust screen segment to work
  ; with the Rrrloa trick
  push 0xa000 - 10
  pop  es

top:
  ; start at the beginning of
  ; the video buffer
  xor  di, di
  mov  cx, 64000

pat:
  ; (dh, dl) -> (x, y)
  mov  ax, 0xcccd			 
  mul  di					

  ; col = x ^ y
  xor  dh, dl
  mov  al, dh
  
  ; paint
  stosb
  loop pat

  ; check for esc
  in  al, 60h				
  cmp al, 1
  
  jne top

  ; return to text
  mov  ax, 0x0003			
  int  0x10

  ; return to dos
  mov  ax, 0x4c00			
  int  0x21
{% endhighlight %}

Pretty!

![XOR Demo]({{ site.url }}/assets/xor-demo.png)
