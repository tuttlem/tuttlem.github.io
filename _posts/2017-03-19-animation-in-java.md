---
layout: post
title: Animation in Java
date: 2017-03-19
comments: false
categories: [ "java", "animation", "double", "buffer" ]
---

The [abstract window toolkit](https://en.wikipedia.org/wiki/Abstract_Window_Toolkit) provide the programmer with a great level of flexibility when creating user interfaces. Today's blog post is going to go through the basic setup of a double-buffered animation loop implemented in Java, using AWT.

## Settings

First off, we start by making some system settings; setting up to use OpenGL, etc:

{% highlight java %}
static {
  System.setProperty("sun.java2d.trace", "timestamp,log,count");
  
  System.setProperty("sun.java2d.transaccel", "True");
  System.setProperty("sun.java2d.opengl", "True");
  
  System.setProperty("sun.java2d.d3d", "false"); //default on windows
  System.setProperty("sun.java2d.ddforcevram", "true");
}
{% endhighlight %}

The particulars of these flags can be found [in the documentation](http://docs.oracle.com/javase/7/docs/technotes/guides/2d/flags.html). These flags, 

* Setup trace logging
* Use hardware acceleration for translucency
* Use OpenGL
* Turn off Direct3D
* Put images into vram

## Canvas

We'll draw to a [Canvas](https://docs.oracle.com/javase/7/docs/api/java/awt/Canvas.html) and flip that onto our [Frame](https://docs.oracle.com/javase/7/docs/api/java/awt/Frame.html). We need to configure the `Canvas` so that it'll behave in a render-loop fashion, rather than responding to `paint` messages as it does normally.

We ignore these repaints using [setIgnoreRepaint](https://docs.oracle.com/javase/8/docs/api/java/awt/Component.html#setIgnoreRepaint-boolean-). 

Now comes the double-buffer part. We create a [BufferStrategy](https://docs.oracle.com/javase/7/docs/api/java/awt/image/BufferStrategy.html) using [createBufferStrategy](https://docs.oracle.com/javase/7/docs/api/java/awt/Canvas.html#createBufferStrategy(int)). The strategy is what holds our graphics objects that we'll render to.

{% highlight java %}
this.createBufferStrategy(2);
strategy = this.getBufferStrategy();
{% endhighlight %}

## Rendering

The pump for the application is the renderer. It's pretty simple:

{% highlight java %}
public void render() {
  // get the graphics object
  Graphics2D bkG = (Graphics2D) strategy.getDrawGraphics();

  // start with a black canvas
  bkG.setPaint(backgroundGradient);
  bkG.fillRect(0, 0, getWidth(), getHeight());

  // TODO: Here's where the render code goes

  // release the resources held by the background image
  bkG.dispose();

  // flip the back buffer now
  strategy.show();
  Toolkit.getDefaultToolkit().sync();
}
{% endhighlight %}

## Get running

[Here](https://gist.github.com/tuttlem/a999dbd80ed047aed056d6125c3299a3) is a class that you can use to get running immediately.


