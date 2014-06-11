---
layout: post
title: Follow up to a Camera Implementation
date: 2013-12-30
comments: false
---

### Introduction 

In a [previous post]({% post_url 2013-12-30-a-camera-implementation-in-c %}) I'd written about a simple camera implementation that you can use in projects of your own. This post I'll show how I've implemented this camera with some mouse handling routines to make it feel like you're orienting your head with the mouse.

### The Idea 

We'll capture all of the mouse movements given in an application and see how far the mouse deviates from a given origin point. I think that the most sane origin point to go from is the center of your window. For each movement that the mouse makes from the center of the window, we need to:

* Determine how much movement occurred on the x axis
* Determine how much movement occurred on the y axis
* Deaden this movement by a co-efficient to simulate mouse "sensitivity"
* Set the yaw and pitch (head up/down, left/right) of the camera
* Reset the mouse back to the origin point

Here's how I've done it in code:

{% highlight c %}
void mouseMotion(int x, int y) {
  // calculate the origin point (shifting right divides by two, remember!)
  int halfWidth = windowWidth >> 1;
  int halfHeight = windowHeight >> 1;

  // calculate how far we deviated from the origin point and deaden this
  // by a factor of 20
  float deltaX = (halfWidth - x) / 20.0f;
  float deltaY = (halfHeight - y) / 20.0f;

  // don't do anything if there wasn't any movement to report
  if ((deltaX == 0.0f) && (deltaY == 0.0f)) {
    return ;
  }

  // set the camera's orientation
  cam.yaw(deltaX);
  cam.pitch(deltaY);

  // reset the mouse pointer back to the origin point
  glutWarpPointer(halfWidth, halfHeight);
}
{% endhighlight %}

You can see that I'm using GLUT to do these demos. The only GLUT-specific piece of code here is the warp command which puts the mouse back onto the origin point. You should have an equivalent function to do the same in the framework of your choice.

Well, there you have it. You can now orient your camera using your mouse.