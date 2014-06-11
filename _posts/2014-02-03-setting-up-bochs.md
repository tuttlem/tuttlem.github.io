---
layout: post
title: Setting up Bochs
date: 2014-02-03
comments: false
---

### Introduction

Today's post is just a short one on the installation of the virtual machine Bochs within a Debian environment.

### Procedure

{% highlight bash %}
# Install Bochs using your package manager
sudo apt-get install bochs

# Install the X11 and sdl plugin for Bochs
sudo apt-get install bochs-x bochs-sdl
{% endhighlight %}

Finally, make sure that your machines are using `SDL` as the display library by adding this line to your `.bochsrc`

{% highlight text %}
files:display_library: sdl
{% endhighlight %}

That's it for today.
