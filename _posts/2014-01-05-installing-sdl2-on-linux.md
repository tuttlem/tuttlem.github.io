---
layout: post
title: Installing SDL2 on Linux
date: 2014-01-05
comments: false
categories: [ "Linux", "SDL2", "Installation" ]
---

### Introduction

[SDL2](http://www.libsdl.org/download-2.0.php) was released a little while ago, but still hasn't made it into the stable repositories of some Linux distributions. After doing a big of digging, it's not too hard to get this installed yourself - most of the advice offered in this post comes from an answer on the Ubuntu forums [here](http://askubuntu.com/questions/344512/what-is-the-general-procedure-to-install-development-libraries-in-ubuntu).

In today's post, we'll install SDL2 on a Debian/Ubuntu style distribution from source.

### Dependencies

First thing before we download and compile the SDL2 source is to get some of the dependencies installed on your system. The following install line will put all of the libraries that SDL2 requires:

{% highlight bash %}
$ sudo apt-get install build-essential xorg-dev libudev-dev libts-dev libgl1-mesa-dev libglu1-mesa-dev libasound2-dev libpulse-dev libopenal-dev libogg-dev libvorbis-dev libaudiofile-dev libpng12-dev libfreetype6-dev libusb-dev libdbus-1-dev zlib1g-dev libdirectfb-dev
{% endhighlight %}

Once all of these have installed successfully, you'll need to download a copy of the source. All downloads can be found here. This guide will assume that you'll download the .tar.gz source archive.

### Compilation and Installation

Extract the package into a directory under your home directory, somewhere . . .

{% highlight bash %}	
$ tar -xzf SDL2-*.tar.gz
$ cd SDL2-*
{% endhighlight %}

Next we'll configure, build and install the libraries.

{% highlight bash %}	
$ ./configure
$ make
$ sudo make install
{% endhighlight %}

Once compilation and installation have complete, you'll need to update your library links/cache. Do this with the following command:

{% highlight bash %}	
$ sudo ldconfig
{% endhighlight %}

That's all there is to it. 