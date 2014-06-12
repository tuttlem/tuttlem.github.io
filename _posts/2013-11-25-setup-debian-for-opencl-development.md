---
layout: post
title: Setup Debian for OpenCL Development
date: 2013-11-25
comments: false
---

### Introduction

[OpenCL](http://www.khronos.org/opencl/) (or Open Computing Language) is a framework that allows you to write code across different connected devices to your computer. Code that you write can execute on CPUs, GPUs, DPSs amongst other pieces of hardware. The framework itself is a standard that puts the focus on running your code across these devices but also emphasises parallel computing.

Today's post will just be on getting your development environment setup on Debian Wheezy to start writing some code.

### Installation

The installation process is pretty straight forward, but there are some choices in libraries. The major vendors (Intel, NVIDIA and AMD) all have development libraries that are installable from Debian's package repository. There's plenty of banter on the internet as to who's is better for what purpose.

First off, we need to install the header files we'll use to create OpenCL programs.

{% highlight bash %}
$ sudo apt-get install opencl-headers
{% endhighlight %}

This has now put all of the development headers in place for you to compile some code.

{% highlight text %}
$ ls -al /usr/include/CL
total 1060
drwxr-xr-x  2 root root   4096 Nov 25 22:51 .
drwxr-xr-x 56 root root   4096 Nov 25 22:51 ..
-rw-r--r--  1 root root   4859 Nov 15  2011 cl_d3d10.h
-rw-r--r--  1 root root   4853 Apr 18  2012 cl_d3d11.h
-rw-r--r--  1 root root   5157 Apr 18  2012 cl_dx9_media_sharing.h
-rw-r--r--  1 root root   9951 Nov 15  2011 cl_ext.h
-rw-r--r--  1 root root   2630 Nov 17  2011 cl_gl_ext.h
-rw-r--r--  1 root root   7429 Nov 15  2011 cl_gl.h
-rw-r--r--  1 root root  62888 Nov 17  2011 cl.h
-rw-r--r--  1 root root 915453 Feb  4  2012 cl.hpp
-rw-r--r--  1 root root  38164 Nov 17  2011 cl_platform.h
-rw-r--r--  1 root root   1754 Nov 15  2011 opencl.h
{% endhighlight %}

Secondly, we need to make a choice in what library we'll use:

The `amd-opencl-dev` package will install AMD's implementation, which you can read up on [here](http://developer.amd.com/resources/heterogeneous-computing/opencl-zone/). NVIDIA's package is installable through the `nvidia-opencl-dev` package which you can read up on [here](https://developer.nvidia.com/opencl). Finally, Intel's implementation is available through the `beignet-dev` package and you can read up on their implementation [here](http://software.intel.com/en-us/vcsource/tools/opencl).

I went with AMD's.

{% highlight bash %}
$ sudo apt-get install amd-opencl-dev
{% endhighlight %}

From here, it's time to write some code. I'll have some more blog posts on the way which will be walk-throughs for your first applications.
