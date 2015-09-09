---
layout: post
title: Can't connect to X11 window server when using docker
date: 2015-09-09
comments: false
categories: [ "docker", "x11" ]
---

On some host system configurations, I've found that [Docker](https://www.docker.com/) containers that use the host's X11 session to present a user interface, terminate with a dreaded:

{% highlight text %}
Can't connect to X11 window server using 'unix:0.0' as the value of the DISPLAY variable
{% endhighlight %}

Seems that on these systems, access needs to be granted to users making connections to the X11 host. This is done using the [xhost](http://linux.die.net/man/1/xhost) command, like so.

{% highlight bash %}
$ sudo xhost +
{% endhighlight %}

Your docker containers shouldn't have any issues now.

