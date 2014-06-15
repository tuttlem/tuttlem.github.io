---
layout: post
title: Turn off that pc speaker in Debian!
date: 2012-12-15
comments: false
categories: [ "pc speaker", "setup", "debian", "linux" ]
---

Only a short post. 

The [ThinkWiki](http://www.thinkwiki.org/wiki/ThinkWiki) has a great little [write-up](http://www.thinkwiki.org/wiki/How_to_disable_the_pc_speaker_(beep!)) on disabling the pc speaker device under Debian linux. I thought I'd just shortcut the details in this post.

{% highlight bash %}
# Remove the pc speaker module
$ sudo modprobe -r pcspkr snd_pcsp
{% endhighlight %}

### Remove the pc speaker permanently

{% highlight text %}
# the following lines need to be added to
# the /etc/modprobe.d/blacklist.conf
# make sure you find out which device you actually have

blacklist pcspkr
blacklist snd_pcsp
{% endhighlight %}

Anyway, these are just the details to disable the device. The ThinkWiki article has some other ideas also on how to make the bell tone tolerable. Not for me though. I can't stand it.