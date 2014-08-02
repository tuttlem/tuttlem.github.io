---
layout: post
title: Mounting your Android phone with mtp
date: 2014-08-03
comments: false
categories: ["mount", "linux", "android", "phone", "fuse"]
---

This post is really just a short-cut bookmark to the [Arch documentation on the topic](https://wiki.archlinux.org/index.php/MTP). This post will walk through the steps required to mount your Android phone using FUSE.

Install the package `mtpfs` if you don't already have it on your system. After that's installed, you'll need to uncomment the line `user_allow_other` in your `/etc/fuse.conf` file.

Plug your phone in and issue the following.

To mount your device:

{% highlight bash %}
$ mtpfs -o allow_other /media/your_mountpoint_here
{% endhighlight %}

Once you're done, you can unmount with:

{% highlight bash %}
$ fusermount -u /media/your_mountpoint_here
{% endhighlight %}


