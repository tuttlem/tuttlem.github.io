---
layout: post
title: Mounting remote filesystems with sshfs
date: 2014-07-19
comments: false
categories: ["admin", "sshfs", "mount"]
---

Getting direct access over [ssh](http://www.openssh.com/) is simplified greatly by [sshfs](http://fuse.sourceforge.net/sshfs.html), a [fuse](http://fuse.sourceforge.net/sshfs.html) based file system. To get started, install [sshfs](http://fuse.sourceforge.net/sshfs.html) with your favourite package manager:

{% highlight bash %}
$ sudo pacman -S sshfs
{% endhighlight %}

To connect to a remote file system, you just use the following:

{% highlight bash %}
$ sshfs host: mountpoint
{% endhighlight %}

Much like <em>ssh</em>, the `host` argument can take on the format of <em>user@host</em> if you're logged in as a user that doesn't correspond to the remote machine.

When you're done, unmounting the filesystem is done like so:

{% highlight bash %}
$ fusermount -u mountpoint
{% endhighlight %}
