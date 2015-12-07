---
layout: post
title: Running X11 applications with port forwarding
date: 2015-12-07
comments: false
categories: [ "ssh", "x11", "xorg" ]
---

Today's post is a quick tip on [X11 port forwarding](http://tldp.org/HOWTO/XDMCP-HOWTO/ssh.html), and how to use it to run X11 applications remotely.

### The setup

Your remote computer, the one that will actually run the application needs [openssh](http://www.openssh.com/) installed. Use your favorite package manager to get that installed. You then need to edit your sshd configuration file to allow X11 port forwarding. 

{% highlight bash %}
sudo emacs /etc/ssh/sshd_config
{% endhighlight %}

You need to make two edits to this file:

{% highlight text %}
X11Forwarding     yes
X11UseLocalhost   no
{% endhighlight %}

Restart the ssh daemon.

### Running

From your client computer now, connect to your remote host and run any X11 application that you want. It'll appear on your client machine.

{% highlight bash %}
ssh -XC user@host
/usr/bin/firefox
{% endhighlight %}

