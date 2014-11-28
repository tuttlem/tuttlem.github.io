---
layout: post
title: Wireshark installation on Debian
date: 2014-11-28
comments: false
categories: [ "Wireshark", "Debian" ]
---

A really quick guide on installing [Wireshark](https://www.wireshark.org/) on [Debian](https://www.debian.org/).

The installation itself is pretty straight forward, however there is a little bit of reconfig work and user administration to get going for non-root users.

Install Wireshark from apt

{% highlight bash %}
$ sudo apt-get install wireshark
{% endhighlight %}

Reconfigure the `wireshark-common` package making sure to answer <strong>yes</strong> to the question asked.

{% highlight bash %}
$ sudo dpkg-reconfigure wireshark-common 
{% endhighlight %}

Add any user to the `wireshark` group that needs to be able to capture data off the network interfaces.

{% highlight bash %}
$ sudo usermod -a -G wireshark $USER
{% endhighlight %}

Remember, if you added yourself to this group; you'll need to logout and log back in for the group changes to take effect.

