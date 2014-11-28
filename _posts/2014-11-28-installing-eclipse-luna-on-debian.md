---
layout: post
title: Installing Eclipse Luna on Debian
date: 2014-11-28
comments: false
categories: [ "Eclipse", "Debian" ]
---

A really quick guide on installing [Eclipse Luna](https://eclipse.org/) on [Debian](https://www.debian.org/).

If you're on a fresh machine, and you're downloading/installing Eclipse for the purposes of [Java](https://www.java.com/en/) development, you'll want to install the [JDK](http://en.wikipedia.org/wiki/Java_Development_Kit). To get this going, I install the `openjdk-7-jdk` out of the apt repository.

{% highlight bash %}
$ sudo apt-get install openjdk-7-jdk
{% endhighlight %}

After that finishes, or while, grab a copy of the Eclipse version that you need from the [download page](https://www.eclipse.org/downloads/). Once it's down, I normally extract it and then put it in a system-wide location (as opposed to just running it from my home directory).

{% highlight bash %}
$ tar -zxvf eclipse-*.tar.gz
$ sudo mv eclipse /opt
{% endhighlight %}

One little oddity before starting Eclipse up, I've had to apply a GTK setting. Prior to making this setting, Eclipse would crash!

Add the following lines to you `/opt/eclipse/eclipse.ini` file. Make sure it appears before the `--launcher.appendVmargs` directive.

{% highlight text %}
--launcher.GTK_version
2
{% endhighlight %}