---
layout: post
title: Installing node.js on Wheezy
date: 2013-11-11
comments: false
---

Here's a quick guide to getting [node.js](http://nodejs.org/) up and running on Debian Wheezy. First of all, you'll need all of the development tools required to build node.js on your machine.

{% highlight bash %}
$ sudo apt-get install python g++ make checkinstall
{% endhighlight %}

Next, grab the latest node.js source from their website and extract it into a local folder

{% highlight bash %}
$ wget -N http://nodejs.org/dist/node-latest.tar.gz
$ tar -zxvf node-latest.tar.gz
{% endhighlight %}

After you've got the source, change into the extracted folder and build node.js (the version I got was 0.10.21)

{% highlight bash %}
$ cd node-v0.10.21
$ ./configure
$ sudo checkinstall
{% endhighlight %}

A package should have now been created for you to install with [dpkg](https://wiki.debian.org/dpkg). You can install it with the following command:

{% highlight bash %}
$ sudo dpkg -i node_*
{% endhighlight %}

Done!