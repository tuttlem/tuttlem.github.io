---
layout: post
title: Listing open ports and who owns them
date: 2015-07-21
comments: false
categories: [ "Linux", "Admin", "Network" ]
---

To list all of the network ports and users that own them you can use the [lsof](http://linux.die.net/man/8/lsof) command. 

{% highlight bash %}
sudo lsof -i
{% endhighlight %}

The [netstat](http://linux.die.net/man/8/netstat) command is also available to provide the same sort of information.

{% highlight bash %}
sudo netstat -lptu
{% endhighlight %}

