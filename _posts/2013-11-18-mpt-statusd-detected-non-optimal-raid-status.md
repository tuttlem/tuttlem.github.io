---
layout: post
title: mpt-statusd detected non-optimal RAID status
date: 2013-11-18
comments: false
categories: [ "mpt-statusd", "non-optimal", "raid", "remove" ]
---

After installing Debian within a few VMWare virtual machines, I keep getting a rather annoying and persistent message spamming out my unix mail box as well as `/var/log/messages`.

> mpt-statusd: detected non-optimal RAID status

Simplest solution that I've come across is to just . . .

{% highlight bash %}
$ sudo apt-get remove mpt-status
{% endhighlight %}

Yup! That's it.
