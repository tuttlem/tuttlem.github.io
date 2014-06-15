---
layout: post
title: Using python from the safety of your virtualenv on Debian
date: 2012-12-15
comments: false
categories: [ "python", "virtualenv", "sandbox" ]
---

I like having my specific development environments setup, tailored for the particular project that I'm working on. When I'm working on a python project, [virtualenv](http://www.virtualenv.org/en/latest/) is my go to tool for the job. It's great.

We're going to install this using `easy_install` so that these instructions should translate pretty well to any linux environment. I am doing this from my Debian workstation though.

First up, we need to install the [python-setuptools](http://packages.debian.org/sid/python-setuptools) package so that we get access to `easy_install`.

{% highlight bash %}
sudo apt-get install python-setuptools
{% endhighlight %}

Next we install [virtualenv](http://www.virtualenv.org/en/latest/) with `easy_install`

{% highlight bash %}
sudo easy_install virtualenv
{% endhighlight %}

As far as the installation is considered, you're done! It's that easy. From here you can start to use `virtualenv` for all of your python based projects. I'll do a quick cheat-sheet write up shortly on how to use `virtualenv` at a glance.
