---
layout: post
title: Textfile processing with bash
date: 2015-02-10
comments: false
categories: [ "bash" ]
---

Today's post will be a [bash](http://www.gnu.org/software/bash/) on liner that allows you to process text files, line-by-line.

{% highlight bash %}
while read line; do wget $line; done < urls.txt
{% endhighlight %}

In this case, `urls.txt` is a list of urls that we want to process using `wget`. `$line` is the value that we're currently processing <em>(the line of text)</em>.

This is a pretty heavy handed way of downloading a handful of URLs. You could much easier do it with [xargs](http://en.wikipedia.org/wiki/Xargs) which I'd mentioned in a [previous post]({% post_url 2014-03-24-xargs-i %}). Anyway, the `while` way gives you a bit more room to breathe in your loops.