---
layout: post
title: Encode or Decode base64 at the console
date: 2014-07-07
comments: false
categories: ["Linux", "Shell", "base64"]
---

Today's post is a quick tip on encoding and decoding base64 information with the `base64` command at the linux prompt.

In order to encode a piece of text, you can do the following:

{% highlight bash %}
$ echo 'I want to encode this text' | base64
SSB3YW50IHRvIGVuY29kZSB0aGlzIHRleHQK
{% endhighlight %}

To reverse the process, simply use the `-d` switch:

{% highlight bash %}
$ echo SSB3YW50IHRvIGVuY29kZSB0aGlzIHRleHQK | base64 -d
I want to encode this text
{% endhighlight %}

Simple.
