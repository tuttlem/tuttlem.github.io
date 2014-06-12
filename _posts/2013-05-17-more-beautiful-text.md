---
layout: post
title: More Beautiful Text
date: 2013-05-17
comments: false
---

Getting a wider array of fonts into your website is a pretty simple task these days. There's quite a selection of fonts that you can use offered on the web. Just take a look at [Google's repository](http://www.google.com/fonts/) to see what I'm talking about.

Once you've selected the font that's right for your application, you can import it to pages using the following directive:

{% highlight css %}
@import url(http://fonts.googleapis.com/css?family=Droid+Sans:400,700);
{% endhighlight %}

... and then finally use it in your css directives.

{% highlight css %}
font-family: "Droid Sans", arial, verdana, sans-serif;
{% endhighlight %}

Easy.