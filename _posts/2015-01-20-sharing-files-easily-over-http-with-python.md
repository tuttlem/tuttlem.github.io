---
layout: post
title: Sharing files easily over HTTP with Python
date: 2015-01-20
comments: false
categories: [ "python", "http", "SimpleHTTPServer" ]
---

Getting a quick web server up and running is really simple (if you don't need a fully blown application server). I find this technique really useful when prototyping web sites that I only need to serve static HTML, CSS & Javascript with.

In the folder that hosts your web application, issue the following Python command:

{% highlight bash %}
python -m SimpleHTTPServer
{% endhighlight %}

After you do this, you'll get a confirmation message that your site is available:

{% highlight text %}
Serving HTTP on 0.0.0.0 port 8000 ...
{% endhighlight %}

And that's it. You can read up more on this really handy utility [here](https://docs.python.org/2/library/simplehttpserver.html). 

