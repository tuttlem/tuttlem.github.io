---
layout: post
title: Using libxml2
date: 2015-08-18
comments: false
categories: [ "libxml", "xml" ]
---

[libxml2](http://www.xmlsoft.org/) is a C library that provides XML handling capabilities to your applications. It supports an extensive range of standards and specifications common in the XML community. It's also a highly ported library between platforms and architectures making it a good choice when your application needs to move.

For a deeper-dive into the libxml2 library, I suggest a read through on their [tutorial](http://xmlsoft.org/tutorial/index.html).

### Building applications

The `xml2-config` program allows your Makefiles to remain relatively noise-free. To build any applications against libxml2, you just need the `--cflags` and `--libs` switches to compile and link respectively:

{% highlight bash %}
$ gcc `xml2-config --cflags --libs` prog.c -o prog
{% endhighlight %}

### Writing programs

Rather than re-produce them all, I've just got a link to the [set of examples](http://www.xmlsoft.org/examples/) on the libxml2 site.

