---
layout: post
title: Installing pyocr on Debian
date: 2014-03-13
comments: false
categories: [ "pyocr", "python", "tesseract", "cuneiform", "installation" ]
---

### Introduction

Today's post is an installation guide to get [pyocr](https://pypi.python.org/pypi/pyocr/0.2.2) up and running on a Debian Linux style distribution.

Prepare your python environment:

{% highlight bash %}
sudo apt-get install build-tools python-dev
sudo apt-get install python-setuptools
sudo easy_install pip
{% endhighlight %}

Install the operating system implementations of the OCR programs. In order to do this, you my need to enable the non-free repositories within your apt settings.

{% highlight bash %}
sudo apt-get install tesseract-ocr tesseract-ocr-eng
sudo apt-get install cuneiform
{% endhighlight %}

At this point, setuptools needed a little extra help with the following fix:

{% highlight bash %}
sudo pip install setuptools --no-use-wheel --upgrade
{% endhighlight %}

Prerequisite development libraries are now required prior to the python binding installations:

{% highlight bash %}
sudo apt-get install libtiff4-dev libjpeg62-dev zlib1g-dev libfreetype6-dev liblcms-dev libwebp-dev
{% endhighlight %}

Finally, we install the python bindings:

{% highlight bash %}
sudo pip install Pillow
sudo pip install pyocr
{% endhighlight %}

That gets pyocr up and running on a machine.

Other libraries I've installed for image manipulation are as follows.

{% highlight bash %}
sudo apt-get install python-pythonmagick
sudo apt-get install python-pdfminer
sudo apt-get install libmagickwand-dev
sudo pip install Wand
{% endhighlight %}