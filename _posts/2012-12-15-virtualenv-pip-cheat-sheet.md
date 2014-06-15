---
layout: post
title: Virtualenv & pip cheat-sheet
date: 2012-12-15
comments: false
categories: [ "virtualenv", "pip", "python", "cheatsheet" ]
---

Here are a couple of the common commands I need day-to-day in order to navigate around [virtualenv](http://www.virtualenv.org/en/latest/).


{% highlight bash %}
# Creating a new virtual envrionment
$ virtualenv env

# Using a virtual environment
$ source env/bin/activate

# Getting out of a virtual environment
$ deactivate

# Re-hydrating a requirements file into your environment
$ pip install -r requirements.txt

# Serializing all of your environment's packages into a requirements file
$ pip freeze > requirements.txt
{% endhighlight %}

That's some basics that will get you up and running.