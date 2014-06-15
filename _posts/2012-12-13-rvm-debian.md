---
layout: post
title: RVM & Debian
date: 2012-12-13
comments: false
categories: [ "ruby", "rvm", "installation" ]
---

It's always nice to have a sandboxed environment for all of your little projects. I know with how frequently I'm jumping between technologies on the same machine, I always like to use a sandbox technology of choice for the particular platform. This is a quick walk through on getting RVM up and running on a debian machine.

{% highlight bash %}
# First up, you need to download an install RVM:
$ curl -L get.rvm.io | bash -s stable

# Load RVM into your environment:
$ source ~/.rvm/scripts/rvm

# Check for additional requirements that rubies (you're about to install) need:
$ rvm requirements
{% endhighlight %}

It's <strong>strongly advised</strong> that you follow the suggestions above for the Ruby(ies) that you want to run:

{% highlight bash %}
# Install the Ruby that you want to use:
$ rvm install 1.9.3

# Get Rubonic (yes, it's a word ... I ... I ... I think)
$ which ruby                      
/home/michael/.rvm/rubies/ruby-1.9.3-p327/bin/ruby
{% endhighlight %}


[The basics of RVM](https://rvm.io/rvm/basics/)
