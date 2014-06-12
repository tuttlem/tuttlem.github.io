---
layout: post
title: Clojure and Leiningen quickstart
date: 2012-12-28
comments: false
---

### Introduction

[Clojure](http://clojure.org/) is the modern [LISP](http://en.wikipedia.org/wiki/Lisp_(programming_language)). Clojure is an elegant, dynamic programming language that runs a-top the JVM. In today's post, I'll show you how to get started with Clojure & [Leiningen](http://leiningen.org/).

### Getting installed

I've written this article from the perspective of a Debian user. Translating these steps into your own native environments should be as easy as re-structuring the installation steps to target your package manager of choice. Installing Clojure & Leiningen was a simple as this:

{% highlight bash %}
$ sudo apt-get install clojure leiningen
{% endhighlight %}

You're done. You should now have Clojure and Leiningen at your disposal.

### Initial steps

You'll want to kick the tires on this puppy, so from your bash prompt fire up the REPL environment and try a few commands:

{% highlight text %}
$ clojure
Clojure 1.2.1
user=> (+ 2 4)
6
user=> (print "Clojure is installed!")
Clojure is installed!nil
user=> (if (= 1 1) (print "Yes, 1 does equal 1") (print "Mathematics just stopped working"))
Yes, 1 does equal 1nil
{% endhighlight %}

Alright! Enough of this already. Let's generate a project. [Leiningen](http://leiningen.org/) is a painless way to get started on your Clojure project. All you have to do, is issue the following commands and you've got a project ready to go:

{% highlight bash %}
$ lein new projname
{% endhighlight %}

Where `projname` is the name of your project. For this test, I just called mine "myproj". If you have a look inside the directory that Leiningen has just generated for you, you'll see the following sub-directories:

{% highlight text %}
lib         - holds your programs dependencies
project.clj - a clojure file describing your project 
README      - duh! 
src         - your source files 
test        - any tests for your application
{% endhighlight %}

This is a pretty neat-and-tidy layout, ready for you to start coding.

### Bulding and running and cleaning, oh my!

Leiningen also makes it very easy to build, run and cleanup your project. Here's how. From within your project directory: 

{% highlight bash %}
# Build your project
$ lein compile

# Clean any built files
$ lein clean

# Run your project
$ lein run
{% endhighlight %}

Too easy.