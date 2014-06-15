---
layout: post
title: List comprehension in Haskell
date: 2012-12-27
comments: false
categories: [ "Haskell", "Programming", "list comprehension" ]
---

### Introduction

Generating and operating on lists of data in languages is an essential tool when doing even the most basic of processing. List comprehension is just this process. It's prevalent in most of today's languages and today I want to post about [this process in Haskell](http://www.haskell.org/haskellwiki/List_comprehension).

### A numeric example

Working with numeric data is probably the easiest of examples to help you grasp this concept. Let's start by using a list comprehension to generate a list of numbers 1-through-5.

{% highlight haskell %}
[x | x <- [1..5]]
{% endhighlight %}

This will return a list looking like [1, 2, 3, 4, 5]. Analysing what we've just written here, we can see that what we want to return is on the left hand side of the pipe `|` symbol, how we want to generate each value sits on the right hand side of the pipe symbol. This expression is wrapped in square braces because we want a list! We can change this ever so slightly to only give us back odd numbers like so:

{% highlight haskell %}
[[x | x <- [1..5], odd x]
{% endhighlight %}

You can see that we've just concatenated another piece of criteria to the right hand side of the expression specifying that we only want odd numbers. We then end up with a list looking like [1, 3, 5]. These are still very simple examples, but we can do some very powerful things with these expressions. Take the following for example.

{% highlight haskell %}
[[x * y | x <- [1..5], y <- [1..5]]
{% endhighlight %}

Looking at the left-hand side you can see that we want the multiple of x and y. On the right-hand side you can see that both x and y iterate 1-through-5, so we end up with the following:

{% highlight text %}
[1,2,3,4,5,2,4,6,8,10,3,6,9,12,15,4,8,12,16,20,5,10,15,20,25]
{% endhighlight %}

Now we're getting somewhere.


