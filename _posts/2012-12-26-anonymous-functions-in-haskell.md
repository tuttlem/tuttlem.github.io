---
layout: post
title: Anonymous Functions in Haskell
date: 2012-12-26
comments: false
categories: [ "Haskell", "anonymous function", "programming" ]
---


### Introduction

It's no doubt that when you're first entering the wild world of Haskell, that it's syntax is a little alien to look at, at first glance. Once of the major building blocks or keys to unlocking power in Haskell are anonymous functions. The idea of higher-order functions in languages these days is becoming more of a standard rather than a feature and Haskell is no exception. There's plenty around on the web to read about [anonymous functions](http://en.wikipedia.org/wiki/Anonymous_function), [higher-order functions](http://en.wikipedia.org/wiki/Higher-order_function) and of course [functional programming](http://en.wikipedia.org/wiki/Functional_programming.

### An example

A fairly unintuitive example, but shows how you can assign an anonymous function to a handleable variable is shown:

{% highlight haskell %}
let y = (\x -> x)
{% endhighlight %}

Breaking this down, the key part is `(\x -> x)`. Immediately, you can see that anonymous functions take the form of `(\param1 .. paramn -> body)`. The real power here is unlocked when you use an anonymous function in conjunction with other functions. The best example I think is the use of the [map](http://zvon.org/other/haskell/Outputprelude/map_f.html) function. The [map](http://zvon.org/other/haskell/Outputprelude/map_f.html) function is defined as follows:

{% highlight haskell %}
map :: (a -> b) -> [a] -> [b]
map f xs = [f x | x <- xs]
{% endhighlight %}

This definition says, give me a function `f` and I'll take all of the `a`'s and turn them into `b`'s. Obviously, the anonymous function comes in for the `f` component. For simplicity, a can be an array of integers.

{% highlight haskell %}
map (\x -> x * x) [1,2..100]
{% endhighlight %}

So the above example maps the function `(x*x)` across an array from 1 to 100. Easy! 