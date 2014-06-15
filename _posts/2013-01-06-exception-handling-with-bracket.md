---
layout: post
title: Exception handling with bracket
date: 2013-01-06
comments: false
categories: [ "Haskell", "Programming", "Exceptions", "Exception Handling", "Bracket" ]
---

### Introduction

Performing IO in our pure environments can be dangerous. Who knows what people put in files that our programs are expected to read? There are tools that can help assure ourselves that we'll at least clean up if an explosion occurs. In this post, I'll talk about exception handling with `IO` actions using [bracket](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Exception-Base.html#v:bracket).

### What is it?

[bracket](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Exception-Base.html#v:bracket) is a function defined in `Control.Exception` and is defined as follows.

{% highlight haskell %}
bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
{% endhighlight %}

Its first parameter is a function that acquires a resource (or handle). Its second parameter is a function that releases the resource acquired in the first. The last parameter is another function that uses the acquired handle to get a result. The error handling comes in because the 2nd parameter, the function that releases the acquired resource is called even if an exception occurs during the execution of the last parameter. That's nifty. An example use of bracket looks as follows.

{% highlight haskell %}
bracket
  (openFile "myfile.txt" ReadMode)
  (\handle -> hClose handle)
  (\handle -> do { ... })
{% endhighlight %}

So, there's another safety mat for when you venture into the impure world.