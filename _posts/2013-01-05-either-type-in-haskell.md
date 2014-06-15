---
layout: post
title: Either type in Haskell
date: 2013-01-05
comments: false
categories: [ "Haskell", "Programming", "Either" ]
---

### Introduction

Doing some further work in the world of Haskell and have come across the [Either](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-Either.html) type from the base library on a few occasions. Today I'll post about how to work with this type as you'll come across it a bit and it is quite handy.

### Background

Just as its english counterpart describes, [Either](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-Either.html) can represent one value or another. Scenarios where this might be the return value from a function where you may get the successful result value or you might get an error value. The [Either](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-Either.html) data type is defined as follows.

<script src="https://gist.github.com/4459889.js"></script>

You construct an [Either](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-Either.html) by calling either the `Left` or `Right` constructor. So, [Either](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-Either.html) is just a wrapper around a value that can be either one or the other.

### Working with it

In GHCI I have created two instances of Either. "lefty" is constructed using Left, "righty" with Right.

{% highlight text %}
> let lefty = Left 10
> let righty = Right "John"
> :t lefty
lefty :: Either Integer b
> :t righty
righty :: Either a [Char]
{% endhighlight %}

That all seems pretty straight forward. Calling `Left` or `Right` gives us back a value of an incomplete type. Haskell only really knows how to fill the types out that we've actually used. Having a look at the values that we've created, we're reminded that they are either `Left` or `Right` values as such.

{% highlight text %}
> lefty
Left 10
> righty
Right "John"
{% endhighlight %}

Convenient, but if we're going to have a chance of using these values for anything real, we'll need to extract or unbox the value from the Either construct. The Either type has two functions which will take the boxed values into array called `lefts` and `rights`. This makes sense. Take a look at how these functions interact with lefty and righty.

{% highlight text %}
> lefts [lefty]
[10]
> rights [lefty]
[]
> lefts [righty]
[]
> rights [righty]
["John"]
{% endhighlight %}

They've now been taken out of the `Either` construct and are values ready to be processed sitting in a list. In the next example, we use pattern matching to detect if we're trying to divide by zero. Even though my preference is to always say that it's infinity, computers just like to complain about it.

{% highlight haskell %}
safeDiv :: Float -> Float -> Either String Float
safeDiv x 0 = Left "Divison by zero"
safeDiv x y = Right (x / y)
{% endhighlight %}

The type that's used here `Either String Float` says that we're either going to receive a `String` or a `Float` in this value. You can see the case for zero division offering a `String` on the `Left`, otherwise we supply the quotient on the `Right`.

There ya have it!