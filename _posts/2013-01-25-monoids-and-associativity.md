---
layout: post
title: Monoids and Associativity
date: 2013-01-25
comments: false
---

### Introduction

Associativity is a property of a function which has an output equivalent no matter what order the function is applied. Addition and product are examples of mathematical functions that are associative as opposed to subtraction and division which are not. Today's post will focus on associative functions and their definitions through Monoids in Haskell.

### Associative functions

In the introduction to this post, I spoke about associative functions and gave a few examples. Here are those examples again, but demonstrated mathematically.

{% highlight text %}
(5 * 7) * 9 == 5 * (7 * 9)
(5 + 7) + 9 == 5 + (7 + 9)
(5 - 7) - 9 != 5 - (7 - 9)
(5 / 7) / 9 != 5 / (7 / 9)
{% endhighlight %}

These examples are pretty straight forward in their reading. You can see clearly that the product and addition operators are associative, subtraction and division not. String concatenation (or array concatenation) is also associative such that:

{% highlight text %}
"Hello " ++ ("World" ++ "!") == ("Hello " ++ "World") ++ "!"
{% endhighlight %}

These proofs are pretty straight forward. I think that you should have the idea of what associative functions are by now.

### Monoids

Monoids are Haskell's way of decorating a type that is functionally associative. The definition of a Monoid is as follows.

{% highlight haskell %}
class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty
{% endhighlight %}

You can find more information specifically about the type up on the [Haskell Wiki](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-Monoid.html). From the definition above, we can see that there are 3 functions to be defined.

### mempty

`mempty` is the identity value for the monoid. It acts more like a constant rather than a function.

### mappend

`mappend` is the binary function that can be used between two of these typed Monoids.

### mconcat

`mconcat` folds the `mappend` function between the elements of an array of Monoids.

### Monoid Laws

Above, I have gone through the basic principals of associative functions which by-and-large cover off on the laws that a Monoid must abide by. There are some explicit ways of writing this concept, there are as follows.

{% highlight text %}
mempty `mappend` x = x
x `mappend` mempty = x
(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
{% endhighlight %}

That's Monoids for you. Take a look as some example implementations on the Wiki and around the web.