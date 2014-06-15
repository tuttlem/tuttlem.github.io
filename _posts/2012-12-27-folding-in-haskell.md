---
layout: post
title: Folding in Haskell
date: 2012-12-27
comments: false
categories: [ "Haskell", "Programming", "foldl", "foldr", "folds", "folding" ]
---

### Introduction

In this post I would like to present some basic concepts in folding. This really will be over in a flash, so don't blink - it's easy.

### tl;dr

The Haskell functions [foldl](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:foldl) and [foldr](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:foldr) allow you to "fold" functions between values.

### Diagram it for me!

If you read up on these functions in the documentation, you'll see mention of "reducing values in a list" and such. All this really means, is that you're going to iterate through a list apply a function at every stop and finish up with a "reduced" answer. Here, take a look at this. I have an array spanning 1 through 5.

{% highlight text %}
[1, 2, 3, 4, 5]
{% endhighlight %}

I want to add all of the values together, so I use `foldl` to move through the list applying the `+` operator.

{% highlight haskell %}
foldl (+) 0 [1,2,3,4,5]
{% endhighlight %}

This command in english says, apply the `+` operator between each element in the list (moving left to right) with an initial value of 0. Or:

{% highlight text %}
0 + 1 + 2 + 3 + 4 + 5 = 15
{% endhighlight %}

### Bring it back the other way!

Folding right is interesting. No so much for the example that we have above, as addition moving left or right is at identity with each other. I've prepared a more interesting example for moving to the right. Take a look at the following and the results:

{% highlight haskell %}
foldl (-) 10 [10, 20, 30]
-50

foldr (-) 10 [10, 20, 30]
10
{% endhighlight %}

Wow, that's quite the difference. When folding right, the reduction occurs in reverse (values right to left) and then it's applied to the initial value.

{% highlight text %}
(foldl)
10 - 10 - 20 - 30     = -50 

(foldr)
10 - (20 - (30 - 10)) = 10
{% endhighlight %}

So, there we are folding to the right! That's (very basic) folding for you anyway. An interesting follow-up to this article is the function `foldl'`. By nature `foldl` that we've just discussed is lazy, meaning it will build the list of computations to execute using the source array and only execute those computations once the array is depleted (internally). It's been shown that this model of execution can cause stack overflow errors for larger lists because of this deferred execution model. `foldl'` solves this by not deferring execution. So, the overall result will be the same it's just that `foldl'` won't be lazy in getting the answer back to you.
