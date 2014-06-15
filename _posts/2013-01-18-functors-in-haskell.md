---
layout: post
title: Functors in Haskell
date: 2013-01-18
comments: false
categories: [ "Haskell", "Functors" ]
---

### Introduction

[In a previous post]({% post_url 2013-01-02-derived-instances-for-types-in-haskell %}), I had lightly grazed the surface of the topic of Functors in Haskell and I thought it was time to come back and be a little more comprehensive about it. Without further ado, let's talk about Functors.

### What is a Functor?

At the code level, a `Functor` is any type that is an instance of the [Functor](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#t:Functor) typeclass. At a concept level, a `Functor` is something that can be mapped over. As soon as anyone says something like this to me I immediately start thinking of any enumerated type (array, list, vector, etc) and this is right.

A [Functor](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#t:Functor) only needs to implement 1 function, [fmap](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:fmap). How this looks in Haskell is as follows.

{% highlight haskell %}
class Functor f where
  fmap :: (a -> b) -> f a -> f b
{% endhighlight %}

Reading this gets to be a mouthful, but bear with me. This definition says: "for my first parameter, I want a function that takes an `a` type and returns a `b`; for my second parameter, I want an `a` value wrapped by this functor and I will return you a `b` value wrapped by this functor". I've tried to place emphasis on `a`'s and `b`'s in that previous sentence, otherwise it reads like rather difficult english. Applying this knowledge to a living-breathing example, I'll show you the [Maybe](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#t:Maybe) type.

{% highlight haskell %}
instance Functor Maybe where
  fmap f (Just x) = Just (f x)
  fmap _ Nothing  = Nothing
{% endhighlight %}

Looking at this we can see that when a `Just` value comes through, it's the value (wrapped in the `Just`) that has the function applied to it. When nothing comes through, we don't care for the function - we know that the return is going to be `Nothing`. The important thing to note here is that it's the value (wrapped in the `Just`) that's being operated on. Seeing this in action always helps an explanation.

{% highlight text %}
ghci> fmap (replicate 3) (Just 4)
Just [4, 4, 4]
{% endhighlight %}

The way that I read this is: "A function" `fmap` "that takes a function" `(replicate 3)` "and a functor value" `(Just 4)` "that then maps that function" `(replicate 3)` "over the inner value" `(4)` "which gets wrapped up in the functor" `(Just)`. This is the best way that I could think of to structure this sentence. It makes sense to me, I hope it helps you also. Again, the important thing to remember is that it's the inner-value that's getting processed which is why we ended up with `Just [4, 4, 4]`  rather than `[Just 4, Just 4, Just 4]`. Finally, when you're implementing functors of your own there are laws that must be met.

### Law #1

Mapping the [id](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:id) function over any functor value should produce the functor value that we supplied. 

{% highlight haskell %}
fmap id = id
{% endhighlight %}

### Law #2

Mapping two composed functions over a functor value should be the same as functionally composing those two functions. 

{% highlight haskell %}
fmap (f . g) = fmap f . fmap g
{% endhighlight %}

This second law makes sense once you think about performing fmap over a function, as it's just the same as function composition.

{% highlight haskell %}
fmap (*15) (-2) == (*15) . (-2)
{% endhighlight %}

This post should arm you sufficiently to start inflicting your own Functors on the world.