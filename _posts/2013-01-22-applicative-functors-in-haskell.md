---
layout: post
title: Applicative Functors in Haskell
date: 2013-01-22
comments: false
---

### Introduction

As a follow up to my [previous post]({% post_url 2013-01-18-functors-in-haskell %}) on Functors, it's a natural progression for me to do a post on the more advanced version, the Applicative Functor. In a normal functor, you'll map a function over a functor and applicative functor is a reverse view of this where it'll allow you to map many functor values over a single function.

### What is an Applicative Functor?

At the code level, an Applicative Functor is any type that is in instance of the [Applicative](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Applicative.html#t:Applicative) typeclass. At a concept level, the Applicative Functor has not only the values in "a context" but also the function that we'll apply is in a context. This differs from just a normal Functor where it's only the value that's wrapped in a context. An Applicative Functor has 2 functions that require implementation. [pure](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Applicative.html#v:pure) and [<*>](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Applicative.html#v:-60--42--62-). This typeclass and functions are defined as follows.

{% highlight haskell %}
class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
{% endhighlight %}

The definition for `pure` on Hoogle suggests that it will

> Lift a value

"Lifting" is very well described in articles all over the web. Here's the [Haskell Wiki's article](http://www.haskell.org/haskellwiki/Lifting) on Lifting. `pure` takes in a value and returns an applicative value around that value. It's actually quite a simple definition when you read it. The other function defined here is `<*>`. The definition for `<*>` on Hoolgle suggests that it will perform

> Sequential application

The definition of sequential application here can be expanded such that `<*>` takes a function `(a -> b)` wrapped up in a functor `f` and another functor value `f a`. It'll extract the function from the first parameter `(a -> b)`, map it over the functor value `f a` to produce a result `f b`. The concept is very similar to what we've seen before in mapping scenarios, it's just that we "map" with different "things". Today we're mapping with functor values over a function.

### pure

Here's some examples of `pure` in action. You'll see that when we're casting to a type, we receive the appropriate value back

{% highlight text %}
> pure "Hey" :: Maybe String
Just "Hey"
> pure "Should be in a List" :: [String]
["Should be in a List"]
{% endhighlight %}

You can see here that the value is "lifted" by `pure`  into the container (in this case either a `Maybe` or a `List`). 

### <*>

For the next set of examples, I'll show you some usages of `<*>`. You'll need to keep in the front of your mind that all functions on the left hand side are applied to all values on the right hand side, so we end up with a Cartesian effect.

{% highlight text %}
> [sin, cos, tan] <*> [pi, pi, pi]
[1.2246467991473532e-16,1.2246467991473532e-16,1.2246467991473532e-16,-1.0,-1.0,-1.0,-1.2246467991473532e-16,-1.2246467991473532e-16,-1.2246467991473532e-16]
{% endhighlight %}

The `<*>` function is left associable, so when you start chaining calls together it's the leftmost that is evaluated first.

{% highlight text %}
> [(+),(*)] <*> [1,2] <*> [3,4]
[4,5,5,6,3,4,6,8]

(which is really)
[1+3,1+4,2+3,2+4,1*3,1*4,2*3,2*4]
{% endhighlight %}

### Applicative Style

You can see how using this "applicative style" in code can often be swapped 1-for-1 with list comprehensions as you achieve the same permuted or Cartesian effect. Take this for example.

{% highlight text %}
> [salutation ++ name | salutation <- ["Hello ", "Goodbye ", "Yo! "], name <- ["John", "Mary", "Anne"]]
["Hello John","Hello Mary","Hello Anne","Goodbye John","Goodbye Mary","Goodbye Anne","Yo! John","Yo! Mary","Yo! Anne"]

> (++) <$> ["Hello ", "GoodBye ", "Yo! "] <*> ["John", "Mary", "Anne"]
["Hello John","Hello Mary","Hello Anne","Goodbye John","Goodbye Mary","Goodbye Anne","Yo! John","Yo! Mary","Yo! Anne"]
{% endhighlight %}

You can see that when dealing with lists, the following is true:

{% highlight text %}
pure f <*>; xs == fmap f xs
{% endhighlight %}

In this context, `pure f` is putting `f` into a list. `[f] <*> xs` just applies each function in the left list to the right. Another implementation of the Applicative Functor that doesn't follow the same notion of a `List` is its use with `IO`. The idea of the applicative functor still holds, but when dealing with `IO` its the actions that are operated on. This can be thought of in the same way as sequencing and mapping in one.

### ZipList

Another type that is an instance of `Applicative` is the [ZipList](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Applicative.html#v:ZipList). The `ZipList` type is defined as follows.

{% highlight haskell %}
instance Applicative ZipList where
  pure x = ZipList (repeat x)
  ZipList fs <*> ZipList xs = ZipList (zipWith id fs xs)
{% endhighlight %}

When we use applicative style on a normal list, we end up with a Cartesian product of the two lists involved. A `ZipList` differs here by operating on indicies in either list that are the same. Index [0] on the left gets applied to Index [0] on the right. Index [1] on the left gets applied to Index [1] on the right, and so on.

### Applicative Functor Laws

Finally, there are a few laws that applicative functors must abide by, they are as follows.

* `pure id <*> v = v`
* `pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`
* `pure f <*> pure x = pure (f x)`
* u <*> pure y = pure ($ y) <*> u

### Applying applicative functors for yourself

This has been a very difficult topic that I've burnt some time on as late. There are plenty of examples of how this knowledge has been applied for a List or Maybe, but I've struggled to apply this to a type of my own. So far though, I've come across [this article on Applicative Functors](http://en.wikibooks.org/wiki/Haskell/Applicative_Functors) on the Hakell Wiki and most notably this sentence:

> Anytime you feel the need to define different higher order functions to accommodate for function-arguments with a different number of arguments, think about how defining a proper instance of Applicative can make your life easier.

That to me makes sense. So, if you have a function that operates on a particular type and you'd like to apply that function to "n" arguments - you'd normally create an `fmap` clone that would cater for that many arguments. Using an applicative functor, the re-creation of the `fmap` instance goes away as your higher-order function can expect any number of arguments. Because of this, the following is true.

{% highlight haskell %}
fmap2 f a b = f `fmap` a <*> b
fmap3 f a b c = f `fmap` a <*> b <*> c
fmap4 f a b c d = f `fmap` a <*> b <*> c <*> d
{% endhighlight %}

That's it for today's post. I hope to update this post with some more examples and information as I discover it!