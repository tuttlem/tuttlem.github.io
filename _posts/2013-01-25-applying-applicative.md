---
layout: post
title: Applying Applicative
date: 2013-01-25
comments: false
---

### Introduction

In a [previous post]({ post_url 2013-01-22-applicative-functors-in-haskell }), I had pretty much got the textbook definition down for an Applicative Functor and shown some of its pre-existing uses. Today's post, I want to walk through some code that I've written that implements `Applicative`.

### Put it in context!

Right on! It's all about context. When making instances of the [Functor](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#t:Functor) typeclass, it's all about defining [fmap](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:fmap) appropriately to "operate" on the data that sits in a context. What's a context then? The books and articles that I've read use pre-existing types as examples. A list for instance is a context, [Either](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#t:Either) is a context, [Maybe](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#t:Maybe) is a context. All of these things wrap data. So, let's make our own context to put something in.

{% highlight haskell %}
data CustomContext a = CustomContext a
                       deriving (Show)
{% endhighlight %}

There's our context, `CustomContext`. It just wraps around something .. anything. We can apply a functor instance to this with the following.

{% highlight haskell %}
instance Functor CustomContext where             
  fmap f (CustomContext x) = CustomContext (f x)
{% endhighlight %}

[fmap](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:fmap) allows a function to get inside the context and operate on the inner data - alright, I've said this already. Proof that all of this is working is as follows.

{% highlight text %}
λ> let x = CustomContext 10
λ> x                       
CustomContext 10           
λ> fmap (+5) x             
CustomContext 15           
{% endhighlight %}

We wrapped 10 in a context. We applied the function `(+5)` to the value inside our context. We got back a 15 wrapped in a context. Moving into Applicative territory now, it's not only the value that we operate on that has the context this time - it's also the function that we apply. We use the `pure` function to lift a value inside a context and we use `<*>` to apply a function to a value (both being inside their respective contexts). Here's how we'll define Applicative for our type.

{% highlight haskell %}
instance Applicative CustomContext where                    
  pure                                = CustomContext      
  CustomContext f <*> CustomContext x = CustomContext (f x)
{% endhighlight %}

The implementation of `pure` is straight forward, we're just going to pop the value in a `CustomContext` context. The sequential application function `<*>` is implemented very much how fmap was implemented, in fact it's the same. The difference comes in with how the parameters are supplied. We're taking the function out of the context, we take the value out of the context - we apply the function and wrap the result in the context. Here it is in action.

{% highlight text %}
λ> CustomContext (+5) <*> CustomContext 10
CustomContext 15                          
{% endhighlight %}

Also, with the use of `pure` lifting our values or functions into the context.

{% highlight text %}
λ> pure (+5) <*> CustomContext 10
CustomContext 15                 
λ> CustomContext (+5) <*> pure 10
CustomContext 15                 
{% endhighlight %}

Excellent. We've done it.

### Conclusion

This may not be the most feature-rich example, but it should give you good insight into the implementation and workings of `Applicative`.