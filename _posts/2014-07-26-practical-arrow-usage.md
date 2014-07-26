---
layout: post
title: Practical Arrow Usage
date: 2014-07-26
comments: false
categories: ["arrow", "haskell", "programming"]
---

### Introduction

Arrows provide you with a way to represent computation. They provide some interesting compositional combinations for building more complex operations that you're not going to find for Monads.

The [Arrow Class](http://hackage.haskell.org/package/base-4.7.0.1/docs/Control-Arrow.html#t:Arrow) is defined in the base libraries and can be imported from `Control.Arrows`. From [Haskell.org's Arrow page](http://www.haskell.org/arrows/):

> Arrows are a new abstract view of computation, defined by John Hughes [Hug00]. They serve much the same purpose as monads -- providing a common structure for libraries -- but are more general. In particular they allow notions of computation that may be partially static (independent of the input) or may take multiple inputs. If your application works fine with monads, you might as well stick with them. But if you're using a structure that's very like a monad, but isn't one, maybe it's an arrow.

Some interesting links on the topic follow:

* [Haskell Wiki page on Arrows](http://www.haskell.org/haskellwiki/Arrow)
* [Arrows on Haskell.org](http://www.haskell.org/arrows/)
* [Understanding Arrows](http://en.wikibooks.org/wiki/Haskell/Understanding_arrows)
* [The Arrow Tutorial on Haskell Wiki](http://www.haskell.org/haskellwiki/Arrow_tutorial)

If you're interested in learning the theory behind Arrows or want to gain a deeper insight, I strongly suggest that you read through the above links. 

Today's post is going to take you through Arrows in practice when working in Haskell. 

### returnA

[returnA](https://hackage.haskell.org/package/base-4.7.0.0/docs/Control-Arrow.html#v:returnA) gives you the identity arrow. It's what you use in place of [return](http://hackage.haskell.org/package/base-4.7.0.1/docs/Prelude.html#v:return) that you would use in monadic contexts.

{% highlight haskell %}
λ> :t returnA
returnA :: Arrow a => a b b
λ> :t returnA 5
returnA 5 :: Num b => b
{% endhighlight %}

### arr

[arr](https://hackage.haskell.org/package/base-4.7.0.0/docs/Control-Arrow.html#v:arr) will take an ordinary function and make an arrow out of it.

{% highlight haskell %}
λ> let a1 = arr (+)
λ> :t a1
a1 :: (Arrow a, Num b) => a b (b -> b)
{% endhighlight %}

Invoking your arrow is simple now with `returnA` and `arr`.

{% highlight haskell %}
λ> a1 (returnA 5) 6
11
{% endhighlight %}

### >>>

[>>>](http://hackage.haskell.org/package/base-4.7.0.1/docs/Control-Arrow.html#v:-62--62--62-) performs left to right composition on arrows. This is very similar to what [>>=](http://hackage.haskell.org/package/base-4.7.0.1/docs/Prelude.html#v:-62--62--61-) provides for monads.

{% highlight haskell %}
λ> let a1 = arr (+5)
λ> let a2 = arr (*2)
λ> a1 >>> a2 $ 3
16
{% endhighlight %}

The next few functions that I'll list here will work on pairs of values. This is where arrows really start to pull away from monads in terms of compositional capability. For example's sake, I've defined `q` as a simple pair of integers:

{% highlight haskell %}
λ> let q = (1,2)
{% endhighlight %}

### first

[first](http://hackage.haskell.org/package/base-4.7.0.1/docs/Control-Arrow.html#v:first) feeds the first element of a pair into the arrow for processing and leaves the second item untouched.

{% highlight haskell %}
λ> first a1 q
(6,2)
{% endhighlight %}

### second

[second](http://hackage.haskell.org/package/base-4.7.0.1/docs/Control-Arrow.html#v:second) feeds the second element, but leaves the first untouched.

{% highlight haskell %}
λ> second a1 q
(1,7)
{% endhighlight %}

### ***

[***](http://hackage.haskell.org/package/base-4.7.0.1/docs/Control-Arrow.html#v:-42--42--42-) will feed the first element of a pair through the first arrow, the second element will go through the second arrow. In the example below, the value `1` goes through the arrow `a1` and `2` goes through `a2`.

{% highlight haskell %}
λ> a1 *** a2 $ q
(6,4)
{% endhighlight %}

### &&&

[&&&](http://hackage.haskell.org/package/base-4.7.0.1/docs/Control-Arrow.html#v:-38--38--38-) will duplicate the input given to it and feed a copy to each arrow, returning a pair.

{% highlight haskell %}
λ> a1 &&& a2 $ 5
(10,10)
{% endhighlight %}

### proc

[proc](http://www.haskell.org/haskellwiki/Keywords#proc) (arrow abstraction) builds a lambda that constructs an arrow as opposed to a function. `proc` allows you to build expressions (or arrows) using [arrow notation](http://www.haskell.org/ghc/docs/latest/html/users_guide/arrow-notation.html).

{% highlight haskell %}
{-# Language Arrows #-}

import Control.Arrow

addA :: Arrow a => a b Int -> a b Int -> a b Int
addA f g = proc x -> do
       	    y <- f -< x
	    z <- g -< x
	    returnA -< y + z
{% endhighlight %}

In this example, the type signature for `addA` is asking for two arrows from `b` to `Int` (`a b Int`) and will return an arrow of the same type. 

The `proc` block has a lambda variable of `x` which is applied when the arrow is invoked. Remember, we're only constructing an arrow here - not running it (yet). 

The following lines make use of a new operator `-<` (arrow application). It feeds the value of an expression into an arrow. I think it helps to look at it like this:

<strong>variable binding pattern</strong> `<-` <strong>arrow</strong> `-<` pure expression giving arrow input

Invoking `addA` is done like so:

{% highlight haskell %}
λ> let a1 = arr (+4)
λ> addA a1 a1 (returnA 3)
14
{% endhighlight %}

Here we have our arrow `a1` that is just `(+4)`. `a1` is being supplied as the first and second parameter of `addA`. Lastly, we give `addA` our pure value `returnA 3`.

So, you can see here that `(+4)` has been applied to 3, and then `(+4)` gets applied to 3. Yes, I said the same thing twice. It's only because I've used `a1` as both input parameters. The output of these arrow invocations is then added together, to give the result of 14. Our pure value is being supplied by `returnA 3`.

These have just been some very fundamental examples of arrow usage. Read up some more on them using the links I've provided above. You can see how they're quite a powreful construct.
