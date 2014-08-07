---
layout: post
title: What is Weak Head Normal Form?
date: 2014-08-07
comments: false
categories: ["haskell", "whnf"]
---

### Introduction

Lazy languages provide a way for developers to define expressions without necessarily forcing them to evaluate immediately. Rather than provide an immediate value, these languages will generate a [thunk](http://www.haskell.org/haskellwiki/Thunk) instead.

### Show me

If you load up GHCi and bind an expression to a name:

{% highlight haskell %}
λ> let five = 2 + 3 :: Int
{% endhighlight %}

We can check if this expression has been evaluated or not by using `sprint`. If the expression hasn't been evaluated, `sprint` will show us an underscore `_`. This is how GHCi tells us that an expression is <strong>unevaluated</strong>.

{% highlight haskell %}
λ> :sprint five
five = _
{% endhighlight %}

`five` is currently a thunk.

If we do force the expression to evaluate and then re-run this test, `sprint` tells us a different story:

{% highlight haskell %}
λ> five
5
λ> :sprint five
five = 5
{% endhighlight %}

`five` has now been evaluated and as such, `sprint` is telling us the value.

### Weak Head Normal Form

With some knowledge of thunks under our belt, we can move onto Weak Head Normal Form or WHNF. If we take our `five` example back to unevaluated, and use a mixture of `take` and `cycle` to generate a list of `five`, we'll end up with another thunk:

{% highlight haskell %}
λ> let five = 2 + 3 :: Int
λ> let fiveFives = take 5 $ cycle [five]
λ> :sprint fiveFives
fiveFives = _
{% endhighlight %}

If we use `seq` on this list, `fiveFives` we end up with two thunks getting concatenated.

{% highlight haskell %}
λ> seq fiveFives []
[]
λ> :sprint fiveFives
fiveFives = _ : _
{% endhighlight %}

`seq` has evaluated `fiveFives` to head normal form here. In fact, hoogle says the following about `seq`:

> Evaluates its first argument to head normal form, and then returns its second argument as the result.

`seq` is defined as follows

{% highlight haskell %}
seq :: a -> b -> b
{% endhighlight %}

So, `seq` forced the list to be evaluated but not the components that make up the list. This is known as weak head normal form.

### In summary

From [this stack overflow question](http://stackoverflow.com/questions/6872898/haskell-what-is-weak-head-normal-form):

> An expression in weak head normal form has been evaluated to the outermost data constructor or lambda abstraction (the head).


