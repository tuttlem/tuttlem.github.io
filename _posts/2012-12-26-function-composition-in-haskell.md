---
layout: post
title: Function composition in Haskell
date: 2012-12-26
comments: false
---

[Function composition](http://en.wikipedia.org/wiki/Function_composition) is a no-brainer concept [for Haskell](http://www.haskell.org/haskellwiki/Function_composition) that makes a lot of sense. It's truly aligned with its mathematical equivelant where you have two given functions:

{% highlight text %}
f(x) = x * x
g(x) = x + 2
{% endhighlight %}

The composition part comes in when you nest these functions, so that you end up with:

{% highlight text %}
f(g(x)) = f(x + 2)
f(g(x)) = (x + 2) * (x + 2)
f(g(x)) = x^2 + 4x + 4
{% endhighlight %}

Mathematically, this is cake. We just nest the second function inside the first. If we look at it with respect to Haskell we end up with this:

{% highlight haskell %}
-- | g(x) = x + 2
let g x = x + 2

-- | f(x) = x * x
let f x = x * x

-- | f(g(x))
let fg = f . g
{% endhighlight %}

So, the `.` operator is doing the composition here for us. Below I've got the mathematical representation on the left, Haskell on the right.

{% highlight text %}
f(g(x)) = (f . g) x
{% endhighlight %}

The real power here is that you can use function composition with any two functions, just as long as the return type of the second function is the same as the argument taken by the first function.

Slick.
