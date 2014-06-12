---
layout: post
title: Scanning in Haskell
date: 2012-12-31
comments: false
---

In a previous post, I had written about [Folding in Haskell]({ post_url 2012-12-27-folding-in-haskell }] which in itself is a very powerful tool. We spent a brief moment in that tutorial actually working through a problem and writing the folding process out long hand.

Well, scanning [scanl](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:scanl) and [scanr](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:scanr) does this for you! It shows you the reduction steps in the form of an array returned back to you.

A quick example:

{% highlight text %}
Prelude> scanl (*) 5 [1,2,3,4]
[5,5,10,30,120]

Prelude> foldl (*) 5 [1,2,3,4]
120
{% endhighlight %}

Here you can see the fold's process "working-sheet".

{% highlight text %}
5  * 1 = 5
5  * 2 = 10
10 * 3 = 30
30 * 4 = 120
{% endhighlight %}

120 being the overall answer as demonstrated by the foldl call above.

Easy.