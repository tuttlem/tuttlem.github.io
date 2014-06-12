---
layout: post
title: Haskell's Function Application Operator ($)
date: 2012-12-31
comments: false
---

Sometimes, Haskell's syntax is so alien to read (to my eyes at least anyway). I've seen wide-spread use of the `$` operator all over lots of people's code and never really had a grasp on what it is/does. In the end, it's really quite simple. The whitespace character has a very high precedence order, so when you're using spaces the precedence order looks like this.

{% highlight haskell %}
f a b c = ((f a) b) c
{% endhighlight %}

This is classified as being left-associative. In contrast, using the `$` operator allows us to be right-associative. An example.

{% highlight haskell %}
f $ a $ b $ c = f (a (b c))
{% endhighlight %}

Looking at this, it's starting to look very much how our programming languages are structured with function calls. We're very right-associative. Because Haskell uses the white space character to denote left-associations, it takes a lot of parenthesis to make a complex state right-associative as you need to change the precedence order by hand. This is the true power of the `$` function. We can use `$` to free us from parenthesising everything, so that a transformation as below occurs.

{% highlight haskell %}
putStrLn (show num)
putStrLn $ show num
{% endhighlight %}

This simple scenario doesn't illustrate exactly how much `$` will help us out. Here's another slightly more complex scenario. It becomes clear here that `$` is working to make our code more readable.

{% highlight haskell %}
sum (take 10 (cycle [1,2,3]))
sum $ take 10 $ cycle [1,2,3]
{% endhighlight %}

In character-space length they're equivalent, but the second version looks less LISP-y. I guess this is what was being aimed at.

Anyway, until next time.