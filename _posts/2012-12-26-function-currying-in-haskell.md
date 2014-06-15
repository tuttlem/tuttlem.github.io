---
layout: post
title: Function currying in Haskell
date: 2012-12-26
comments: false
categories: [ "Function currying", "Haskell", "Programming" ]
---

I think it's important to follow up my previous post on [anonymous functions]({% post_url 2012-12-26-anonymous-functions-in-haskell %}) with a post on currying. One of the more difficult concepts to think about (only because Haskell does a great job of separating you from this) is that every function only has 1 argument.

Take this basic greeting example which expects the name of someone who is doing the greeting and the name of someone who is being greeted:

{% highlight haskell %}
let sayHello toWho fromWho = fromWho ++ " says Hello to " ++ toWho
{% endhighlight %}

This is a pretty trivial example. Give it two names and the computer will appear to play nice between these two people:

{% highlight text %}
*Main> sayHello "John" "Peter"
"Peter says Hello to John"
{% endhighlight %}

We like John so much, that we're going to make a new function using this existing one.

{% highlight haskell %}
let sayHelloToJohn = sayHello "John"
{% endhighlight %}

So now, we can get anyone to play nice with John:

{% highlight text %}
*Main> sayHelloToJohn "Peter"
"Peter says Hello to John"
*Main> sayHelloToJohn "Joe"
"Joe says Hello to John"
*Main> sayHelloToJohn "Jane"
"Jane says Hello to John"
{% endhighlight %}

Great! We've just made a <strong>partially applied</strong> function. When you don't specify enough parameters to a function, you're actually returned a function (or, partially applied function) that you can continue to use. Breaking down how this works, when Jane is saying hello to John she is actually doing so by doing this:

{% highlight haskell %}
(sayHello "John") "Jane"
{% endhighlight %}

This should at least explain my outlandish claims above of functions only having one argument, anyway. You've just witnessed function currying in motion. These principles are also directly applicable on infix functions as well, they just need a little extra help to be told so. Take this for example:

{% highlight haskell %}
double :: (Floating a) => a -> a
double = (*2)
{% endhighlight %}

Ignoring the function definition, you can see that all you need to do for infix functions is to surround then with parenthesis. You need to supply the value that makes it a partial application of course!