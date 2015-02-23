---
layout: post
title: Working with lambdas in python
date: 2015-02-23
comments: false
categories: [ "python", "lambda" ]
---

Python provides a simple way to define [anonymous functions](http://en.wikipedia.org/wiki/Anonymous_function) through the use of the `lambda` keyword. Today's post will be a brief introduction to using lambdas in python along with some of the supported higher-order functions.

### Declaration

For today's useless example, I'm going to create a "greeter" function. This function will take in a name and give you back a greeting. This would be defined using a python function like so:

{% highlight python %}
def greet(name):
	return "Hello, %s." % (name,)
{% endhighlight %}

Invoking this function gives you endless greetings:

{% highlight text %}
>>> greet("Joe")
'Hello, Joe.'
>>> greet("Paul")
'Hello, Paul.'
>>> greet("Sally")
'Hello, Sally.'
{% endhighlight %}

We can transform this function into a lambda with a simple re-structure:

{% highlight python %}
greeter = lambda name: "Hello %s" % (name,)
{% endhighlight %}

Just to show you a more complex definition (i.e. one that uses more than one parameter), I've prepared a lambda that will execute the [quadratic formula](http://en.wikipedia.org/wiki/Quadratic_formula).

{% highlight python %}
from math import sqrt

quadratic = lambda a, b, c: ((-b + sqrt((b * b) - (4 * a * c))) / (2 * a), (-b - sqrt((b * b) - (4 * a * c))) / (2 * a))
{% endhighlight %}

This is invoked just like any other function:

{% highlight text %}
>>> quadratic(100, 45, 2)
(-0.05, -0.4)
>>> quadratic(100, 41, 2)
(-0.0565917792034417, -0.3534082207965583)
>>> quadratic(100, 41, 4)
(-0.16, -0.25)
{% endhighlight %}

### Higher order functions

Now that we're able to define some anonymous functions, they really come into their own when used in conjunction with higher-order functions. The primary functions here are `filter`, `map` and `reduce`.

We can <strong>filter</strong> a list of numbers to only include the even numbers.

{% highlight python %}
filter(lambda x: x%2 == 0, range(1, 10))
{% endhighlight %}

Of course it's the `lambda x: x%2 == 0` performing the even-number test for us.

We can <strong>reduce</strong> a list of numbers to produce the accumulation of all of those values:

{% highlight python %}
reduce(lambda x, y: x + y, range(1, 10))
{% endhighlight %}

Finally, we can transform a list or <strong>map</strong> a function over a list of numbers and turn them into their inverses:

{% highlight python %}
map(lambda x: 1.0/x, range(1, 10))
{% endhighlight %} 

