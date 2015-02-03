---
layout: post
title: Using yield to create generators in python
date: 2015-02-03
comments: false
categories: [ "python", "yield", "generator" ]
---

[Generators](https://wiki.python.org/moin/Generators) are python functions that act like iterators. This abstraction allows you to simplify a lot of your for-loop code, implement [lazy evaluation](http://en.wikipedia.org/wiki/Lazy_evaluation) and even create more intelligent value producing iterators.

In today's post, I'll go through a basic usage of the `yield` keyword; the generator that's created as a result and how you can interact with this function type.

### Prime number example

To produce the generator, I've written a function that will filter through numbers picking out prime numbers. The algorithm isn't highly optimised. It's quite crude/brute-force in its approach, but it'll be enough for us to understand the generator function.

{% highlight python %}
import math

def primes():
	ps, cur = [2], 3
	yield 2
	while True:
		y = int(math.sqrt(cur))
		c = next((x for x in ps if x < y and (cur % x) == 0), None)

		if c == None:
			yield cur
			ps.append(cur)

		cur += 2
{% endhighlight %}

We're maintaining an internal list of primes that we've found. When we come across a potential candidate, we try to divide it by primes that we've already found. To cut down on the number of divides, we only go for numbers lower than the square root of the candidate.

Note the use of `yield`. As we call `yield`, this makes another value available in the iterator. You can see that this is an iterator that doesn't end. Well, it will end - once the integer data type overflows. If we were using a data type that wasn't susceptible to this type of overflow, we'd only be limited by the amount of memory in the machine. 

### Iterating

So, we've created what appears to be an infinite list. Testing it out in the REPL:

{% highlight text %}
>>> ps = primes()
>>> ps
<generator object primes at 0x7fa1396e8af0>
>>> ps.next()
2
>>> ps.next()
3
>>> ps.next()
5
>>> ps.next()
7
>>> ps.next()
9
>>> ps.next()
11
{% endhighlight %}

`ps` is the generator, and we're able to call the `next` function on it. As we do that, we progress through the iterator. We can start to work with `ps` now as if it were any other [iterator](https://docs.python.org/2/tutorial/classes.html#iterators).

Using a list comprehension, we can find the first 10 primes:

{% highlight text %}
>>> ps = primes()
>>> [ps.next() for _ in xrange(1, 10)]
[2, 3, 5, 7, 9, 11, 13, 15, 17]
{% endhighlight %}

Using [itertools](https://docs.python.org/2/library/itertools.html) we can get all of the prime numbers under 100:

{% highlight text %}
>>> import itertools
>>> list(itertools.takewhile(lambda x: x < 100, primes()))
[2, 3, 5, 7, 9, 11, 13, 15, 17, 19, 23, 25, 29, 31, 35, 37, 41, 43, 47, 49, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]
{% endhighlight %}

`yield` allows you to make generators which are the potential to create values, as opposed to the values themselves. It's not until you start to iterate over the generator that the values start to materialise.

