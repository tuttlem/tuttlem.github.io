---
layout: post
title: Convenience and performance with Python collections
date: 2016-01-17
comments: false
categories: [ "python", "collection" ]
---

The [collections](https://docs.python.org/2/library/collections.html) library included with [python](https://www.python.org/) has some very helpful utilities to make your programming life a little easier. In today's post, I'm going to go through a few of them.

### Named tuple

This is really the feature that brought my attention to this library, initially. Where a [tuple](https://docs.python.org/2/tutorial/datastructures.html#tuples-and-sequences) is an immutable set of values that are unnamed, you can create a class using the [namedtuple()](https://docs.python.org/2/library/collections.html#collections.namedtuple) function to bring a little more formality to your types:

{% highlight python %}
import collections
Person = collections.namedtuple('Person', ['firstName', 'lastName', 'age'])
joe = Person("Joe", "Smith", 21)

# joe is now as follows
Person(firstName='Joe', lastName='Smith', age=21)
{% endhighlight %}

That's a neat shortcut.

### Counter

A [Counter](https://docs.python.org/2/library/collections.html#collections.Counter) class is a [dict](https://docs.python.org/2/library/stdtypes.html#dict) object that when queried for a key that doesn't exist, will return a 0; and create that item ready for counting.

{% highlight python %}
animals = ['dog', 'cat', 'cat', 'bat', 'mouse', 'dog', 'elephant']
c = collections.Counter()

for animal in animals:
    c[animal] += 1

# "c" now looks like this
# Counter({'dog': 2, 'cat': 2, 'bat': 1, 'elephant': 1, 'mouse': 1})
{% endhighlight %}

Pretty handy.

### deque

A basic stack or queue like data structure can be initialized with the use of the [deque](https://docs.python.org/2/library/collections.html#collections.deque) class. As this object does look a lot like a `list`, it's important to remember why it exists:

> Though `list` objects support similar operations, they are optimized for fast fixed-length operations and incur O(n) memory movement costs for `pop(0)` and `insert(0, v)` operations which change both the size and position of the underlying data representation.

This tells us that some internal implementation assumptions have been made to tailor the runtime usecase of this class specifically for statically sized queues.

{% highlight python %}
orders = collections.deque()
orders.append({ 'name': 'Mario', 'pizza': 'Cheese' })
orders.append({ 'name': 'Joe', 'pizza': 'Supreme' })
orders.append({ 'name': 'Tony', 'pizza': 'Pepperoni' })

orders.popleft()
# {'name': 'Mario', 'pizza': 'Cheese'}

orders.popleft()
# {'name': 'Joe', 'pizza': 'Supreme'}

orders.popleft()
# {'name': 'Tony', 'pizza': 'Pepperoni'}
{% endhighlight %}

