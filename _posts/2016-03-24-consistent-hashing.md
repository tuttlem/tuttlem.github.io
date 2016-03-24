---
layout: post
title: Consistent Hashing
date: 2016-03-24
comments: false 
categories: [ "hash", "hashing", "consistent", "distributed" ]
---

In today's post, I'm going to run through how [Consistent hashing](https://en.wikipedia.org/wiki/Consistent_hashing) differs from standard modulus distribution and how it can help distributed key searches.

From the [wikipedia article]((https://en.wikipedia.org/wiki/Consistent_hashing)):

> Consistent hashing is a special kind of [hashing](https://en.wikipedia.org/wiki/Hash_function) such that when a hash table is resized, only K/n keys need to be remapped on average, where K is the number of keys, and n is the number of slots. In contrast, in most traditional [hash tables](https://en.wikipedia.org/wiki/Hash_table), a change in the number of array slots causes nearly all keys to be remapped.

### Using a simple modulus
 
A simple way to balance information between a set of nodes is to take a simple modulus of a hash. The hash of an object is taken; a modulus is calculated with respect to the number of nodes. The information is then assigned to that node:

{% highlight text %}
Number of Nodes = 5
"Joe"           = 348561223
Node            = 348561223 mod 5
                = 3
{% endhighlight %}

So, the string `"Joe"` is then destined to live on node `3`.

This sort of balancing gets the information distributed, but starts to really show problems when nodes are added or removed from the set. Under these operations there are large suffles of information between nodes required.

### How it Works

The aim here is to lower the sensitivity of a piece of information's hash identity amongst replicas. This way, we still reap the benefits of being in a distributed system but we don't incur such a loss at the time of adding or removing nodes. Minimalising this disturbance is what consistent hashing aims to solve.

To achieve consistent hashing, not only the key by which the information is retrieved is cached; so do the nodes managing the information. Both of these elements are added to a [ring buffer](https://en.wikipedia.org/wiki/Circular_buffer). When this system gets exercised, and a client is looking for information; the key that they're looking for will land _somewhere_ on the circular buffer. We continually move clockwise through the buffer until we hit a node to find our information.

Adding and removing nodes from the ring buffer is a matter of distribution from a neighbour now, rather than the whole set.

One problem is the distribution of nodes along the ring buffer. If these nodes clump together, there will be a large hash-space empty that a lot of queries could hit. Adding replicas of each node to the hash seems to saturate the ring sufficiently to mitigate this problem. 

### Implementations

There are many implementations of consisten hashing available. It's a good exercise to implement one yourself by hand, but these problems have already been solved for you. Some of the better known uses can be found in projects like [Openstack](https://en.wikipedia.org/wiki/Openstack), Amazon's [Dynamo](https://en.wikipedia.org/wiki/Dynamo_(storage_system)) and Apache [Cassandra](https://en.wikipedia.org/wiki/Apache_Cassandra).

There are much simpler examples to look at also:

* [Consistent hashing](http://michaelnielsen.org/blog/consistent-hashing/)
* [The Absolutely Simplest Consistent Hashing Example](http://techspot.zzzeek.org/2012/07/07/the-absolutely-simplest-consistent-hashing-example/)
* [Consistent Hash Ring](http://www.martinbroadhurst.com/Consistent-Hash-Ring.html)
