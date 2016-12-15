---
layout: post
title: Lists, vectors, sets and maps
date: 2016-12-15
comments: false
categories: [ "clojure", "list", "vector", "set", "map", "collection" ]
---

In a previous article, we'd spent a little bit of time working with maps in clojure; we also looked at formalising the structure of a map into records. In today's post, I'll discuss some of the collection types that will allows us to carry around many values at once.

## General

Before we start looking at the collection types, there are some general-purpose functions that you can use with these types. These are very helpful functions that really form the basis of your collection processing:

[first](https://clojuredocs.org/clojure.core/first) will return you the head of your collection; with [rest](https://clojuredocs.org/clojure.core/rest) providing you with everything else but the head. [last](https://clojuredocs.org/clojure.core/last) will return you the final item in your collection and [cons](https://clojuredocs.org/clojure.core/cons) will provide you with the ability to construct a new list with a given head and tail.

[nth](https://clojuredocs.org/clojure.core/nth) will give you indexed access to you collection.

## List

A list is an ordered collection of elements. From the documentation:

> Lists are collections. They implement the ISeq interface directly (except for the empty list, which is not a valid seq). [count](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/count) is O(1). [conj](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/conj) puts the item at the front of the list.

Because lists are a natural part of the language, they'll be invoked as a function if they're not defined correctly or quoted adequately. A list can be defined using the [list](https://clojuredocs.org/clojure.core/list*) function, but can also be quoted:

{% highlight plain %}
user=> (list :gibson :fender :ibanez)
(:gibson :fender :ibanez)
user=> '(:joe :john :peter)
(:joe :john :peter)
user=> '("a" "b" "c"))
("a" "b" "c")
user=> '(1 2 3)
(1 2 3)
{% endhighlight %}

## Vector

A vector is an ordered collection of elements, but is optimised for the random-access case. From the documentation:

> A Vector is a collection of values indexed by contiguous integers. Vectors support access to items by index in log32N hops. [count](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/count) is O(1). [conj](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/conj) puts the item at the end of the vector.

{% highlight plain %}
user=> [:gibson :fender :ibanez]
[:gibson :fender :ibanez]
{% endhighlight %}

You can use the [nth](https://clojuredocs.org/clojure.core/nth) function to get the value at an index, or you can just invoke the vector itself. It'll take an index, and return you the item:

{% highlight plain %}
user=> (nth [:gibson :fender :ibanez] 1)
:fender
user=> ([:gibson :fender :ibanez] 1)
:fender
{% endhighlight %}

## Set

A set is an un-ordered collection of items. You can use [sorted-set](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/sorted-set) if your application requires your set to remain in an ordered way.

Clojure allows you to wrap your values with `#{}` in order to define a set, literally. 

{% highlight plain %}
user=> #{:gibson :fender ibanez}
#{:gibson :fender ibanez}
{% endhighlight %}

Sets have methods to easily [union](https://clojuredocs.org/clojure.set/union) two sets, or find the [difference](https://clojuredocs.org/clojure.set/difference). 

## Map

### Definition

A map is a collection of key-value-pairs. Declaring maps allows you to define attribute keys and values in an organised or arbitrary way. From the documentation:

> A Map is a collection that maps keys to values. Two different map types are provided - hashed and sorted. Hash maps require keys that correctly support hashCode and equals. Sorted maps require keys that implement Comparable, or an instance of Comparator. Hash maps provide faster access (log32N hops) vs (logN hops), but sorted maps are, well, sorted. [count](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/count) is O(1). [conj](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/conj) expects another (possibly single entry) map as the item, and returns a new map which is the old map plus the entries from the new, which may overwrite entries of the old. 

{% highlight plain %}
user=> { :name "Michael" :age 21 }
{:name "Michael", :age 21}
user=> ({ :name "Michael" :age 21 } :name)
"Michael"

user=> (def person { :name "Michael" :age 21 })
#'user/person
user=> (assoc person :age 22)
{:name "Michael", :age 22}

user=> (def person-aux { :hair :brown })
#'user/person-aux
user=> (merge person person-aux)
{:name "Michael", :age 21, :hair :brown}
{% endhighlight %}

## Finishing up

This has been a quick lap around the collection types in Clojure.

