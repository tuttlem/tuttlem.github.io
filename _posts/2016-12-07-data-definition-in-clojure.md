---
layout: post
title: Data definition in Clojure
date: 2016-12-07
comments: false
categories: [ "clojure", "map", "record" ]
---

## Introduction

In today's article, I'll briefly go over representing record-like information in [Clojure](http://clojure.org/).

## Maps

First off, we'll take a quick tour using a [Map](http://clojure.org/reference/data_structures#Maps). 

> A `Map` is a collection that maps keys to values

So this is a pretty fundamental data type that we can use. We can pretty easily represent a person using it:

{% highlight plain %}
user=> (def person { :first-name "John" :last-name "Smith" :age 21 })
#'user/person
user=> user/person
{:first-name "John", :last-name "Smith", :age 21}
{% endhighlight %}

We can use [count](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/count) to count the number of pairs in the map.

{% highlight plain %}
user=> (count person)
3
{% endhighlight %}

We can use [conj](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/conj) to give our person a `hair` attribute.

{% highlight plain %}
user=> (conj person { :hair :brown })
{:first-name "John", :last-name "Smith", :age 21, :hair :brown}
{% endhighlight %}

To navigate the map a little bit we can use [get](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/get), [contains?](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/contains?), [find](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/find), [keys](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/keys), [vals](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/vals).

Using [seq](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/seq) you can return a seq over the pairs; on each item in the seq you can use [key](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/key) to get the value of the key and [val](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/val) to get the value.

{% highlight plain %}
user=> (map #(println (key %1)) (seq person))
:first-name
:last-name
:age
(nil nil nil)
{% endhighlight %}

This gives us a full key/value pairing structure for us to arbitrarily represent data in a organized fashion; but not organized enough.

## Records

Using records or [defrecord](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/defrecord) we can turn a list of keys into a type that we can repeatably construct.

{% highlight plain %}
user=> (defrecord Person [ first-name last-name age ])
user.Person
user=> (def person-rec (->Person "Mary" "Jane" 25))
#'user/person-rec
user=> user/person-rec
#user.Person{:first-name "Mary", :last-name "Jane", :age 25}
{% endhighlight %}

We're afforded all of the same functions above to work on this value; but we're given the positional factory function [->Constructor](http://clojure.github.io/clojure/clojure.reflect-api.html#clojure.reflect/->Constructor) so that we can construct our types in a much more intuitive way.

## Domain functions

Now that we've spent a little bit of time creating data structures and defining data records, we can create functions that will allow us to perform operations on this data. Domain functions in Clojure perform operations specific to the data structure. In this following example, we've created a function to format a person's full name:

{% highlight plain %}
user=> (defn get-full-name [p] (str (get p :first-name) " " (get p :last-name)))
#'user/get-name
user=> (user/get-full-name person)
"John Smith"
{% endhighlight %}

[Object-oriented programming](https://en.wikipedia.org/wiki/Object-oriented_programming) gives us [polymorphism](https://en.wikipedia.org/wiki/Polymorphism_(computer_science)) by allowing us to implement a class's method differently per type that we derive from a common base. Clojure, gives us this multi-dispatch effect (choosing the right function for the given data-type) through [multi methods](http://clojure.org/reference/multimethods) and [protocols](http://clojure.org/reference/protocols).

## Multi-methods

We're going to use the [defmulti](https://clojuredocs.org/clojure.core/defmulti) macro to define our multi method and we'll provide implementations using the [defmethod](https://clojuredocs.org/clojure.core/defmethod) macro. From the documentation:

> Clojure eschews the traditional object-oriented approach of creating a new data type for each new situation, instead preferring to build a large library of functions on a small set of types. However, Clojure fully recognizes the value of runtime polymorphism in enabling flexible and extensible system architecture. Clojure supports sophisticated runtime polymorphism through a multimethod system that supports dispatching on types, values, attributes and metadata of, and relationships between, one or more arguments.

We're going to expand the `Person` example above, by adding a new record of `Company`. 

{% highlight plain %}
user=> (defrecord Person [first-name last-name age])
user=> (defrecord Company [name number])
{% endhighlight %}

Then we'll define our multi method called `get-full-name`. Its job is to put together the name of our entity. Because we have both a `Person` and `Company` type entity, we're going to need two different implementations:

{% highlight plain %}
user=> (defmulti get-full-name (fn [entity] (class entity)))
user=> (defmethod get-full-name Person [person] (str (get person :first-name) " " (get person :last-name)))
user=> (defmethod get-full-name Company [company] (get company :name))
{% endhighlight %}

The `Person` implementation of the `get-full-name` function concatenates the `:first-name` and `:last-name` attributes together, where as the `Company` implementation need only return the `:name` attribute.

Something that is interesting and unique to multi-methods is value-based dispatch; we've already seen type-based dispatch. 

Consider a temperature conversion between Fahrenheit and Celsius. We create our multi method the same way, but this time we need to give the parameter values identity:

{% highlight plain %}
(defmulti convert-temp (fn [src-unit dest-unit amount] [src-unit dest-unit]))
{% endhighlight %} 

We can now give our multi method some implementations based on the source and destination units passed:

{% highlight plain %}
;; C to F
(defmethod convert-temp [:c :f] [_ _ c] (+ 32 (* c 1.8)))

;; F to C
(defmethod convert-temp [:f :c] [_ _ f] (/ (- f 32) 1.8))

;; anything else
(defmethod convert-temp :default 
  [su du a] 
  (if (= su du) 
    a 
    (assert false "Conversion between units was not defined")))
{% endhighlight %}

We can now test out that the dispatching works:

{% highlight plain %}
user=> (convert-temp :f :c 100)
37.77777777777778
user=> (convert-temp :c :f 37.77777777777778)
100.0
user=> (convert-temp :c :c 50)
50
user=> (convert-temp :c :k 50)

AssertionError Assert failed: Conversion between units was not defined
false  user/eval1317/fn--1318 (form-init7827911538193486373.clj:1)
{% endhighlight %}

## Protocols

Protocols are a little more reminiscent of object-oriented programming in a sense that they are closely related to interfaces. From the documentation:

> Clojure is written in terms of abstractions. There are abstractions for sequences, collections, callability, etc. In addition, Clojure supplies many implementations of these abstractions. The abstractions are specified by host interfaces, and the implementations by host classes. While this was sufficient for bootstrapping the language, it left Clojure without similar abstraction and low-level implementation facilities. The protocols and datatypes features add powerful and flexible mechanisms for abstraction and data structure definition with no compromises vs the facilities of the host platform.

So, protocols give us a way to defining abstractions. We can treat our `Person` and `Company` scenario as such, by calling them a `Party`.

We use [defprotocol](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/defprotocol) to start our abstraction definition. [extend-protocol](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/extend-protocol) is then used to supply implementations.

{% highlight plain %}
user=> (defprotocol Party
  #_=>   (get-full-name [entity]))

user=> (extend-protocol Party
  #_=>   Person
  #_=>   (get-full-name [person] (str (get person :first-name) " " (get person :last-name))))

user=> (extend-protocol Party
  #_=>   Company
  #_=>   (get-full-name [company] (get company :name)))
{% endhighlight %}

## Wrapping up

This has been a brief tour on creating map/record data and some domain functions to work with them.

