---
layout: post
title: Clojure threading macros
date: 2017-10-02
comments: false
categories: [ "macro", "clojure" ]
---

A [Threading Macro]() in Clojure is a utility for representing nested function calls in a linear fashion. 

## Simple transformations

Meet `mick`. 

{% highlight clojure %}
user=> (def mick {:name "Mick" :age 25})
#'user/mick
{% endhighlight %}

He's our *subject* for today. 

If we wanted to give `mick` an `:occupation`, we could simply do this using `assoc`; like so:

{% highlight clojure %}
user=> (assoc mick :occupation "Painter")
{:name "Mick", :age 25, :occupation "Painter"}
{% endhighlight %}

At the same time, we also want to take note of his earning for the year:

{% highlight clojure %}
user=> (assoc mick :occupation "Painter" :ytd 0)
{:name "Mick", :age 25, :occupation "Painter", :ytd 0}
{% endhighlight %}

Keeping in mind that this isn't actually changing `mick` at all. It's just associating new pairs to him, and returning the new object. 

`mick` got paid, $100 the other week, so we increment his `:ytd` by 100. We do this by performing the transformation *after* we've given him the attribute.

{% highlight clojure %}
user=> (update (assoc mick :occupation "Painter" :ytd 0) :ytd + 100)
{:name "Mick", :age 25, :occupation "Painter", :ytd 100}
{% endhighlight %}

He earned another $32 as well, in another job.

{% highlight clojure %}
user=> (update (update (assoc mick :occupation "Painter" :ytd 0) :ytd + 100) :ytd + 32)
{:name "Mick", :age 25, :occupation "Painter", :ytd 132}
{% endhighlight %}

He also got a dog.

{% highlight clojure %}
user=>  (assoc (update (update (assoc mick :occupation "Painter" :ytd 0) :ytd + 100) :ytd + 32) :pets [:dog])
{:name "Mick", :age 25, :occupation "Painter", :ytd 132, :pets [:dog]}
{% endhighlight %}

So, the nesting gets out of control. Quickly.

## Thread first macro

We'll use `->` (The thread-first macro) to perform all of these actions in one form (must as we've done above), but in a much more readable manner.

{% highlight clojure %}
user=> (-> mick
  #_=>   (assoc :occupation "Painter" :ytd 0)
  #_=>   (update :ytd + 100)
  #_=>   (update :ytd + 32)
  #_=>   (assoc :pets [:dog]))
{:name "Mick", :age 25, :occupation "Painter", :ytd 132, :pets [:dog]}  
{% endhighlight %}

So, it's the same result; but with a much cleaner and easier to read interface.

## Thread last macro

We saw above that the `->` threading macro works well for bare values being passed to forms. When the problem changes to the value not being supplied in the initial position, we use thread last `->>`. The value that we're threading appears as the last item in each of the transformations, rather than the `mick` example where they were the first.

{% highlight clojure %}
user=> (filter #(> % 12) (map #(* % 5) [1 2 3 4 5]))
(15 20 25)
{% endhighlight %}

We multiply the elements of the vector `[1 2 3 4 5]` by `5` and then filter out those items that are greater than `12`.

Again, nesting quickly takes over here; but we can express this with `->>`:

{% highlight clojure %}
user=> (->> [1 2 3 4 5]
  #_=>   (map #(* % 5) ,,,)
  #_=>   (filter #(> % 12) ,,,))
(15 20 25)
{% endhighlight %}

Again, this is a much more readable form.

## as

If the insertion point of the threaded value varies, we can use `as->` to alias the value.

{% highlight clojure %}
user=> (as-> "Mick" n
  #_=>   (clojure.string/upper-case n)
  #_=>   (clojure.string/reverse n)
  #_=>   (.substring n 1))
"CIM"
{% endhighlight %}

Take the name "Mick"
* Convert it to upper case
* Reverse it
* Substring, skipping the first character

It's the `substring` call, which takes the string in the initial position that's interesting here; as it's the only call that does that. `upper-case` and `reverse` take it in as the only (or last).

## some

The two macros `some->` and `some->>` work like their `->` and `->>` counterparts; only they do it on Java interop methods.

## cond

`cond->` and `cond->>` will evaluate a set of conditions, applying the threaded value to the front to back of any expression associated to a condition that evaulates true.

The following example has been taken [from here](https://clojure.org/guides/threading_macros).

{% highlight clojure %}
(defn describe-number [n]
  (cond-> []
    (odd? n) (conj "odd")
    (even? n) (conj "even")
    (zero? n) (conj "zero")
    (pos? n) (conj "positive")))
{% endhighlight %}

So you can describe a number as you go:

{% highlight text %}
user=> (describe-number 1)
["odd" "positive"]
user=> (describe-number 5)
["odd" "positive"]
user=> (describe-number 4)
["even" "positive"]
user=> (describe-number -4)
["even"]
{% endhighlight %}


