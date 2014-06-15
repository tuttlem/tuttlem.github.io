---
layout: post
title: Clojure's spine: Java
date: 2013-01-12
comments: false
categories: [ "clojure", "programming", "java" ]
---

### Introduction

One of Clojure's greatest strengths is the fact that it sits on the JVM. This puts all of those jars that people have worked tirelessly over the years to produce right at your fingertips ready for use in your Clojure code. Today's post will just be a short tutorial on some of the most basic routines to use Java classes in your Clojure code. All of the examples that I'll produce are all based on the [Java_interop page](http://clojure.org/java_interop) on the Clojure site.

### Importing classes

First things first. You'll need to import the classes that you want to use. You don't have to, it just makes your code a little less verbose later on.

{% highlight clojure %}
(import 'java.net.URL)
{% endhighlight %}

We're now able to use the URL class in our code. If you're importing classes into your programs that aren't local and need to be downloaded as a dependency, I suggest you use [Leiningen](http://leiningen.org/) to do all the heavy lifting for you there. You'd just need to list your external package in the dependencies list in your project file and away you go.

### Using the classes

We need to construct some objects so that we'll be able to call methods on them, so following on from the example, we construct instances of the classes that we import like so.

{% highlight clojure %}
(def google (new URL "http://www.google.com"))
{% endhighlight %}

This is really getting the Java mojo mixed in with Clojure now. So "google" is our constructed object, we can start to call methods on this variable like so.

{% highlight clojure %}
(.getProtocol google)

(def yahoo (new URL "http://www.yahoo.com"))
(.sameFile google yahoo)
{% endhighlight %}

We were able to find out the protocol of the URL using [getProtocol](http://docs.oracle.com/javase/6/docs/api/java/net/URL.html#getProtocol()) and we were able to compare "google" and "yahoo" using [sameFile](http://docs.oracle.com/javase/6/docs/api/java/net/URL.html#sameFile(java.net.URL)).

### doto

The last thing I want to talk about is [doto](http://clojuredocs.org/clojure_core/1.2.0/clojure.core/doto). `doto` evaluates its first parameter than allows you to chain a series of calls (just as you would in Java using the `.` operator) together. Here's a an example using a [HashMap](http://docs.oracle.com/javase/6/docs/api/java/util/HashMap.html) class and chaining a few put's together. This statement will return the built `HashMap`.

{% highlight clojure %}
(doto (java.util.HashMap.) 
      (.put "First Name" "John") 
      (.put "Last Name" "Smith"))
{% endhighlight %}

Well, that's it for today. I'm off to write some Java .. I mean, Clojure, I mean ... you know what I mean.