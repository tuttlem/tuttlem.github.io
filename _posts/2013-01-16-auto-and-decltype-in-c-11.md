---
layout: post
title: auto and decltype in C++11
date: 2013-01-16
comments: false
categories: [ "C++", "C++11", "auto", "decltype" ]
---

### Introduction

Type brevity has never been C++'s strong suit, especially when you start dealing with template classes. It's just a mess! One of the nifty features that comes along with the C++11 standard is the ability to not need to specify the type. This leaves it as the job for the compiler to complete. This will only be a short post on `auto` and `decltype`'s usage.

### Usage

To use the `auto` keyword, just declare your variables as `auto`. Here are some variables for some simple data types

{% highlight cpp %}
// simple data types
auto i = 10;
auto ch = 'a';
auto f = 9.2f;
{% endhighlight %}

One of the problems I've always had, iterating over STL containers is how verbose the type becomes when you declare your iterator. You can use the "auto" keyword here to simplify this greatly now.

{% highlight cpp %}
// what was this ..
std::vector<std::string>::iterator i = v.begin();

// now becomes this
auto i = v.begin();
{% endhighlight %}

That is an improvement out of sight! `decltype` operates along the same paradigm but instead of operating on a variable's type, it will take the type of an expression's result and allow you to bind a name to it.

{% highlight cpp %}
// a list of names
vector<string> names;

// declare the iterator type for the list
typedef decltype (names.begin()) name_it;

// reuse the declared type
name_it another;
{% endhighlight %}

Using `auto` throughout your code guarantees you that there won't be any conversions going on to that variable. This in itself is a few layers of performance sapping translation gone! Just the cleanliness of the code is worth its weight in gold!

