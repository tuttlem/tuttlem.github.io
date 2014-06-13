---
layout: post
title: C++ References
date: 2013-01-15
comments: false
---

### Introduction

I've always thought of a reference as the half-way house between pointers and statically allocated objects. References are in-fact addresses but they are used within our code just like objects as opposed to requiring pointer syntax.

### Some facts ..

<strong>How reference are defined</strong>

You declare a reference variable using the ampersand `&` to modify the type declaration.

{% highlight cpp %}
type& var;
{% endhighlight %}

<strong>A reference must be initialised</strong>

This is pretty basic, it just means that when you declare your reference it must start out with a place to reference.

{% highlight cpp %}
// this is ok
int val = 90;
int& ref = val;

// this will not compile
int& ref;
{% endhighlight %}

<strong>A reference cannot be changed</strong>

When we initialise a reference to point to a variable, that's it. We can't change what the reference points to. This caught be out to begin with, but it's pretty easy stuff.

{% highlight cpp %}
int val1 = 90, val2 = 100;
int& ref = val1;

// prints out "90"
std::cout << val1 << std::endl;

// doesn't change ref, but changes the value
// of val1 to val2
ref = val2;

// prints out "100"
std::cout << val1 << std::endl;
{% endhighlight %}

Makes sense. Gives references a sense of stubbornness (and sanity).

<strong>Pointer compatibility through dereferencing</strong>

I see a fair bit of banter on "how to convert pointer to reference", etc. It's really quite simple and it's also subject to the same assignment laws as above.

{% highlight cpp %}
int i = 50, j = 60;
int* p = &i;
int& r = *p;

// prints 50 
std::cout << *p << std::endl;

// through this reference, we've changed "i" to 60
r = j;

// prints 60
std::cout << *p << std::endl;
{% endhighlight %}

These concepts really come into their own (I think) once you start using them within your own class structures. It's a much more natural feel to deal with references rather than pointers and a bit easier to read as well.