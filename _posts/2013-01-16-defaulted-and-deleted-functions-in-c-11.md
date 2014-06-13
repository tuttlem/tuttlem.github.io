---
layout: post
title: Defaulted and Deleted Functions in C++11
date: 2013-01-16
comments: false
---

A default (and more efficient) implementation can be given to your functions using the `default` keyword. This is the usage of a defaulted function in C++11. In this example, my `person` class has no written implementation for its constructor or destructor.

{% highlight cpp %}
class person {
  public:
    person(void) = default;
    virtual ~person(void) = default;
};
{% endhighlight %}

The opposite to a defaulted function is a deleted function. The deleted function allows you to remove the implementation of a function by specifying the `delete` keyword. In C++ this is useful to us if we want to remove the copy constructor from classes that C++ so nicely provides for us. In this example, you can see that we've shut down the copy constructor as well as the assignment operator so that copying will no longer be supported.

{% highlight cpp %}
class person {
  public:
    person(void) = default;
    person(const person&) = delete;
    virtual ~person(void) = default;
    
    person& operator =(const person&) = delete;
};
{% endhighlight %}

That's it for these two features. Simple, but effective.
