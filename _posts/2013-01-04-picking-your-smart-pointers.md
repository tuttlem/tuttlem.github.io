---
layout: post
title: Picking your Smart Pointers
date: 2013-01-04
comments: false
categories: [ "C++", "Programming", "boost", "smart pointers" ]
---

### Introduction

I wanted to do a quick write up on the [Boost library's](http://www.boost.org/) [Smart Pointers](http://www.boost.org/doc/libs/1_52_0/libs/smart_ptr/smart_ptr.htm) and when they should and shouldn't be used. I'd hope to use this post in future (if the knowledge doesn't stick in my head) as a rough guide when finding the right tool for the job. For the uninitiated, I strongly advise that you go through the [smart pointer](http://www.boost.org/doc/libs/1_52_0/libs/smart_ptr/smart_ptr.htm) documentation on the Boost website as it's a real eye-opener as to just how hands-off you can now be with dynamic memory allocation in C++ (these days).

### Why use Smart Pointers?

Smart pointers will help you manage the lifetime of your objects. It will force you to think about the ownership of these pointers and who's currently "in-charge" in some cases. They will allow you to think in terms of observation of objects so that you don't disturb the ownership of a resource and they just generally make your code cleaner, easier to maintain and read.

### How can I start using Smart Pointers?

[Get Boost!](http://www.boost.org/users/download/) That's going to be the best way. Dive right in, take a look at samples, set things up, blow them up - be a scientist about it! Anyway, enough of this! On to the pointers.

### [scoped_ptr](http://www.boost.org/doc/libs/1_52_0/libs/smart_ptr/scoped_ptr.htm) & [scoped_array](http://www.boost.org/doc/libs/1_52_0/libs/smart_ptr/scoped_array.htm)

`scoped_ptr` is all about ensuring that the pointer that you're working with is the ultimate owner of the resource that it points to. There's no facility within this pointer type to transfer the ownership of the inner resource elsewhere. With all of this in mind, `scoped_ptr` ensures that the resource that is under ownership will be destroyed properly once the pointer has dropped out of scope. `scoped_ptr` is a very lightweight resource. It's by no means going to harm the performance or size of your application. `scoped_array` will perform the same service as `scoped_ptr` does, it's just that `scoped_array` will work on array types (as the name suggests).

### [shared_ptr](http://www.boost.org/doc/libs/1_52_0/libs/smart_ptr/shared_ptr.htm) & [shared_array](http://www.boost.org/doc/libs/1_52_0/libs/smart_ptr/shared_array.htm)

`shared_ptr` is all about reference counting. They will internally manage the reference count that they have and govern the managed resource's lifespan based on this. The clear advantage that they have over the `scoped_ptr` and `scoped_array` counterparts is their ability to be shared between multiple owner so that those owners can maintain their interest in the object by "hanging around". The true power of this class of pointer is when you don't know when to delete the underlying resource. As long as someone is referencing you, you'll stay alive. `shared_array` will perform the same service as `shared_ptr` does, it's just that `shared_array` will work on array types (deja vu anyone?)

### [intrusive_ptr](http://www.boost.org/doc/libs/1_52_0/libs/smart_ptr/intrusive_ptr.html)

The `intrusive_ptr` is another reference counting pointer only is allows you to provide your own mechanism for performing the reference counting. This means if you have an existing codebase that does all of this work for you, all you need to do is provide it to an `intrusive_ptr`. `intrusive_ptr` also allows for native usage of the `this` keyword.

### [weak_ptr](http://www.boost.org/doc/libs/1_52_0/libs/smart_ptr/weak_ptr.htm)

A `weak_ptr` just performs observation on a `shared_ptr` without getting its hands into ownership. It's used purely at an observation capacity.

### Conclusion

That's it for a brief smart pointer analysis. Hopefully these tid-bits will help you decide which pointer fits your problem best.