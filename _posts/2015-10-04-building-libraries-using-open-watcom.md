---
layout: post
title: Building libraries using Open Watcom
date: 2015-10-04
comments: false
categories: [ "dos", "watcom", "32" ]
---

Being able to bundle blocks of your code (and data to some extent) into library files is quite a productive step forward when developing applications. Being able to port these pieces around means a higher level of code-reuse, and a less number of times you'll spend re-writing the same stuff.

In today's post, I'll take you through creating a very minimal library. We'll create a library module from this code and I'll also show you how to consume it.

### Howdy!

Our example library will expose one function, called `greet`. `greet` will take in a person's name and will print a greeting to the console. Here's the header:

{% highlight c %}
/* greeter.h */

#ifndef __greeter_h_
#define __greeter_h_

#include <stdio.h>

void greet(const char *name);

#endif 
{% endhighlight %}

The implementation is basic. It doesn't even really matter, but is included for completeness:

{% highlight c %}
/* greeter.c */

#include "greeter.h"

void greet(const char *name) {
  printf("Greetings, %s!", name);
}
{% endhighlight %}

### Make me a library

Making a library is all about compiling your code to produce object files and then bundling your object files into a library file. So, the first step is to compile `greeting.c` into an object file:

{% highlight text %}
C:\SRC> wcc386 greeter.c
{% endhighlight %}

After this, you'll now have `GREETER.OBJ` in your project folder. You can turn this into a library with the following:

{% highlight text %}
C:\SRC> wlib greeter +greeter
{% endhighlight %}

The command itself says invoke `wlib` to create (or modify) a library called `greeter` (the `.lib` extension is handled for us). Finally the `+greeter` says that we want to add `greeter.obj` into the library. We'll now have a `.LIB` file that we can link against.

### Consuming the library

Writing code that actually uses the library is as easy as including the header and calling functions. Here's a test:

{% highlight c %}
/* test.c */

#include "greeter.h"

int main(int argc, char *argv[]) {
  greet("Joe");
  return 0;
}
{% endhighlight %}

Converting this into a callable executable is achieved with `wcl386'.

{% highlight text %}
C:\SRC> wcl386 test.c greeter.lib
{% endhighlight %}

That's all there is to it.