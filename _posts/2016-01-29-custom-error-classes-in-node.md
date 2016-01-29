---
layout: post
title: Custom error classes in node
date: 2016-01-29
comments: false
categories: [ "node", "js", "javascript", "error" ]
---

The [Error class](https://nodejs.org/api/errors.html#errors_class_error) in Node.js provides the programmer with a reference point of failure when problems occur. To take this idea further, we can sub-class this class and specialize information within these errors to provide a richer execution tree in times of failure.

From the [Node.js](https://nodejs.org/) documentation:

> A generic JavaScript `Error` object that does not denote any specific circumstance of why the error occurred. `Error` objects capture a "stack trace" detailing the point in the code at which the `Error` was instantiated, and may provide a text description of the error. 

In today's post, I'll walk through a deriving from the [Error class](https://nodejs.org/api/errors.html#errors_class_error) and how you can use it in your client code.

### Definition

We'll be using the `inherits` function from the `util` module to accomplish sub-classification; as per usual. Our `FooError` class looks like this:

{% highlight javascript %}
'use strict';

var util = require('util');

var FooError = function (message, extra) {
  Error.captureStackTrace(this, this.constructor);
  this.name = 'FooError';
  this.message = message;
  this.extra = extra;
};

util.inherits(FooError, Error);
{% endhighlight %}

For good measure, we'll also define a `BarError`:

{% highlight javascript %}
var BarError = function (message, extra) {
  Error.captureStackTrace(this, this.constructor);
  this.name = 'BarError';
  this.message = message;
  this.extra = extra;
};

util.inherits(BarError, Error);
{% endhighlight %}

That's it as far as the definition is concerned. `FooError` and `BarError` are ready for us to use.

### Usage

A bonehead example follows, but it'll at least give you an example of what the logic tree looks like to investigate exactly what type of error just occurred.

{% highlight javascript %}
var errors = [
  new FooError('Foo happens', null),
  new BarError('Bar happens', null),
  new Error('Unspecified stuff happens')
];

errors.forEach(function (err) {

  try {
    throw err;
  } catch (e) {
    console.log(e.toString());
  }

});
{% endhighlight %}

We build an array of errors, enumerate the array; throw each error. In our `catch` block, I'm simply `console.log` the information out. We end up with the following:

{% highlight text %}
FooError: Foo happens
BarError: Bar happens
Error: Unspecified stuff happens
{% endhighlight %}

Just by simply testing the `name` property on these error objects, we can be a little more sophisticated in the way we make decisions on what to do:

{% highlight javascript %}
errors.forEach(function (err) {

  try {
    throw err;
  } catch (e) {

    if (e.name == 'FooError') {
      console.log('--- FOO ---');
    } else if (e.name == 'BarError') {
      console.log('--- BAR ---');
    } else if (e.name == 'Error') {
      console.log('Unspecified error')
    }

  }

});
{% endhighlight %}

This change results in the following being sent to the console:

{% highlight text %}
--- FOO ---
--- BAR ---
Unspecified error
{% endhighlight %}

