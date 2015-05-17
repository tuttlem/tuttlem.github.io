---
layout: post
title: Working Asynchronously with async in Node.js
date: 2015-05-16
comments: false
categories: [ "node", "node.js", "async" ]
---

As mentioned [previously]({ post_url 2015-05-16-multi-core-peocessing-with-cluster }), Node.js runs in a single thread. In order to get it running with concurrency you need to use a library. In today's post, I'm going to go through the [async](https://www.npmjs.com/package/async) library really briefly.

<strong>It's important to note</strong> that you need to follow conventions in order for this library to be successful for you.

> All these functions assume you follow the Node.js convention of providing a single callback as the last argument of your async function.

### List processing

A great deal of asynchronous work that you'll do will be conducted on collections/lists. The `async` module provides the usual processing facilities for these data types, and they're all simple to use. Here we'll filter out non-prime numbers:

{% highlight js %}
var async = require('async'),
    range = require('node-range');

var candidates = range(3, 1000).toArray();

var isPrime = function (v, callback) {

  if ((v % 2) == 0) {
    callback(false);
    return;
  }

  var m = 3;

  while (m < v) {
    if ((v % m) == 0) { 
      callback(false);
      return;
    }

    m += 2;
  }

  callback(true);
};


async.filter(candidates, isPrime, function (res) {
  console.log(res);
});
{% endhighlight %}

<strong>Note</strong> that `isPrime` uses a callback to send its result back. This allows all items in the array, `candidates` to participate nicely in the async operation.

### Work sequencing

There are a few different work strategies you can employ with the async module.

`series` will execute items one-after-the-other; `parallel` execute items at the same time <em>(or, in parallel)</em>; `waterfall` operates like `series` however it'll automatically supply the return of the previous call as input to the next.

Everything you expect is in this library.