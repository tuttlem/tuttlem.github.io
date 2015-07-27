---
layout: post
title: node.js module patterns
date: 2015-07-27
comments: false
categories: [ "node", "js", "module" ]
---

In today's post, I'll walk through some of the more common node.js module patterns that you can use when writing modules.

### Exporting a function

Exporting a function from your module is a very procedural way to go about things. This allows you to treat your loaded module as a function itself. 

You would define your function in your module like so:

{% highlight js %}
module.exports = function (name) {
  console.log('Hello, ' + name);
};
{% endhighlight %}

You can then use your module as if it were a function:

{% highlight js %}
var greeter = require('./greeter');
greeter('John');
{% endhighlight %}

### Exporting an object

Next up, you can pre-assemble an object and export it as the module itself. 

{% highlight js %}
var Greeter = function () { };

Greeter.prototype.greet = function (name) {
  console.log('Hello, ' + name);
}

module.exports = new Greeter();
{% endhighlight %}

You can now start to interact with your module as if it were an object:

{% highlight js %}
var greeter = require('./greeter');
greeter.greet('John');
{% endhighlight %}

### Exporting a prototype

Finally, you can export an object definition (or prototype) as the module itself. 

{% highlight js %}
var Greeter = function () { };

Greeter.prototype.greet = function (name) {
  console.log('Hello, ' + name);
}

module.exports = Greeter;
{% endhighlight %}

You can now create instances from this module:

{% highlight js %}
var Greeter = require('./greeter');
var greeter = new Greeter();
greeter.greet('John');
{% endhighlight %}
