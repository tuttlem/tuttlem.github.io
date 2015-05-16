---
layout: post
title: Eventing model in Node.js
date: 2015-05-16
comments: false
categories: [ "node", "node.js", "EventEmitter" ]
---

An easy way to create an extensible API in [Node.js](https://nodejs.org/) is to use the [EventEmitter](https://nodejs.org/api/events.html) class. It allows you to publish interesting injection points into your module so that client applications and libraries can respond when these events are emitted.

### Simple example

In the following example, I'll create a `Dog` class that exposes an event called `bark`. When this class <em>internally</em> decides that it's time to bark, it will emit this event for us.

First of all, we define our class which includes a way to start the dog barking.

{% highlight js %}
var util = require('util');
var EventEmitter = require('events').EventEmitter;

var Dog = function (name) {
    var self = this;

    self.name = name;

    self.barkRandomly = function () {
        // WOOF WOOF!
        var delay = parseInt(Math.random() * 1000);

        setTimeout(function () {
            self.emit('bark', self);
            self.barkRandomly();
        }, delay);

    };

    self.on('bark', function (dog) {
        console.log(dog.name + ' is barking!');
    });
};

util.inherits(Dog, EventEmitter);
{% endhighlight %}

The `barkRandomly` function will take a random interval of time and then emit the `bark` event for us. It's an example for demonstration purposes so that you can see how you'd emit and event at the back end of a callback.

Note that the `emit` call allows us to specify some information about the event. In this example, we'll just send the dog (or `self`) that's currently barking.

Using the `on` function at the end, we're also able to get the class itself to subscribe to its own `bark` event. The `emit` and `on` functions are available to us internally because we've used the `inherits` function from the `util` module to extend the `Dog` class with the attributes of `EventEmitter`.

All that's left now is to create a dog and get it to bark.

{% highlight js %}
var rover = new Dog('Rover');

rover.on('bark', function (dog) {
    console.log('I just heard ' + dog.name + ' barking');
});

rover.barkRandomly();
{% endhighlight %}

Running this code, you'll end up with a stream of barking notifications scrolling down your page.

### Subscription management

Just as you can subscribe to an emitted event, you can remove a handler from the event when you are no longer interested in updates from it. To continue from the example above; if we had a handler that only cared if the dog barked for the first 3 times we could manage the subscription like so:

{% highlight js %}
var rover = new Dog('Rover');

var notificationCount = 0;

var handler = function (dog) {
    console.log('I just heard ' + dog.name + ' barking');
    
    notificationCount ++;

    if (notificationCount == 3) {
        rover.removeListener('bark', handler);
    }
};

rover.on('bark', handler);
{% endhighlight %}

The operative line here being the call to `removeListener`.

You can simulate an irritable neighbor who would call the cops as soon as he heard your dog bark with a call to `once` which will fire once only, the first time it gets a notification:

{% highlight js %}
rover.once('bark', function (dog) {
    console.log('I\'VE HAD IT WITH THAT DOG, ' + dog.name + '! I\'M CALLING THE COPS!');
});
{% endhighlight %}

Finally, all subscribers can be removed from any given event with a call to `removeAllListeners`.

