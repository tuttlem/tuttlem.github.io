---
layout: post
title: Working with Promises using Q in Node.js
date: 2015-05-16
comments: false
categories: [ "node", "node.js", "promise", "q" ]
---

A [promise](http://en.wikipedia.org/wiki/Futures_and_promises) is an object that represents the result of a computation; whether it be a positive or negative result. What's special about promises in concurrent programming is that they allow you to compose your code in such a way that is a little more natural than the callbacks-in-callbacks style. 

In today's post, I'm going to work with the [Q](https://www.npmjs.com/package/q) library for Node.js to demonstrate how we can use promises to clean up our code into more concise blocks of logic.

From the [npm page](https://www.npmjs.com/package/q) for the Q library, it even says:

> On the first pass, promises can mitigate the “[Pyramid of Doom](http://calculist.org/blog/2011/12/14/why-coroutines-wont-work-on-the-web/)”: the situation where code marches to the right faster than it marches forward.

### Callbacks to Promises

In the following example, I'm going to simulate some work using `setTimeout`. This will also give us some `asynchronous` context. Here are the two function calls we'll look to sequence:

{% highlight js %}
var getUserByName = function (name, callback) {
  setTimeout(function () {

    try {
      callback(null, {
        id: 1,
        name: name
      });            
    } catch (e) {
      callback(e, null);
    }

  }, 1000);
};

var getCarsByUser = function (userId, callback) {
  setTimeout(function () {

    try {
      callback(null, ['Toyota', 'Mitsubishi', 'Mazda']);
    } catch (e) {
      callback(e, null);
    }

  }, 1000);
};
{% endhighlight %}

Even though the inputs and outputs of these functions are invalid, I just wanted to show that `getCarsByUser` is dependent on the output of `getUserByName`.

As any good-citizen in the node eco-system the last parameter of both of these functions is a callback function that take the signature of (`err`, `data`). Sequencing this code normally would look as follows:

{% highlight js %}
getUserByName('joe', function (err, user) {
  getCarsByUser(user.id, function (err, cars) {
    // do something here
  });
});
{% endhighlight %}

The code starts to move to the right as you get deeper and deeper into the callback tree.

We can convert this into promises with the following code:

{% highlight js %}
var pGetUserByName = Q.denodeify(getUserByName),
    pGetCarsByUser = Q.denodeify(getCarsByUser);

pGetUserByName('joe').then(pGetCarsByUser)
                     .done();
{% endhighlight %}

Because we've structured our callbacks "correctly", we can use the `denodeify` function to directly convert our functions into promises. We can then sequence our work together using `then`. If we wanted to continue to build this promise, we could omit the `done` call for something else to complete work on.

### Going pear-shaped

When error handling gets involved in the callback scenario, the if-trees start to muddy-up the functions a little more:

{% highlight js %}
getUserByName('joe', function (err, user) {
  if (err != null) {
    console.error(err);
  } else {
    getCarsByUser(user.id, function (err, cars) {
      if (err != null) {
        console.error(err);
      } else {
        // work with the data here
      }
    });
  }
});
{% endhighlight %}

In the promise version, we can use the `fail` function to perform our error handling for us like so:

{% highlight js %}
pGetUserByName('joe').then(pGetCarsByUser)
                     .fail(console.error)
                     .done();
{% endhighlight %}

Makes for a very concise set of instructions to work on.

### Different ways to integrate

There are a couple of ways to get promises integrated into your existing code base. Of course, it's always best to implement these things at the start so that you have this model of programming in the front of your mind; as opposed to an after thought.

From <strong>synchronous</strong> code, you can just use the `fcall` function to start off a promise:

{% highlight js %}
var getName = Q.fcall(function () {
  return 'John';
});
{% endhighlight %}

In this case, you just supply any parameters that are expected also:

{% highlight js %}
var getGenderName = function (gender) {
  if (gender == 'F') {
    return 'Mary';
  }

  return 'John';
}

var getName = Q.fcall(getGenderName, 'F');
{% endhighlight %}

In <strong>asynchronous</strong> cases, you can use `defer`. This will require you to restructure your original code though to include its use.

{% highlight js %}
var getGenderName = function (gender) {
  var deferred = Q.defer();
  var done = false;
  var v = 0;

  var prog = function () {
    setTimeout(function () {
      if (!done) {
        v ++;
        deferred.notify(v);
        prog();
      }
    }, 1000);

  };

  prog();

  setTimeout(function () {

    if (gender == 'F') {
      deferred.resolve('Mary');
    } else if (gender == 'M') {
      deferred.resolve('John');  
    } else {
      deferred.reject(new Error('Invalid gender code'));
    }

    done = true;

  }, 5000);

  return deferred.promise;
};
{% endhighlight %}

We're able to send progress updates using this method as well. You can see that with the use of the `notify` function. Here's the call for this function now:

{% highlight js %}
getGenderName('F')
.then(function (name) {
  console.log('Gender name was: ' + name);
})
.progress(function (p) {
  console.log('Progress: ' + p);
})
.fail(function (err) {
  console.error(err);
})
.done();
{% endhighlight %}

`resolve` is our successful case, `reject` is our error case and `notify` is the progress updater.

This function can be restructured a little further with the use of `promise` though:

{% highlight js %}
var getGenderName = function (gender) {
  return Q.promise(function (resolve, reject, notify) {

    var done = false;
    var v = 0;

    var prog = function () {
      setTimeout(function () {
        if (!done) {
          v ++;
          notify(v);
          prog();
        }
      }, 1000);

    };

    prog();

    setTimeout(function () {

      if (gender == 'F') {
        resolve('Mary');
      } else if (gender == 'M') {
        resolve('John');  
      } else {
        reject(new Error('Invalid gender code'));
      }

      done = true;

    }, 5000);

  });
};
{% endhighlight %}

Our client code doesn't change.

Finally, `nfcall` and `nfapply` can be used to ease the integration of promises in your code. These functions are setup deliberately to deal with the Node.js callback style.