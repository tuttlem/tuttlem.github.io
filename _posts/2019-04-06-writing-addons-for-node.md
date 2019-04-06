---
layout: post
title: Writing addons for node
date: 2019-04-06
comments: false
categories: [ "node", "addon", "c++" ]
---

### Introduction

Sometimes you might find yourself in the situation where you require a little more power out of your [node.js](https://nodejs.org/en/) application. You may need to [squeeze some extra performance](https://www.future-processing.pl/blog/javascript-is-slow/) out of a piece of code that you simply can't achieve using [javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript) alone. Node.js provides a very [rich sdk](https://github.com/nodejs/node-addon-api) to allow application developers to create their own addons to use, that allow you to write in [C++](http://www.cplusplus.com/doc/tutorial/).

These [binary compiled modules](https://github.com/nodejs/node-gyp) then become directly accessible from your node.js applications.

In today's article, I'd like to walk through the basic setup of an addon project. We'll also add a function to the addon, and demonstrate the call from javascript to C++.

### Setup

Before you can get developing, you'll need to make sure you have some dependencies installed. Create a directory, and start a new node application.

{% highlight bash %}
mkdir my-addon
cd my-addon

npm init
{% endhighlight %}

You'll need to let the package manager know that your application has a `gyp` file present by switching `gypfile` to `true`. 

{% highlight javascript %}
// package.json

{
  "name": "my-addon",
  "version": "1.0.0",
  "description": "",
  "main": "index.js",
  "gypfile": true,
  "scripts": {
    "build": "node-gyp rebuild",
    "clean": "node-gyp clean"
  },
  "author": "",
  "license": "ISC",
  "devDependencies": {
    "node-gyp": "^3.8.0"
  },
  "dependencies": {
    "node-addon-api": "^1.6.3"
  }
}
{% endhighlight %}

The project is going to require a [gyp](https://gyp.gsrc.io/index.md) file called `binding.gyp`. It's the responsibility of this file to generate the build environment that will compile our addon.

{% highlight javascript %}
// binding.gyp

{
  "targets": [{
    "target_name": "myaddon",
    "cflags!": ["-fno-exceptions"],
    "cflags-cc!": ["-fno-exceptions"],
    "sources": [
      "src/main.cpp"
    ],
    "include_dirs": [
      "<!@(node -p \"require('node-addon-api').include\")"
    ],
    "libraries": [],
    "dependencies": [
      "<!(node -p \"require('node-addon-api').gyp\")"
    ],
    "defines": [ "NAPI_DISABLE_CPP_EXCEPTIONS" ]
  }]
}
{% endhighlight %}

With these in place, you can install your dependencies.

{% highlight bash %}
npm install
{% endhighlight %}

### Your first module

The gyp file notes that the source of our addon sits at `src/main.cpp`. Create this file now, and we can fill it out with the following.

{% highlight cpp %}
// src/hello.cpp 

#include <napi.h>

Napi::Object InitAll(Napi::Env env, Napi::Object exports) {
  return exports;
}

NODE_API_MODULE(myaddon, InitAll)
{% endhighlight %}

The keen reader would see that our module does nothing. That's ok to start with. This will be an exercise in checking that the build environment is setup correctly.

Import and *use* your addon just like you would any other module from within the node environment.

{% highlight javascript %}
// index.js

const myAddon = require("./build/Release/myaddon.node");
module.exports = myAddon;
{% endhighlight %}

### Build and run

We're ready to run.

{% highlight bash %}
npm run build
node index.js
{% endhighlight %}

Ok, great. As expected, that did nothing.

### Make it do something

Let's create a function that will return a string. We can then take that string, and print it out to the console once we're in the node environment. 

We'll add a header file that will define any functions. We also need to tell our build environment that we've got another file to compile.

{% highlight javascript %}
// binding.gyp

{
  "targets": [{
    "target_name": "myaddon",
    "cflags!": ["-fno-exceptions"],
    "cflags-cc!": ["-fno-exceptions"],
    "sources": [
      "src/funcs.h",
      "src/main.cpp"
    ],
    "include_dirs": [
      "<!@(node -p \"require('node-addon-api').include\")"
    ],
    "libraries": [],
    "dependencies": [
      "<!(node -p \"require('node-addon-api').gyp\")"
    ],
    "defines": [ "NAPI_DISABLE_CPP_EXCEPTIONS" ]
  }]
{% endhighlight %}

We define the functions for the addon.

{% highlight cpp %}
// src/funcs.h

#include <napi.h>

namespace myaddon {
  Napi::String getGreeting(const Napi::CallbackInfo &info);
}
{% endhighlight %}

Now for the definition of the function, as well as its registration into the module.

{% highlight cpp %}
#include "funcs.h"

Napi::String myaddon::getGreeting(const Napi::CallbackInfo &info) {
  Napi::Env env = info.Env();
  return Napi::String::New(env, "Good morning!");
}

Napi::Object InitAll(Napi::Env env, Napi::Object exports) {
  exports.Set("getGreeting", Napi::Function::New(env, myaddon::getGreeting));
  return exports;
}

NODE_API_MODULE(myaddon, InitAll)
{% endhighlight %}

The `getGreeting` function is actually doing *the work* here. It's simply returning a greeting. The `InitAll` function now changes to add a `Set` call on the `exports` object. This is just registering the function to be available to us.

### Greetings

So, now we can actually use the greeting. We can just `console.log` it out.

{% highlight javascript %}
const myAddon = require("./build/Release/myaddon.node");

console.log(myAddon.getGreeting());

module.exports = myAddon;
{% endhighlight %}

We can now run our code.

{% highlight text %}
➜  my-addon node index.js
Good morning!
{% endhighlight %}

