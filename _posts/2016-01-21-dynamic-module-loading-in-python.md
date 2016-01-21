---
layout: post
title: Dynamic module loading in Python
date: 2016-01-21
comments: false
categories: [ "import", "python", "module", "dynamic" ]
---

The [module system](https://docs.python.org/2/tutorial/modules.html) in fairly rich, where the `import` keyword is really only the tip of the iceberg. In today's post, I'm going to go through a very simple dynamic module loading unit.

A module loader allows you to invoke pieces of code dynamically at run-time. Configurations of what gets executed when can be defined in a database, statically, where ever. What's important is that we'll reference our modules by name (using a string), have them loaded dynamically and then executed.

As a quick aside, my use of the term *module* throughout this article could refer to a python module; it could also refer to a small dynamic piece of code I'm calling a *module*.

### Common module format

To get started, we really need to settle on a common format or structure that our modules will take. It'll be this assumption that our host object will use to uniformly invoke these pieces of code. Immediately, this format has two major problems that need to be solved for this system to work. It needs to:

* Provide a construction interface
* Provide runnability

There are plenty of other things that we could add in:

* Event for when the module is loaded and torn down
* Top-level error handler for when exceptions bubble out of the module
* Common logging framework

For the purposes of this article, we'll focus on loading and executing the module.

### What is a module?

For our implementation, we're going to say that a module is a `class`. Making this decision to create a module as a class allows us to refer to our module as a definition (the actual class itself) and defer the instancing of our objects to the class's construction invocation.

A very simple module might look like this:

{% highlight python %}
class Module:

    def __init__(self, state):
        self.state = state
        
    def run(self):
        print "This is module1 doing its thing"
{% endhighlight %}

You can see that our constructor is taking in a parameter called `state`. There's no real significance here aside from giving the module system the ability to send arbitrary state information during the instantiation process. The `run` function is what our host will be executing to perform the module's work.

### Factory construction

The [factory pattern](https://en.wikipedia.org/wiki/Factory_method_pattern) allows a developer to encapsulate the construction of related types inside of a function (or factory implementation class), and have this construction derived at run-time or configured elsewhere (think inversion-of-control). We're going to borrow very shallowly from this concept.

Each module that participates on our framework **must** export a `create` function. This function can take parameters; so long as all of your factory constructors define the same interface it doesn't matter. For today's example, my `create` function takes an arbitrary object called `state` which just allows the implementing developer to send information to the constructed module:

{% highlight python %}
def create(state):
    return Module(state)
{% endhighlight %}

### The host

The host's job is to:

* Load a module (a python file) off disk
* `import` it
* Call the module factory `create`
* Call the `run` method of the module

There's a lot more fancy stuff that we could do of course. Going back to the start of this article, there are lots of different services that the host should be able to offer each module that gets loaded to provide the overall system a richer experience without re-writing common services. Logging, error handling, network and data connections could be simplified in the framework to provide a quick avenue to modules to be productive!

Our *very simple* host, would look something like this:

{% highlight python %}
def run_module(module_name):
    name = "modules." + module_name
    mod = __import__(name, fromlist=[''])
    obj = mod.create({})
    obj.run()

run_module('mod1')
{% endhighlight %}

The heart of the host really is the `run_module` function and it leans heavily on the `__import__` call to get its job done. The `state` parameter for the module is wasted in this context, but you can see how it'd be relatively easy to manage context aware state per process that you're running.

The `run` method runs our module code.

### Conclusion

This is just a simple, dynamic module loader. It can be applied in a lot of highly complex scenarios but the basic principles of keeping modules small, concise should help you not get ahead of yourself.



