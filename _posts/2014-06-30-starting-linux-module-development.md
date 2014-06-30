---
layout: post
title: Starting Linux module development
date: 2014-06-30
comments: false
categories: ["Linux", "Kernel", "Module", "programming"]
---

### Introduction

Developing modules for the [Linux Kernel](http://kernel.org/) has previously been a difficult discipline to even get started in, but as time has passed it's become a more approachable and accessible topic. In today's post, I'm going to go through

* How to setup your build environment
* Writing the code for a simple module
* Building your module
* Adding and removing your module from the Kernel

There are a lot of different sources on the internet that have this information and the best reference that I can suggest is on the [LDP](www.tldp.org) which is [here](www.tldp.org/LDP/lkmpg/2.6/html/lkmpg.html#AEN121).

We've got a bit to cover, so let's get started. One last thing though, I'm running [Arch Linux](https://www.archlinux.org) to write this tutorial however everything that I'm writing about here should be directly translatable into your distribution of choice.

### Setting up your build environment

You need to put your system into a state where you're able to build kernel modules, and you'll do this with the `linux-headers` package from your distribution's package manager.

{% highlight bash %}
sudo pacman -S linux-headers
{% endhighlight %}

Once this has installed, you'll find a build environment has been made under `/lib/modules/` on your system. You'll also have all of the development files required to include in your modules.

### The code

First up, just a couple of requirements. We're going to print a kernel message using `printk` when the module is initialised and we'll print another when the module has been unloaded. Pretty unimaginative but it'll be great for the sake of demonstration.

Here's the code:

{% highlight c %}
#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/init.h>

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Michael Tuttle");
MODULE_DESCRIPTION("A useless module for demonstration purposes");

/** Initialises our module into the kernel */
static int __init message_init(void) {
   printk(KERN_INFO "Module has been loaded");
   return 0;
}

/** Unloads our module from the kernel */
static void __exit message_exit(void) {
   printk(KERN_INFO "Module has been unloaded");
}

module_init(message_init);
module_exit(message_exit);
{% endhighlight %}

That's it for the code. It's pretty easy to follow. You can see that `message_init` is what will be called when our module is loaded and `message_exit` when unloaded. Traditionally these were called `init_module` and `cleanup_module` respectively but these names are allowed to change due to the use of the `__init`, `module_init`, `__exit` and `module_exit` macros.

`printk` is what we'll use to send some text into the kernel messages. You retrieve these with the `dmesg` shell command.

### Building your module

The Makefile for this module is actually quite simple, but requires a little explanation.

{% highlight basemake %}
obj-m += msg.o

all:
   make -C /lib/modules/$(shell uname -r)/build M=$(PWD) modules

clean:
   make -C /lib/modules/$(shell uname -r)/build M=$(PWD) clean
{% endhighlight %}

Quite simple in the end, but cryptic if you haven't come across some of it before. The `obj-m` directive tells the make system that we want a kernel <em>module</em>. `obj-d` can be used here when we're making a <em>driver</em>.

The make targets are executed against the build environment we installed above. `-C` tells make to issue its instructions in the folder given.

After compiling your module, you're ready to see it in action.

### Adding and Removing your module

To get your kernel to start using your module, you issue `insmod` against the `.ko` file that has been built for you.

{% highlight bash %}
$ sudo insmod msg.ko
{% endhighlight %}

The kernel now should have loaded your module and you should be able to confirm that it's loaded by checking `dmesg`:

{% highlight text %}
$ dmesg

. . .
. . .
. . .
[ 3320.686668] Module has been loaded
{% endhighlight %}

Of course there's an easier way to check that it's loaded. You can see a list of all the loaded modules in your kernel by issuing `lsmod`.

{% highlight bash %}
$ lsmod
Module                  Size  Used by
msg                      825  0
. . .
. . .
. . .
{% endhighlight %}

The first half has gone to plan. To unload this module, we now use `rmmod`.

{% highlight bash %}
$ sudo rmmod msg.ko
{% endhighlight %}

Now that the module has been removed, we should see the leaving message in the output of `dmesg`.

{% highlight text %}
$ dmesg

. . .
. . .
. . .
[ 3641.668948] Module has been unloaded
{% endhighlight %}


