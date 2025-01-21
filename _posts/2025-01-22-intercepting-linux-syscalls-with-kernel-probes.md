---
layout: post
title: Intercepting Linux Syscalls with Kernel Probes
date: 2025-01-22
comments: false
categories: [ "linux", "kernel", "kprobe" ]
---

# Introduction

n this tutorial, we will explore how to write a Linux kernel module that intercepts system calls using kernel probes 
([kprobes](https://docs.kernel.org/trace/kprobes.html)). 

Instead of modifying the syscall table—a risky and outdated approach—we will use [kprobes](https://docs.kernel.org/trace/kprobes.html), an officially supported and 
safer method to trace and modify kernel behavior dynamically.

## What Are System Calls?

System calls are the primary mechanism by which user-space applications interact with the operating system’s kernel. 
They provide a controlled gateway to hardware and kernel services. For example, opening a file uses the open syscall, 
while reading data from it uses the read syscall.

## What Are Kernel Probes?

[Kprobes](https://docs.kernel.org/trace/kprobes.html) are a powerful debugging and tracing mechanism in the Linux kernel. They allow developers to dynamically 
intercept and inject logic into almost any kernel function, including system calls. Kprobes work by placing breakpoints 
at specific addresses in kernel code, redirecting execution to custom handlers.

Using [kprobes](https://docs.kernel.org/trace/kprobes.html), you can intercept system calls like close to log parameters, modify behavior, or gather debugging 
information, all without modifying the syscall table or kernel memory structures.

# The Code

We have some preparation steps in order to be able to do Linux Kernel module development. If your system is already 
setup to do this, you can skip the first section here.

Before we start, remember to do this in a **safe environment**. Use a virtual machine or a disposable system for 
development. Debugging kernel modules can lead to crashes or instability.

## Prerequisites

First up, we need to install the prerequisite software in order to write and build modules:

{% highlight bash %}
sudo apt-get install build-essential linux-headers-$(uname -r)
{% endhighlight %}

## Module code

Now we can write some code that will actually be our kernel module.

{% highlight c %}
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/kprobes.h>

MODULE_LICENSE("GPL");

static struct kprobe kp = {
    .symbol_name = "__x64_sys_close",
};

static int handler_pre(struct kprobe *p, struct pt_regs *regs) {
    printk(KERN_INFO "Intercepted close syscall: fd=%ld\n", regs->di);
    return 0;
}

static int __init kprobe_init(void) {
    int ret;

    kp.pre_handler = handler_pre;
    ret = register_kprobe(&kp);
    if (ret < 0) {
        printk(KERN_ERR "register_kprobe failed, returned %d\n", ret);
        return ret;
    }

    printk(KERN_INFO "Kprobe registered\n");
    return 0;
}

static void __exit kprobe_exit(void) {
    unregister_kprobe(&kp);
    printk(KERN_INFO "Kprobe unregistered\n");
}

module_init(kprobe_init);
module_exit(kprobe_exit);
{% endhighlight %}

## Breakdown

First up, we have our necessary headers for kernel development and the module license:

{% highlight c %}
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/kprobes.h>

MODULE_LICENSE("GPL");
{% endhighlight %}

This ensures compatibility with GPL-only kernel symbols and enables proper loading of the module.

Next, the kprobe structure defines the function to be intercepted by specifying its symbol name. Here, we target 
`__x64_sys_close`:

{% highlight c %}
static struct kprobe kp = {
    .symbol_name = "__x64_sys_close",
};
{% endhighlight %}

This tells the kernel which function to monitor dynamically.

The `handler_pre` function is executed before the intercepted function runs. It logs the file descriptor (`fd`) argument 
passed to the `close` syscall:

{% highlight c %}
static int handler_pre(struct kprobe *p, struct pt_regs *regs) {
    printk(KERN_INFO "Intercepted close syscall: fd=%ld\n", regs->di);
    return 0;
}
{% endhighlight %}

In this case, `regs->di` contains the first argument to the syscall (the file descriptor).

The kprobe_init function initialises the kprobe, registers the handler, and logs its status. If registration fails, an 
error message is printed:

{% highlight c %}
static int __init kprobe_init(void) {
    int ret;

    kp.pre_handler = handler_pre;
    ret = register_kprobe(&kp);
    if (ret < 0) {
        printk(KERN_ERR "register_kprobe failed, returned %d\n", ret);
        return ret;
    }

    printk(KERN_INFO "Kprobe registered\n");
    return 0;
}
{% endhighlight %}

The `kprobe_exit` function unregisters the kprobe to ensure no stale probes are left in the kernel:

{% highlight c %}
static void __exit kprobe_exit(void) {
    unregister_kprobe(&kp);
    printk(KERN_INFO "Kprobe unregistered\n");
}
{% endhighlight %}

Finally, just like usual we define the entry and exit points for our module:

{% highlight c %}
module_init(kprobe_init);
module_exit(kprobe_exit);
{% endhighlight %}

## Building

Now that we've got our module code, we can can build and install our module. The following `Makefile` will allow us to 
build our code:

{% highlight text %}
obj-m += syscall_interceptor.o

all:
        make -C /lib/modules/$(shell uname -r)/build M=$(PWD) modules

clean:
        make -C /lib/modules/$(shell uname -r)/build M=$(PWD) clean
{% endhighlight %}

We build the module:

{% highlight shell %}
make
{% endhighlight %}

After a successful build, you should be left with a `ko` file. In my case it's called `syscall_interceptor.ko`. This is 
the module that we'll install into the kernel with the following:

{% highlight shell %}
sudo insmod syscall_interceptor.ko
{% endhighlight %}

## Verify

Let's check `dmesg` to verify it's working. As we've hooked the `close` call we should end up with a flood of messages 
to verify:

{% highlight shell %}
dmesg | tail
{% endhighlight %}

You should see something like this:

{% highlight text %}
[  266.615596] Intercepted close syscall: fd=-60473131794600
[  266.615596] Intercepted close syscall: fd=-60473131794600
[  266.615597] Intercepted close syscall: fd=-60473131794600
[  266.615600] Intercepted close syscall: fd=-60473131794600
[  266.615731] Intercepted close syscall: fd=-60473131925672
{% endhighlight %}

You can unload this module with `rmmod`:

{% highlight shell %}
sudo rmmod syscall_interceptor
{% endhighlight %}

# Understand Kprobe Handlers

Kprobe handlers allow you to execute custom logic at various stages of the probed function’s execution:
* **Pre-handler**: Runs before the probed instruction.
* **Post-handler**: Runs after the probed instruction (not used in this example).
* **Fault handler**: Runs if an exception occurs during the probe.

Modify the module to add post- or fault-handling logic as needed.

# Clean Up

Always unregister kprobes in the module's exit function to prevent leaving stale probes in the kernel. Use dmesg to 
debug any issues during module loading or unloading.

# Caveats and Considerations
1. **System Stability**: Ensure your handlers execute quickly and avoid blocking operations to prevent affecting system performance.
2. **Kernel Versions**: Kprobes are supported in modern kernels, but some symbols may vary between versions.
3. **Ethical Usage**: Always ensure you have permission to test and use such modules.

# Conclusion

Using kprobes, you can safely and dynamically intercept system calls without modifying critical kernel structures. This 
tutorial demonstrates a clean and modern approach to syscall interception, avoiding deprecated or risky techniques like 
syscall table modification.