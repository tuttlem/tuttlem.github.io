---
title: "Hooking open() with LD_PRELOAD"
date: 2025-08-31
layout: post
categories: [linux, debugging, syscalls]
---

# Introduction

Modern Linux systems provide a fascinating feature for overriding shared library behavior at runtime: `LD_PRELOAD`. 
This environment variable lets you inject a custom shared library before anything else is loaded — meaning you can 
intercept and modify calls to common functions like `open`, `read`, `connect`, and more.

In this post, we’ll walk through hooking the `open()` function using `LD_PRELOAD` and a simple shared object. No extra 
tooling required — just a few lines of C, and the ability to compile a `.so` file.

## Intercepting `open()`

Let’s write a tiny library that intercepts calls to `open()` and prints the file path being accessed. We’ll also 
forward the call to the real `open()` so the program behaves normally.

Create a file named `hook_open.c` with the following:

{% highlight c %}
#define _GNU_SOURCE
#include <stdio.h>
#include <stdarg.h>
#include <dlfcn.h>
#include <fcntl.h>

int open(const char *pathname, int flags, ...) {
    static int (*real_open)(const char *, int, ...) = NULL;
    if (!real_open)
        real_open = dlsym(RTLD_NEXT, "open");

    va_list args;
    va_start(args, flags);
    mode_t mode = va_arg(args, int);
    va_end(args);

    fprintf(stderr, "[HOOK] open() called with path: %s\n", pathname);
    return real_open(pathname, flags, mode);
}
{% endhighlight %}

This function matches the signature of `open`, grabs the "real" function using `dlsym(RTLD_NEXT, ...)`, and then 
forwards the call after logging it.

{% include callout.html type="info" title="Note" text="We use va_list to handle the optional mode argument safely." %}

## Compiling the Hook

Compile your code into a shared object:

{% highlight bash %}
gcc -fPIC -shared -o hook_open.so hook_open.c -ldl
{% endhighlight %}

Now you can use this library with any dynamically linked program that calls `open`.

## Testing with a Simple Program

Try running a standard tool like `cat` to confirm that it’s using `open()`:

{% highlight bash %}
LD_PRELOAD=./hook_open.so cat hook_open.c
{% endhighlight %}

You should see:

{% highlight plain %}
[HOOK] open() called with path: hook_open.c
#define _GNU_SOURCE
...
{% endhighlight %}

Each time the program calls `open()`, your hook intercepts it, logs the call, and passes control along.

## Notes and Gotchas

- This only works with dynamically linked binaries — statically linked programs don’t go through the dynamic linker.
- Some programs (like `ls`) may use `openat()` instead of `open()`. You can hook that too, using the same method.
- If your hook causes a crash or hangs, it’s often due to incorrect use of `va_arg` or missing `dlsym` resolution.

## Where to Go From Here

You can expand this basic example to:
- Block access to specific files
- Redirect file paths
- Inject fake contents
- Hook other syscalls like `connect()`, `write()`, `execve()`

`LD_PRELOAD` is a powerful mechanism for debugging, sandboxing, and learning how programs interact with the system. 
Just don’t forget — you’re rewriting the behavior of fundamental APIs at runtime. 

With great power comes great segfaults!
