---
layout: post
title: Viewing shared dependencies with ldd
date: 2015-08-20
comments: false
categories: [ "libraries", "ldd", "linux" ]
---

Today's post is about the system utility [ldd](http://man7.org/linux/man-pages/man1/ldd.1.html). `ldd` will show you the shared object dependencies on the program that you supply as input.

{% highlight bash %}
$ ldd program
{% endhighlight %}

I ran `ldd` against an executable that I'd made from a previous post on libxml2 and was given the following results:

{% highlight text %}
linux-vdso.so.1 =>  (0x00007ffc6e987000)
libxml2.so.2 => /usr/lib/x86_64-linux-gnu/libxml2.so.2 (0x00007fbbfcca7000)
libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007fbbfc8e2000)
libdl.so.2 => /lib/x86_64-linux-gnu/libdl.so.2 (0x00007fbbfc6de000)
libz.so.1 => /lib/x86_64-linux-gnu/libz.so.1 (0x00007fbbfc4c5000)
liblzma.so.5 => /lib/x86_64-linux-gnu/liblzma.so.5 (0x00007fbbfc2a3000)
libm.so.6 => /lib/x86_64-linux-gnu/libm.so.6 (0x00007fbbfbf9d000)
/lib64/ld-linux-x86-64.so.2 (0x00007fbbfd00d000)
{% endhighlight %}

A word of <em>warning</em> is given in the man page, when using this tool however:

> In the usual case, ldd invokes the standard dynamic linker (see ld.so(8))  with the LD_TRACE_LOADED_OBJECTS environment variable set to 1, which causes the linker to display  the  library  dependencies.  Be aware, however, that  n some circumstances, some versions of ldd may attempt to obtain the dependency information by directly executing  the program.  Thus, you should never employ ldd on an untrusted executable, since this may result in the execution of arbitrary code. 

The suggestion from this manual page is to use the `objdump` command, as this won't invoke anything extra to find the dependencies:

{% highlight bash %}
$ objdump -p program | grep NEEDED
{% endhighlight %}

For the same program, this gives me the following output:

{% highlight text %}
NEEDED      libxml2.so.2
NEEDED      libc.so.6
{% endhighlight %}

