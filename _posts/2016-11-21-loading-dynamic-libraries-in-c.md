---
layout: post
title: Loading dynamic libraries in C
date: 2016-11-21
comments: false
categories: [ "c", "dyndl", "dl" ]
---

Today's post is going to be a quick demonstration of the [dynamic library loading](http://tldp.org/HOWTO/Program-Library-HOWTO/dl-libraries.html) available through [Glibc](https://www.gnu.org/software/libc/).

Some really important links that shouldn't be glossed over if you're serious about some dynamic library development are:

* [Dynamically Loaded (DL) Libraries](http://tldp.org/HOWTO/Program-Library-HOWTO/dl-libraries.html)
* [dlopen](http://man7.org/linux/man-pages/man3/dlopen.3.html)
* [dlsym](http://man7.org/linux/man-pages/man3/dlsym.3.html)
* [dlerror](http://man7.org/linux/man-pages/man3/dlerror.3.html)

## Simple library

To start, we're going to write a tiny library. It'll have one function it it called `greet` that will send out a string:

{% highlight c %}
char *greeting = "Hello";

char *greet(void) {
  return greeting;
}
{% endhighlight %}

We can make `libtest.so` out of this with the following:

{% highlight bash %}
gcc -c -Wall -fPIC greet.c -o greet.o
gcc --shared greet.o -o libtest.so
{% endhighlight %}

We now have `libtest.so` as our shared library, ready to be loaded by our *host* program.

## Host program

The executable that takes care of loading this shared library, engaging the functions within it and executing the code will be called the *host* in this instance. First up, we'll use [dlopen](http://man7.org/linux/man-pages/man3/dlopen.3.html) to load the shared library off of disk:

{% highlight c %}
void *test_lib = dlopen(LIBTEST_SO, RTLD_LAZY);

if (!test_lib) {
  fprintf(stderr, "%s\n", dlerror());
  exit(EXIT_FAILURE);
}
{% endhighlight %}

Now that we've opened the library up, we'll use [dlsym](http://man7.org/linux/man-pages/man3/dlsym.3.html) to bury into the library and extract the `greet` function:

{% highlight c %}
char* (*greet)(void);

greet = (char * (*)(void)) dlsym(test_lib, "greet");

if ((error = dlerror()) != NULL) {
  fprintf(stderr, "%s\n", error);
  exit(EXIT_FAILURE);
}
{% endhighlight %}

We're referencing the function now. Notice the goofy cast: `(char * (*)(void))`. Here's a blurb from the manpage:

> /* According to the ISO C standard, casting between function
>    pointers and 'void *', as done above, produces undefined results.
>    POSIX.1-2003 and POSIX.1-2008 accepted this state of affairs and
>    proposed the following workaround:
>
>       *(void **) (&cosine) = dlsym(handle, "cos");
>
>   This (clumsy) cast conforms with the ISO C standard and will
>   avoid any compiler warnings.
>
>   The 2013 Technical Corrigendum to POSIX.1-2008 (a.k.a.
>   POSIX.1-2013) improved matters by requiring that conforming
>   implementations support casting 'void *' to a function pointer.
>   Nevertheless, some compilers (e.g., gcc with the '-pedantic'
>   option) may complain about the cast used in this program. */

Now we can call the greeter, and clean up with [dlclose](https://linux.die.net/man/3/dlclose)!

{% highlight c %}
printf("%s\n", greet());

dlclose(test_lib);
exit(EXIT_SUCCESS);
{% endhighlight %}

Because we do the dynamic loading of the library inside of our application, we don't need to tell the compiler of the library's existence. The host application will need to know about Glibc's `dl` library though:

{% highlight bash %}
gcc -Wall host.c -ldl -o host
{% endhighlight %}

## In closing

This has been a really quick lap around the `dl` library. The working prototype is crude, but forms the skeletal basis of a plugin-architecture should you be able to establish a strong contract between the pieces of library code and the host!

