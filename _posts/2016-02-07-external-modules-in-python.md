---
layout: post
title: External modules in Python
date: 2016-02-07
comments: false
categories: [ "python", "external", "c", "module" ]
---

You can extend [Python](https://www.python.org/) relatively easily with the development libraries. Once installed, you can write a module in [C](https://en.wikipedia.org/wiki/C_%28programming_language%29), build it and start using it in your Python code. 

In today's post, I'll create a *Hello world* module and use it from python.

### Environment

In order to get started, you'll need to prepare your environment with the right tools. It's also and idea to create a bit of a project structure.

Create a directory that your code will go into. My source structure looks like this:

{% highlight text %}
.
├── Dockerfile
└── pymod
    ├── README
    ├── setup.py
    ├── src
    │   ├── hello.c
    │   └── hello.h
    └── test.py
{% endhighlight %}

My `Dockerfile` looks as follows. This should describe how to setup your environment:

{% highlight text %}
FROM python:2

RUN apt-get update && \
    apt-get install -y build-essential python-dev && \
    apt-get clean && \
    rm -Rf /tmp/* /var/tmp/*
{% endhighlight %}

### The module

The module itself really consists of a c header and source file and a `setup.py` file to build/install the module.

The header file looks as follows:

{% highlight c %}
#ifndef __hello_h_
#define __hello_h_

#include <Python.h>

static PyObject *hello_say_hello(PyObject *self, PyObject *args);

#endif
{% endhighlight %}

Note the `Python.h` header as well as the `PyObject` types being used. These are a part of the `python-dev` library that we installed before. This header file then gets implemented pretty simply. Here I've cheated using `printf` to do the printing for us:

{% highlight c %}
#include "hello.h"

static char module_doc[] = "This is a simple, useless, hello module";
static char say_hello_doc[] = "This function will say hello";

static PyMethodDef module_methods[] = {
  { "say_hello", hello_say_hello, METH_VARARGS, say_hello_doc },
  { NULL, NULL, 0, NULL }
};

PyMODINIT_FUNC init_hello(void) {
    PyObject *m = Py_InitModule3("_hello", module_methods, module_doc);

    if (m == NULL)
        return;
}

PyObject *hello_say_hello(PyObject *self, PyObject *args) {
  printf("I'm here");
  return Py_None;
}
{% endhighlight %}

A brief analysis of this code sees us building a `PyMethodDef` array. We expose it out using `Py_InitModule3' from within the initialization function (typed with `PyMODINIT_FUNC`). 

To out actual function itself, we're printing *"I'm here"* to the console and then bailing out with a return value of `Py_None`, which is equivalent to `None`.

### Building

To build our module, we'll use `setup.py`. It'll read as follows:

{% highlight python %}
from distutils.core import setup, Extension

setup(
  ext_modules=[Extension("_hello", ["src/hello.c"])]
)
{% endhighlight %}

To invoke the build, we issue the following:

{% highlight bash %}
python setup.py build_ext --inplace
{% endhighlight %}

### Testing it out

Now that our module is built, we can give it a test. Easiest to use the python console to do this for us:

{% highlight text %}
Python 2.7.10 (default, Sep  9 2015, 20:21:51) 
[GCC 4.9.2] on linux2
Type "help", "copyright", "credits" or "license" for more information.
>>> import _hello
>>> _hello.say_hello()
I'm here>>> 
{% endhighlight %}

### Finishing up

I couldn't pick a simpler example. More complex examples to come, but this is how we establish the bridge between **C** and **python**.