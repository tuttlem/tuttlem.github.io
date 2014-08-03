---
layout: post
title: Getting started with OpenMP
date: 2014-08-03
comments: false
categories: ["openmp"]
---

### Introduction

[OpenMP](http://openmp.org/wp/) is an API for performing shared memory multiprocessing tasks in a variety of different languages and platforms. OpenMP hides away the complexities of parallel programming so that the developer can focus on writing their applications.

In today's post, I'll run through building OpenMP applications, some examples and what the internal `#pragma` statements mean.

### Building

Building applications against the OpenMP API is relatively simple. Using GCC:

{% highlight bash %}
$ gcc -o progname -fopenmp progname.c
{% endhighlight %}

Interestingly, the maximum number of threads that the framework will use (at runtime) can be controlled by setting the `OMP_NUM_THREADS` environment variable. This can be overridden in your programs with the `omp_set_num_threads`.

### Hello MP!

The standard "Hello, World" application that you'll find around the web is as follows:

{% highlight c %}
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
  int th_id, nthreads;

  #pragma omp parallel private(th_id)
  {
    th_id = omp_get_thread_num();
    printf("Hello world from thread %d\n", th_id);

    #pragma omp barrier
    if (th_id == 0) {
      nthreads = omp_get_num_threads();
      printf("There are %d threads\n", nthreads);
    }
  }

  return EXIT_SUCCESS;
}
{% endhighlight %}

Immediately, you'll notice the use of `#pragma` directives throughout the source. These are used to control the behaviour of the OpenMP API within your application. I'll go through a few of these below.

`#pragma omp parallel private` forces the variable `th_id` to be private to the thread that's executing. `omp_get_thread_num` <em>(as the name suggests)</em> gives you back the current thread number. `#pragma omp barrier` tells OpenMP to synchronize all threads to that point.

Some example invocations of this program look as follows:

{% highlight bash %}
$ OMP_NUM_THREADS=2 ./hello
Hello world from thread 0
Hello world from thread 1
There are 2 threads

$ OMP_NUM_THREADS=4 ./hello
Hello world from thread 0
Hello world from thread 3
Hello world from thread 2
Hello world from thread 1
There are 4 threads
{% endhighlight %}

Moving on, we'll take a look at a few of the `#pragma` directives you can use to control OpenMP.

### pragmas

All of the following directives in the table are applied in your source code with a `#pragma omp` prefix.

| Pragma          | Description                                                          |
|-----------------|----------------------------------------------------------------------|
| `atomic`        | Applied to a memory assignment that forces OpenMP to perform the update atomically |
| `parallel`      | Parallelize a segment of code. |
| `for`           | Distributes loop iterations over a group of threads |
| `ordered`       | Forces a block of code to be executed in sequential order |
| `parallel for`  | Combines both the `parallel` and `for` directives |
| `single`        | Forces a block of code to be run by a single thread |
| `master`        | Forces a block of code to be run by the master thread |
| `critical`      | Forces a block of code to be run by threads, one at a time |
| `barrier`       | Forces execution to wait for all threads to reach this point |
| `flush`         | Gives all threads a refresh of specified objects in memory |
| `threadprivate` | Forces named file-scope, namespace-scope or static block-scope variables private to a thread |

### References

There is plenty of information around on this particular topic. Take a look at the following links to dig into these topics even further:

* [Intel support for OpenMP](https://software.intel.com/sites/products/documentation/studio/composer/en-us/2011Update/compiler_c/index.htm#optaps/common/optaps_par_mpex.htm)
* [OpenMP Examples](http://openmp.org/mp-documents/OpenMP4.0.0.Examples.pdf)
* [GCC OpenMP Manual](https://gcc.gnu.org/onlinedocs/libgomp/)


