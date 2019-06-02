---
layout: post
title: Perform your shell work in parallel
date: 2019-06-02
comments: false
categories: [ "linux", "shell", "parallel" ]
---

### Introduction

In some cases, breaking your larger programming problems into smaller, parallelizable units makes sense from a time complexity problem. If the work you are trying to perform exhibits some of these parallelizable characteristics, you should only need to wait for the longest of your jobs to finish.

In today's post, we'll be talking about [GNU Parallel](https://www.gnu.org/software/parallel/).

A summary from their website:

> GNU parallel is a shell tool for executing jobs in parallel using one or more computers. A job can be a single command or a small script that has to be run for each of the lines in the input. The typical input is a list of files, a list of hosts, a list of users, a list of URLs, or a list of tables. A job can also be a command that reads from a pipe. GNU parallel can then split the input and pipe it into commands in parallel.

### Input

The input system is quite complex. Delimiting the inputs with the `:::` operator, `parallel` will make a catersian product out of the input values. 

{% highlight text %}
parallel echo ::: John Paul Sally
John
Paul
Sally

parallel echo ::: John Paul Sally ::: Smith
John Smith
Paul Smith
Sally Smith

parallel echo ::: John Paul Sally ::: Smith Jones Brown
John Smith
John Jones
John Brown
Paul Smith
Paul Jones
Paul Brown
Sally Smith
Sally Jones
Sally Brown
{% endhighlight %}

Linkage is possible using `:::+`, should this flexibility be required.

{% highlight text %}
parallel echo ::: John Paul Sally :::+ Smith Jones Brown
John Smith
Paul Jones
Sally Brown
{% endhighlight %}

See more about [input sources](https://www.gnu.org/software/parallel/parallel_tutorial.html#Input-sources) in the [tutorial](https://www.gnu.org/software/parallel/parallel_tutorial.html).

### curl

For some examples, I'll use [curl](https://curl.haxx.se/).

Let's get three web pages downloaded:

* google.com
* yahoo.com
* zombo.com

Getting these down, one at a time times in at nearlly 1.2 seconds. 

{% highlight text %}
( curl www.google.com && curl www.yahoo.com && curl www.zombo.com; )  0.02s user 0.05s system 5% cpu 1.195 total
{% endhighlight %}

Running these downloads in parallel, we take half a second off the time:

{% highlight text %}
parallel curl {1} ::: www.google.com www.yahoo.com www.zombo.com  0.21s user 0.04s system 33% cpu 0.774 total
{% endhighlight %}

### Summing up

[GNU Parallel](https://www.gnu.org/software/parallel/) is a great utility to get multiple things done at once at the shell. Take a look at the tutorial and immediately become more productive.


