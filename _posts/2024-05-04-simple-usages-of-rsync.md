---
layout: post
title: Simple usages of rsync
date: 2024-05-04
comments: false
categories: [ "rsync", "admin" ]
---

### Introduction

`rsync` is an excellent tool to have as your disposal for working with file data.

In today's article, I'll go through some usages.

### Progress Copy

To securely and efficiently transfer data to (or from) a remote server while providing you progress updates you can use the following:

{% highlight bash %}
rsync -avzh --progress --stats user@server:/path/to/file output_name
{% endhighlight %}

* `-a` puts rsync into archive mode, preserving your file and link structures
* `-v` pumps up the logging output to give you verbose information
* `-z` will compress the file data as it's sent over the network
* `-h` makes the output human readable
* `--progress` provides a progress bar to be displayed, which tracks how far through we are
* `--stats` provides a full statistical run down after the process has completed
