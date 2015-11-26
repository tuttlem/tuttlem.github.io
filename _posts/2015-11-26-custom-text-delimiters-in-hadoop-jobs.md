---
layout: post
title: Custom text delimiters in Hadoop jobs
date: 2015-11-26
comments: false
categories: [ "hadoop", "textinputformat" ]
---

[Hadoop](https://hadoop.apache.org/) will process your data, line for line, splitting it on the `\n` newline character to send out to your mappers. In today's post, I'll demonstrate the ussage of the `textinputformat.record.delimiter` setting so that your Hadoop jobs can process different data structures.

### Configuration

When you're first setting up your job, you'll create a `Configuration` object. This object has arbitrary settings that can be applied to use through the use of the `set` method. To make a job work on a delimiter of `---`, you'd use the following:

{% highlight java %}
Configuration conf = new Configuration();
conf.set("textinputformat.record.delimiter", "---");
{% endhighlight %}

From here, there's no change to your code. Here's a very simple map reduce module that is using the custom format.

{% gist 57722af49ad3feeec384 %}
