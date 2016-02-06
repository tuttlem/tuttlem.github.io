---
layout: post
title: Logging in python
date: 2016-02-06
comments: false
categories: [ "python", "logging" ]
---

Adding logging to your [python](https://www.python.org/) applications is simple with the standard [logging module](https://docs.python.org/2/library/logging.html). The best part about having a standard logging module, is that every application has the ability to send log information in the same way.

In today's post, I'll go through a small howto on setting up logging. 

### Logging levels

Much like any other logging framework, python's logging framework expects that you'll send messages out to the logger that belong to a particular class (or level). The levels are as follows:

| Level | Description |
|-------|-------------|
| DEBUG | Debug output, trace |
| INFO  | Informational output on successful events |
| WARNING | Pre-emptive notification of failures or unexpected events |
| ERROR | Processing failed |
| CRITICAL | Processing failed and the application can not recover |

### Getting started

{% highlight python %}
import logging

logging.debug('Debug message')
logging.info('Info message')
logging.warning('Warning message')
logging.error('Error message')
logging.critical('Critical message')
{% endhighlight %}

The code above ends up emitting the following text:

{% highlight text %}
WARNING:root:Warning message
ERROR:root:Error message
CRITICAL:root:Critical message
{% endhighlight %}

You notice a couple of things here; first we asked for a **debug** and **info** message here but we never saw one. Secondly, we see the format of the messages being written:

`LEVEL`:`NAME`:`MESSAGE`

We're using the `root` logger. 

The default logging level is `WARNING`. Anything requested below this (like `INFO` and `DEBUG`) are not emitted by the logger. We can change this with `basicConfig`.

{% highlight python %}
logging.basicConfig(level=logging.DEBUG)
{% endhighlight %}

Being that `DEBUG` is the lowest level logger that you can ask for, we should see all of the messages.

### Further configuration

To give your logger a little more context for your application, you can control the formatting parameters. The information that you can specify into your log lines is specified [here](https://docs.python.org/2/library/logging.html#logrecord-attributes).

For this example, we'll just have the time and the log line.

{% highlight python %}
logging.basicConfig(format='%(asctime)s %(message)s', level=logging.DEBUG)
{% endhighlight %}

We now are presented with the following:

{% highlight text %}
2016-02-06 22:26:27,494 Debug message
2016-02-06 22:26:27,494 Info message
2016-02-06 22:26:27,494 Warning message
2016-02-06 22:26:27,494 Error message
2016-02-06 22:26:27,494 Critical message
{% endhighlight %}

There is so much more to look at in this module and you can find it [here](https://docs.python.org/2/howto/logging.html).