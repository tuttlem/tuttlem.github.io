---
layout: post
title: Upgrading AWS Linux to use Java 8
date: 2018-10-12
comments: false
categories: [ "aws", "java", "linux" ]
---

Some applications that you'll come across will require [Java 8](http://openjdk.java.net/projects/jdk8/) in order to run. By default (as of the time of this article), the [Amazon Linux AMI](https://aws.amazon.com/amazon-linux-ami/) has [Java 7](http://openjdk.java.net/projects/jdk7/) installed.

In order to upgrade these machines so that they are using [Java 8](http://openjdk.java.net/projects/jdk8/), use the following:

{% highlight bash %}
# make sure that you install java8 prior to removing java7
sudo yum install -y java-1.8.0-openjdk.x86_64

# update the binary links in-place
sudo /usr/sbin/alternatives --set java /usr/lib/jvm/jre-1.8.0-openjdk.x86_64/bin/java
sudo /usr/sbin/alternatives --set javac /usr/lib/jvm/jre-1.8.0-openjdk.x86_64/bin/javac

# remove java7
sudo yum remove java-1.7
{% endhighlight %}

That's it. You're now running [Java 8](http://openjdk.java.net/projects/jdk8/).
