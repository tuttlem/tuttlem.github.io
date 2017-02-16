---
layout: post
title: Creating a Scala SBT project structure
date: 2017-02-16
comments: false
categories: [ "scala", "sbt", "project", "structure" ]
---

Today's post is going to be a tip on creating a project structure for your [Scala](https://www.scala-lang.org/) projects that is [SBT](http://www.scala-sbt.org/) ready. There's no real magic to it, just a specific structure that you can easily bundle up into a console application.

## The shell script

To kick start your project, you can simple use the following shell script:

{% highlight bash %}
#!/bin/zsh
mkdir $1
cd $1

mkdir -p src/{main,test}/{java,resources,scala}
mkdir lib project target

echo 'name := "$1"
version := "1.0"
scalaVersion := "2.10.0"' > build.sbt

cd ..
{% endhighlight %}

This will give you everything that you need to get up an running. You'll now have a structure like the following to work with:

{% highlight text %}
.
├── build.sbt
├── lib
├── project
├── src
│   ├── main
│   │   ├── java
│   │   ├── resources
│   │   └── scala
│   └── test
│       ├── java
│       ├── resources
│       └── scala
└── target
{% endhighlight %}