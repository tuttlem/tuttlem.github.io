---
layout: post
title: Create a UDF for Hive with Scala
date: 2017-02-17
comments: false
categories: [ "hive", "hadoop", "scala", "udf" ]
---

In today's post, I'm going to walk through the basic process of creating a [user defined function](https://en.wikipedia.org/wiki/User-defined_function#Apache_Hive) for [Apache Hive](https://hive.apache.org/) using the [Scala](https://www.scala-lang.org/).

*A quick _but important_ note*: I needed to use the JDK 1.7 to complete the following. Using 1.8 saw errors that suggested that Hive on my distribution of Hadoop was not supported.

## Setup your project

Create an [sbt](http://www.scala-sbt.org/)-based project, and start off adding the following to your `project/assembly.sbt`.

{% highlight scala %}
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.3")
{% endhighlight %}

What this had added is the [sbt-assembly](https://github.com/sbt/sbt-assembly) to your project. This allows you to bundle your scala application up as a [fat JAR](http://stackoverflow.com/questions/19150811/what-is-a-fat-jar). When we issue the command `sbt assemble` at the console, we invoke this plugin to construct the fat JAR for us.

Now we fill out the `build.sbt`. We need to reference an external JAR, called `hive-exec`. This JAR is available by itself from the [maven repository](https://mvnrepository.com/artifact/org.apache.hive/hive-exec/2.1.1). I took a copy of mine from the hive distribution installed on my server. Anyway, it lands in the project's `lib` folder.

{% highlight scala %}
name := "hive-udf"
version := "1.0"
scalaVersion := "2.11.1"
unmanagedJars in Compile += file("./lib/hive-exec-2.1.1.jar")
{% endhighlight %}

## Write your function

Now it's time to actually start writing some functions. In the following module, we're just performing some basic string manipulation with `trim`, `toUpperCase` and `toLowerCase`. Each of which is contained in its own class, deriving from the `UDF` type:

`scala/StringFunctions.scala`

{% highlight scala %}
package me.tuttlem.udf

import org.apache.hadoop.hive.ql.exec.UDF

class TrimString extends UDF {
  def evaluate(str: String): String = {
    str.trim
  }
}

class UpperCaseString extends UDF {
  def evaluate(str: String): String = {
    str.toUpperCase
  }
}

class LowerCaseString extends UDF {
  def evaluate(str: String): String = {
    str.toLowerCase
  }
}
{% endhighlight %}

Now that we've written all of the code, it's time to compile and assemble our JAR:

{% highlight plain %}
$ sbt assemble
{% endhighlight %}

## To invoke

Copying across the JAR into an accessible place for hive is the first step here. Once that's done, we can start up the hive shell and add it to the session:

{% highlight plain %}
ADD JAR /path/to/the/jar/my-udfs.jar;
{% endhighlight %}

Then, using the `CREATE FUNCTION` syntax, we can start to reference pieces of our module:

{% highlight plain %}
CREATE FUNCTION trim as 'me.tuttlem.udf.TrimString';
CREATE FUNCTION toUpperCase as 'me.tuttlem.udf.UpperCaseString';
CREATE FUNCTION toLowerCase as 'me.tuttlem.udf.LowerCaseString';
{% endhighlight %}

We can now use our functions:

{% highlight plain %}
hive> CREATE FUNCTION toUpperCase as 'me.tuttlem.udf.UpperCaseString';
OK
Time taken: 0.537 seconds
hive> SELECT toUpperCase('a test string');
OK
A TEST STRING
Time taken: 1.399 seconds, Fetched: 1 row(s)

hive> CREATE FUNCTION toLowerCase as 'me.tuttlem.udf.LowerCaseString';
OK
Time taken: 0.028 seconds
hive> SELECT toLowerCase('DON\'T YELL AT ME!!!');
OK
don't yell at me!!!
Time taken: 0.093 seconds, Fetched: 1 row(s)
{% endhighlight %}

That's it!