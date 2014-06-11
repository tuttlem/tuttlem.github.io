---
layout: post
title: Create a MapReduce Job using Java and Maven
date: 2014-01-30
comments: false
---

### Introduction
In a [previous post]({% post_url 2014-01-30-create-a-mapreduce-job-using-java-and-maven %}), I walked through the very basic operations of getting a Maven project up and running so that you can start writing Java applications using this managed environment.

In today's post, I'll walk through the modifications required to your POM to get a MapReduce job running on Hadoop 2.2.0.

If you don't have Maven installed yet, do that . . . maybe even have a bit of a read up on what it is, how it helps and how you can use it. Of course you'll also need your Hadoop environment up and running!
Project Setup
First thing you'll need to do, is to create a project structure using Maven in your workspace/source folder. I do this with the following command:

{% highlight bash %}
$ mvn archetype:generate -DarchetypeGroupId=org.apache.maven.archetypes -DgroupId=com.test.wordcount -DartifactId=wordcount
{% endhighlight %}

As it runs, this command will ask you a few questions on the details of your project. For all of the questions, I've found selecting the default value was sufficient. So . . . enter enter enter !

Once the process is complete, you'll have a project folder created for you. In this example, my project folder is "wordcount" (you can probably see where this tutorial is now headed). Changing into this folder and having a look at the directory tree, you should see the following:

{% highlight text %}
~/src/wordcount$ tree
.
├── pom.xml
└── src
    ├── main
    │   └── java
    │       └── com
    │           └── test
    │               └── wordcount
    │                   └── App.java
    └── test
        └── java
            └── com
                └── test
                    └── wordcount
                        └── AppTest.java

11 directories, 3 files
{% endhighlight %}

Now it's time to change the project environment so that it'll suit our Hadoop application target.

### Adjusting the POM for Hadoop

There's only a few minor alterations that are required here. The first one is, referencing the Hadoop libraries so that they are available to you to program against. We also specify the type of packaging for the application. Lastly, changing the language version (to something higher than what's specified as default).

Open up "pom.xml" in your editor of choice and add the following lines into the "dependencies" node.

{% highlight xml %}
<dependency>
  <groupid>org.apache.hadoop</groupid>
  <artifactid>hadoop-client</artifactid>
  <version>2.2.0</version>
</dependency>
{% endhighlight %}

This tells the project that we need the "hadoop-client" library (version 2.2.0).

We're now going to tell Maven to make us an executable JAR. Unfortunately, here's where the post is slightly pre-emptive upon itself. In order to tell Maven that we want an executable JAR, we need to tell it what class is holding our "main" function. . . we haven't written any code yet - but we will!

Create a "build" node and within that node create a "plugins" node and add the following to it:

{% highlight xml %}
<plugin>
  <groupId>org.apache.maven.plugins</groupId>
  <artifactId>maven-jar-plugin</artifactId>
  <configuration>
    <archive>
      <manifest>
        <addClasspath>true</addClasspath>
        <mainClass>com.test.wordcount.WordCount</mainClass>
      </manifest>
    </archive>
  </configuration>
</plugin>
{% endhighlight %}

More on the [maven-jar-plugin](http://maven.apache.org/plugins/maven-jar-plugin/) plugin can be found on the Maven website, but this block builds an executable JAR for us.

Add this next plugin to use Java 1.7 for compilation:

{% highlight xml %}
<plugin>
  <groupId>org.apache.maven.plugins</groupId>
  <artifactId>maven-compiler-plugin</artifactId>
  <configuration>
    <source>1.7</source>
    <target>1.7</target>
  </configuration>
</plugin>
{% endhighlight %}

That's all that should be needed now to perform compilation and packaging of our Hadoop application.

### The Job

I'll leave writing Hadoop Jobs to another post, but we still need some code to make sure our project is working (for today).

All I have done for today, is taken the WordCount code that's on the Hadoop Wiki here [http://wiki.apache.org/hadoop/WordCount](http://wiki.apache.org/hadoop/WordCount), changed the package name to align with what I created my project as `com.test.wordcount` and saved it into `src/main/java/com/test/wordcount/WordCount.java`

I removed the template provided `App.java` that was in this folder. I did make one minor patch to this code also. Here's my full listing that I've used for reference anyway.

{% highlight java %}
package com.test.wordcount;

import java.io.IOException;
import java.util.*;

import org.apache.hadoop.fs.Path;
import org.apache.hadoop.conf.*;
import org.apache.hadoop.io.*;
import org.apache.hadoop.mapreduce.*;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.TextInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.TextOutputFormat;

public class WordCount {

   public static class Map extends Mapper<LongWritable, Text, Text, IntWritable> {
      private final static IntWritable one = new IntWritable(1);
      private Text word = new Text();

      public void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
         String line = value.toString();
         StringTokenizer tokenizer = new StringTokenizer(line);
         while (tokenizer.hasMoreTokens()) {
            word.set(tokenizer.nextToken());
            context.write(word, one);
         }
      }
   }

   public static class Reduce extends Reducer<Text, IntWritable, Text, IntWritable> {

      public void reduce(Text key, Iterable<IntWritable> values, Context context)
         throws IOException, InterruptedException {
         int sum = 0;
         for (IntWritable val : values) {
            sum += val.get();
         }
         context.write(key, new IntWritable(sum));
      }
   }

   public static void main(String[] args) throws Exception {
      Configuration conf = new Configuration();

      Job job = new Job(conf, "wordcount");

      job.setJarByClass(WordCount.class);
      job.setOutputKeyClass(Text.class);
      job.setOutputValueClass(IntWritable.class);

      job.setMapperClass(Map.class);
      job.setReducerClass(Reduce.class);

      job.setInputFormatClass(TextInputFormat.class);
      job.setOutputFormatClass(TextOutputFormat.class);

      FileInputFormat.addInputPath(job, new Path(args[0]));
      FileOutputFormat.setOutputPath(job, new Path(args[1]));

      job.waitForCompletion(true);
   }

}
{% endhighlight %}

### Compile, Package & Run!

Our project is setup, our code is in place; it's now time to compile our project.

{% highlight bash %}
$ mvn clean install
{% endhighlight %}

Lots of downloading of dependencies and a bit of compilation go on . . . If all has gone to plan, you can now have a package to run. As usual, you'll need a text file of words to count. I've popped one up on hdfs called "input.txt".

{% highlight bash %}
$ hadoop jar target/wordcount-1.0-SNAPSHOT.jar input.txt wcout
{% endhighlight %}

You should now have a map reduce job running!