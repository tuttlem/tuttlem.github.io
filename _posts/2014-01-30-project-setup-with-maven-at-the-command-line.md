---
layout: post
title: Project setup with Maven at the Command Line
date: 2014-01-30
comments: false
---

### Introduction

A few utilities exist to manage your build, dependencies, test running for Java projects. One that I've seen that is quite intuitive (once you wrap your head around the xml structure) is [Maven](http://maven.apache.org/). According to the website, Maven is a "software project management and comprehension tool".

The main benefit I've seen already is how the developer's work-cycle is managed using the "POM" (project object model). The POM is just an XML file that accompanies your project to describe to Maven what your requirements are to build, test & package your software unit.

An excellent, short post can be found on the Maven website called ["Maven in 5 minutes"](http://maven.apache.org/guides/getting-started/maven-in-five-minutes.html).

Today's post will focus on Maven installation and getting a "Hello, world" project running.

### Installation

I'm on a Debian-flavored Linux distribution, so you may need to translate slightly between package managers. To get Maven installed, issue the following command at the prompt:

{% highlight bash %}
sudo apt-get install maven
{% endhighlight %}

Check that everything has installed correctly with the following command:

{% highlight bash %}
mvn --version
{% endhighlight %}

You should see some output not unlike what I've got here:

{% highlight text %}
Apache Maven 3.0.4
Maven home: /usr/share/maven
Java version: 1.7.0_25, vendor: Oracle Corporation
Java home: /usr/lib/jvm/java-7-openjdk-amd64/jre
Default locale: en_AU, platform encoding: UTF-8
OS name: "linux", version: "3.2.0-4-amd64", arch: "amd64", family: "unix"
{% endhighlight %}

If you're seeing output like what I've got above - that's it. You're installed now.

### First Project

Getting your first application together is pretty easy. A "quick start" approach is to use the quick start templates to generate a project structure like so:

{% highlight bash %}
cd ~/Source
mvn archetype:generate -DgroupId=org.temp -DartifactId=hello -DarchetypeArtifactId=maven-archetype-quickstart -DinteractiveMode=false
{% endhighlight %}

Maven will then go out and grab all that it needs from the web to get your project setup. It's now generated a project structure for you (in a directory called "hello") that looks like this:

{% highlight text %}
.
└── src
    ├── main
    │   └── java
    │       └── org
    │           └── temp
    └── test
        └── java
        	└── org
        		└── temp
{% endhighlight %}

Editing the file that they put into the source folder for you (at src/main/java/org/temp/App.java), you can see that your job is already done:

{% highlight java %}
package org.temp;

/**
 * Hello world!
 *
 */
public class App {
    public static void main( String[] args ) {
            System.out.println( "Hello World!" );
    }
}
{% endhighlight %}

Build it and give it a run!

{% highlight bash %}
mvn compile
mvn exec:java -Dexec.mainClass="org.temp.App"
{% endhighlight %}

You should see some output like this:

{% highlight text %}
[INFO] Scanning for projects...
[INFO]
[INFO] ------------------------------------------------------------------------
[INFO] Building hello 1.0-SNAPSHOT
[INFO] ------------------------------------------------------------------------
[INFO]
[INFO] >>> exec-maven-plugin:1.2.1:java (default-cli) @ hello >>>
[INFO]
[INFO] <<< exec-maven-plugin:1.2.1:java (default-cli) @ hello <<<
[INFO]
[INFO] --- exec-maven-plugin:1.2.1:java (default-cli) @ hello ---
Hello World!
[INFO] ------------------------------------------------------------------------
[INFO] BUILD SUCCESS
[INFO] ------------------------------------------------------------------------
[INFO] Total time: 2.331s
[INFO] Finished at: Thu Jan 30 12:46:28 EST 2014
[INFO] Final Memory: 7M/30M
[INFO] ------------------------------------------------------------------------
{% endhighlight %}

Most important line there is "Hello World!". 

There is so much more that you can do with Maven for your projects. Check out the documentation - that's it for today.