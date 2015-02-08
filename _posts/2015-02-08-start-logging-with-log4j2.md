---
layout: post
title: Start logging with log4j2
date: 2015-02-08
comments: false
categories: [ "java", "logging", "log4j2" ]
---

Probably the easiest way to get logging into your java application is with [log4j](http://logging.apache.org/log4j/2.x/) from [Apache](http://apache.org/). In today's post I'm going to setup some basic logging with the 2.x series of this library.

### Getting installed

The first thing to do is to download log4j from Apache's [downloads page](http://logging.apache.org/log4j/2.x/download.html). Once you have the binary distribution, extract it out into your preferable location. I personally put all of my libraries under `~/src/lib` that I use for my projects.

Add the following jars to your `lib` folder in your project that you're going to add logging to:

* log4j-api-2.1.jar
* log4j-core-2.1.jar

The versions may be differ, but as long as you've referenced the <strong>api</strong> file and the <strong>core</strong> file, you'll be fine.

### First logs

In order to get started, you'll need a concrete implementation of the `Logger` interface. This is pretty easy with the help of the `LogManager` class:

{% highlight java %}
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class LoggerTest {

static Logger log = LogManager.getLogger(
LoggerTest.class.getName()
);

public static void main(String[] args) {
}

}
{% endhighlight %}

We give `getLogger` the name of the class that we're in so that the resulting log can reference this name.

From here on, you'll reference the variable `log` to send anything into the log. Running this application as it is results in the following error from the log4j2 framework:

{% highlight text %}
ERROR StatusLogger No log4j2 configuration file found. Using default configuration: logging only errors to the console.
{% endhighlight %}

That's fine, we can still take a look at what an error looks like then by adding the following line to main:

{% highlight java %}
log.log(Level.ERROR, "Error message");
{% endhighlight %}

The result is that our line gets logged out:

{% highlight text %}
16:25:16.656 [main] ERROR org.keystoreplay.LoggerTest - Error message
{% endhighlight %}

### Configuration

The log4j manual has a [whole section](http://logging.apache.org/log4j/2.x/manual/configuration.html) dedicated to configuring the framework. If you're looking to do more complex things, that's the best place to look.

Quoted directly from the manual are the following bullet points regarding the process that the framework will go through to perform automatic configuration:

1. Log4j will inspect the "log4j.configurationFile" system property and, if set, will attempt to load the configuration using the ConfigurationFactory that matches the file extension.
2. If no system property is set the YAML ConfigurationFactory will look for log4j2-test.yaml or log4j2-test.yml in the classpath.
3. If no such file is found the JSON ConfigurationFactory will look for log4j2-test.json or log4j2-test.jsn in the classpath.
4. If no such file is found the XML ConfigurationFactory will look for log4j2-test.xml in the classpath.
5. If a test file cannot be located the YAML ConfigurationFactory will look for log4j2.yaml or log4j2.yml on the classpath.
6. If a YAML file cannot be located the JSON ConfigurationFactory will look for log4j2.json or log4j2.jsn on the classpath.
7. If a JSON file cannot be located the XML ConfigurationFactory will try to locate log4j2.xml on the classpath.
8. If no configuration file could be located the DefaultConfiguration will be used. This will cause logging output to go to the console.

What I like to do is contain all of my configuration files under a directory; normally called `etc` or `conf`. It doesn't matter where your configuration files go, just as long as the classpath references them.

A simple configuration for this system is as follows:

{% highlight xml %}
<?xml version="1.0" encoding="UTF-8"?>
<Configuration status="WARN">
  <Appenders>
    <Console name="Console" target="SYSTEM_OUT">
      <PatternLayout pattern="%d{HH:mm:ss.SSS} [%t] %-5level %logger{36} - %msg%n"/>
    </Console>
  </Appenders>
  <Loggers>
    <Root level="trace">
      <AppenderRef ref="Console"/>
    </Root>
  </Loggers>
</Configuration>
{% endhighlight %}

This was based off a config block found in the configuration manual. Note that the `Level` attribute has been set to `trace`. This will throw everything to the console for us. Adding the following line:

{% highlight java %}
log.log(Level.WARN, "A simple warning");
{% endhighlight %}

We get the following output (without an error message telling us that we haven't configured to logger):

{% highlight text %}
16:40:14.207 [main] WARN  org.keystoreplay.LoggerTest - A simple warning
16:40:14.210 [main] ERROR org.keystoreplay.LoggerTest - Error message
{% endhighlight %}