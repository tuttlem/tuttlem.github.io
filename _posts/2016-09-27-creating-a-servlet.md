---
layout: post
title: Creating a Servlet
date: 2016-09-27
comments: false
categories: [ "java", "javax", "servlet" ]
---

Servlets are java applications that are run on the server, responding to different requests made by clients. They most commonly are setup to respond to web-based requests although they are not limited to this scope.

From [the tutorial](http://docs.oracle.com/javaee/6/tutorial/doc/bnafe.html):

> A servlet is a Java programming language class used to extend the capabilities of servers that host applications accessed by means of a request-response programming model. Although servlets can respond to any type of request, they are commonly used to extend the applications hosted by web servers. For such applications, Java Servlet technology defines HTTP-specific servlet classes.

> The `javax.servlet` and `javax.servlet.http` packages provide interfaces and classes for writing servlets. All servlets must implement the Servlet interface, which defines lifecycle methods. When implementing a generic service, you can use or extend the GenericServlet class provided with the Java Servlet API. The `HttpServlet` class provides methods, such as `doGet` and `doPost`, for handling HTTP-specific services.

In today's post, I'll walkthrough the creation of a servlet.

### Setup

First up, we're going to use [Maven](https://maven.apache.org/) to generate the project infrastructure required to support our servlet.

{% highlight shell %}
mvn archetype:generate \
    -DgroupId=org.test \
    -DartifactId=hello \
    -Dversion=1.0-SNAPSHOT \
    -DarchetypeArtifactId=maven-archetype-webapp \
    -DinteractiveMode=false
{% endhighlight %}

This will generate our `hello` project for us, and will create a directory structure that looks like this:

{% highlight plain %}
.
├── pom.xml
└── src
    └── main
        ├── resources
        └── webapp
            ├── index.jsp
            └── WEB-INF
                └── web.xml
{% endhighlight %}

### Get started

We can package our application and test it out pretty quickly:

{% highlight shell %}
mvn package
{% endhighlight %}

After processing, the `package` instruction will leave our project directly looking like this:

{% highlight plain %}
└── target
    ├── classes
    ├── hello
    │   ├── index.jsp
    │   ├── META-INF
    │   └── WEB-INF
    │       ├── classes
    │       └── web.xml
    ├── hello.war
    └── maven-archiver
        └── pom.properties
{% endhighlight %}

The `hello.war` file can now be deployed to our application server of choice for testing. In my example here, I'm using [Jetty](http://www.eclipse.org/jetty/) inside of a docker container.

{% highlight shell %}
docker run -ti --rm -p 8080:8080 \
           -v $(pwd)/target/hello.war:/var/lib/jetty/webapps/hello.war \
           jetty
{% endhighlight %}

Navigate to [http://localhost:8080/hello/](http://localhost:8080/hello/), and you'll see your jsp running.

