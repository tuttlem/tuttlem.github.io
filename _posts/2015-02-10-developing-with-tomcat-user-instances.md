---
layout: post
title: Developing with Tomcat user instances
date: 2015-02-10
comments: false
categories: [ "java", "tomcat", "eclipse" ]
---

In today's post, I'll take you through installing [Apache Tomcat](http://tomcat.apache.org/) for the purposes of development.

### Installing a user instance

To get started, install `tomcat8` as you normally would:

{% highlight bash %}
$ sudo apt-get install tomcat8
{% endhighlight %}

For the purposes of development, it makes sense to have your own instance of tomcat which is away from the system installation. The `tomcat8-user` package allows you to do just that.

{% highlight bash %}
$ sudo apt-get install tomcat8-user
{% endhighlight %}

After this install is complete, you can create yourself a local tomcat instance that you can blow up without hurting the system's version. You do this with the `tomcat8-instance-create` command:

{% highlight bash %}
$ tomcat7-instance-create -p 10080 -c 10005 tomcat
{% endhighlight %}

The switches `-p` puts this instance listening for application requests on port 10080 and the `-c` switch puts the control port on 10005.

After you've done this, you'll be notified by the console.

{% highlight text %}
You are about to create a Tomcat instance in directory 'tomcat'
* New Tomcat instance created in tomcat
* You might want to edit default configuration in tomcat/conf
* Run tomcat/bin/startup.sh to start your Tomcat instance
{% endhighlight %}

The directory that has been setup for you now looks like this:

{% highlight text %}
.
├── bin
│   ├── setenv.sh
│   ├── shutdown.sh
│   └── startup.sh
├── conf
│   ├── catalina.properties
│   ├── context.xml
│   ├── logging.properties
│   ├── server.xml
│   ├── tomcat-users.xml
│   └── web.xml
├── logs
├── temp
├── webapps
└── work
{% endhighlight %}

### Integrating with Eclipse

To get Eclipse to play nicely with your user-local version of tomcat, you'll still need to add a few components. This tip is largly based off of the information in [this](http://askubuntu.com/questions/310767/how-should-i-install-apache-tomcat-7-for-use-with-eclipse) stack overflow question.

{% highlight bash %}
$ ln -s /usr/share/tomcat8/lib
$ cp /etc/tomcat8/policy.d/03catalina.policy conf/catalina.policy
$ ln -s /usr/share/tomcat8/bin/bootstrap.jar bin/bootstrap.jar
$ ln -s /usr/share/tomcat8/bin/tomcat-juli.jar bin/tomcat-juli.jar
$ mkdir -p common/classes;
$ mkdir -p server/classes;
$ mkdir -p shared/classes;
{% endhighlight %}

You can now add your local user instance of tomcat to Eclipse.

