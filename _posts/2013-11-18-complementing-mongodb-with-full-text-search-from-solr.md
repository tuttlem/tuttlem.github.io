---
layout: post
title: Complementing MongoDB with Full Text Search from Solr
date: 2013-11-18
comments: false
categories: [ "MongoDB", "Solr", "mongo-connector" ]
---

### Introduction

MongoDB is a great database, but one area that I've noticed it's been deficient in is full text search. Thankfully, there are some great tools around that we can employ to compliment Mongo and give it this functionality. Today's post will be a walk through to getting Solr & mongo-connector installed and configured on Debian Wheezy.

### Get the software

First up, install Solr on tomcat with the tomcat administration tools

{% highlight bash %}
$ sudo apt-get install solr-tomcat tomcat6-admin
{% endhighlight %}

Straight after this has installed, you'll need to configure a user to access these applications. Use your favorite text editor and open `/etc/tomcat6/tomcat-users.xml`. This file (like all of the configuration files) is really well commented. The steps I took here were:

* Added a "role" node for "manager-gui"
* Added "manager-gui" as a role to the "tomcat" user

In the end, you should have something sort-of like this:

{% highlight xml %}
<role rolename="tomcat"/>
<role rolename="manager-gui"/>
<user username="tomcat" password="tomcat" roles="tomcat,manager-gui"/>
{% endhighlight %}

Now that you've finished configuring all of the user access, restart tomcat.

{% highlight bash %}
$ sudo service tomcat restart
{% endhighlight %}

You can now check that tomcat is up and running by pointing your web browser at [http://localhost:8080/](http://localhost:8080/). When you click on the manager-app link, you'll be prompted for a username and password. As defined by the user configuration above, the username is "tomcat" and the password is "tomcat". Have a click around, you should also see Solr installed in there also. 

### Solr Schema

Now it's time we tell Solr exactly what we want to index. Remember, it's going to be a client to our mongo database - so any interesting fields that you want indexed will need to be mentioned here. Solr's schema file is found at `/etc/solr/conf/schema.xml`. Everyone's requirements are way to broad for me to go into depth here on what to do, but it would be a good time to look up the documentation and learn about how you want your data attributed: [http://wiki.apache.org/solr/SchemaXml](http://wiki.apache.org/solr/SchemaXml).

### Connecting to Mongo

Next, we're going to connect Solr to mongo using [mongo-connect](http://blog.mongodb.org/post/29127828146/introducing-mongo-connector). There's some more software that's needed to be installed here. mongo-connect is a python package that listens to mongo's oplog for "interesting" things, and then stores them away into Solr for fast searching later. We will need pip, some xml dependencies that mongo-connect relies on - then we can install the connector.

{% highlight bash %} 
$ sudo apt-get install python-dev python-pip
$ sudo apt-get install libxml2 libxml2-dev libxslt-dev
$ sudo pip install lxml cssselect
$ sudo pip install mongo-connector
{% endhighlight %}

### Running the connector

Now that you're all installed, it's time to start indexing some data. Again, everyone's requirements are going to be quite different - so it's a good time to go out and take a look at the [mongo-connector github page](https://github.com/10gen-labs/mongo-connector) to understand the full usage of the command. A typical execution of the command would look like this:

{% highlight bash %}
$ mongo-connector -m localhost:27217 -t http://localhost:8080/solr -o oplog_progress.txt -n alpha.foo,test.test -u _id -k auth.txt -a admin -d ./doc_managers/solr_doc_manager.py
{% endhighlight %}

From here, mongo-connector listens to changes and stores them away in Solr so that your full text search facility has them available.

That's it for Solr & MongoDB.