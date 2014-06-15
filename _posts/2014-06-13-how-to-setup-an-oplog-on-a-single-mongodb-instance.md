---
layout: post
title: How to setup an oplog on a single MongoDB instance
date: 2014-06-13
comments: false
categories: [ "MongoDB", "standalone", "oplog" ]
---

The MongoDB oplog allows you to keep track of changes that have happened on your database in real-time. This is a very useful tool that isn't offered out of the box with a single server instance. You can follow these steps to enable to oplog on a standalone MongoDB instance.

Un-comment the following lines from your `/etc/mongodb.conf` file

{% highlight text %}
replSet=rs0
oplogSize=1024
{% endhighlight %}

This will give your MongoDB server a replica set identity of `rs0` and will allow your oplog to grow to 1024mb. You can tune these parameters to suit.

To complete the process, restart your MongoDB daemon and open a shell. You just need to issue `rs.initiate()` on the `local` database:

{% highlight text %}
michael@mongo:~$ mongo
MongoDB shell version: 2.6.1
connecting to: test
> use local
switched to db local
> rs.initiate()
{
   "info2" : "no configuration explicitly specified -- making one",
   "me" : "mongo:27017",
   "info" : "Config now saved locally.  Should come online in about a minute.",
      "ok" : 1
   }
> show collections
me
oplog.rs
startup_log
system.indexes
system.replset
{% endhighlight %}

You now have the oplog available to you.

