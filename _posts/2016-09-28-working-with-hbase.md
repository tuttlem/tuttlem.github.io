---
layout: post
title: Working with HBase
date: 2016-09-28
comments: false
categories: [ "hbase", "hadoop" ]
---

[Apache HBase](https://hbase.apache.org/) is a data storage technology that allows random, realtime read/write access to your big stores. It's modelled on Google's [Bigtable](http://research.google.com/archive/bigtable.html) paper and is available for use with [Apache Hadoop](http://hadoop.apache.org/). In today's article, I'll walk through some very simple usage of this technology.

### Installation

First up, we'll need to get some software installed. From the [downloads page](http://www.apache.org/dyn/closer.cgi/hbase/), you can grab a release. Once this is downloaded, get it unpacked onto your machine. In this instance, we'll be using HBase in [standalone mode](http://archive.cloudera.com/cdh5/cdh/5/hbase-0.98.6-cdh5.3.4/book/standalone_dist.html)

>This is the default mode. Standalone mode is what is described in the Section 1.2, “Quick Start - Standalone HBase” section. In standalone mode, HBase does not use HDFS -- it uses the local filesystem instead -- and it runs all HBase daemons and a local ZooKeeper all up in the same JVM. Zookeeper binds to a well known port so clients may talk to HBase.

If you need to perform any further configuration, the `/conf` folder holds the xml files required. To put your root folders into more sane places, you can change the values of `conf/hbase-site.xml`:

{% highlight xml %}
<?xml version="1.0"?>
<?xml-stylesheet type="text/xsl" href="configuration.xsl"?>
<configuration>
  <property>
    <name>hbase.rootdir</name>
    <value>file:///DIRECTORY/hbase</value>
  </property>
  <property>
    <name>hbase.zookeeper.property.dataDir</name>
    <value>/DIRECTORY/zookeeper</value>
  </property>
</configuration>
{% endhighlight %}

Start up your server:

{% highlight plain %}
$ ./bin/start-hbase.sh 
starting master, logging to /opt/hbase-1.2.3/bin/../logs/hbase--master-0f0ebda04483.out
SLF4J: Class path contains multiple SLF4J bindings.
SLF4J: Found binding in [jar:file:/opt/hbase-1.2.3/lib/slf4j-log4j12-1.7.5.jar!/org/slf4j/impl/StaticLoggerBinder.class]
SLF4J: Found binding in [jar:file:/usr/local/hadoop-2.7.0/share/hadoop/common/lib/slf4j-log4j12-1.7.10.jar!/org/slf4j/impl/StaticLoggerBinder.class]
SLF4J: See http://www.slf4j.org/codes.html#multiple_bindings for an explanation.
SLF4J: Actual binding is of type [org.slf4j.impl.Log4jLoggerFactory]
{% endhighlight %}

### Shell!

Now that HBase is running, we can shell into it and have a poke around.

{% highlight plain %}
$ ./bin/hbase shell
SLF4J: Class path contains multiple SLF4J bindings.
SLF4J: Found binding in [jar:file:/opt/hbase-1.2.3/lib/slf4j-log4j12-1.7.5.jar!/org/slf4j/impl/StaticLoggerBinder.class]
SLF4J: Found binding in [jar:file:/usr/local/hadoop-2.7.0/share/hadoop/common/lib/slf4j-log4j12-1.7.10.jar!/org/slf4j/impl/StaticLoggerBinder.class]
SLF4J: See http://www.slf4j.org/codes.html#multiple_bindings for an explanation.
SLF4J: Actual binding is of type [org.slf4j.impl.Log4jLoggerFactory]
HBase Shell; enter 'help<RETURN>' for list of supported commands.
Type "exit<RETURN>" to leave the HBase Shell
Version 1.2.3, rbd63744624a26dc3350137b564fe746df7a721a4, Mon Aug 29 15:13:42 PDT 2016

hbase(main):001:0> 
{% endhighlight %}

First up, we'll create a table called `person` with a column family of `name':

{% highlight plain %}
hbase(main):002:0> create 'person', 'name'
0 row(s) in 1.5290 seconds

=> Hbase::Table - person
{% endhighlight %}

Now we can insert some tables into our table:

{% highlight plain %}
hbase(main):004:0> put 'person', 'row1', 'name:first', 'John'
0 row(s) in 0.1430 seconds

hbase(main):005:0> put 'person', 'row2', 'name:first', 'Mary'
0 row(s) in 0.0150 seconds

hbase(main):006:0> put 'person', 'row3', 'name:first', 'Bob'
0 row(s) in 0.0080 seconds

hbase(main):007:0> scan 'person'
ROW                   COLUMN+CELL                                               
 row1                 column=name:first, timestamp=1475030956731, value=John    
 row2                 column=name:first, timestamp=1475030975840, value=Mary    
 row3                 column=name:first, timestamp=1475030988587, value=Bob
{% endhighlight %}

Values can also be read out of our table:

{% highlight plain %}
hbase(main):009:0> get 'person', 'row1'
COLUMN                CELL                                                      
 name:first           timestamp=1475030956731, value=John                       
1 row(s) in 0.0250 seconds
{% endhighlight %}

Now, we can clean up our test:

{% highlight plain %}
hbase(main):011:0> disable 'person'
0 row(s) in 2.3000 seconds

hbase(main):012:0> drop 'person'
0 row(s) in 1.2670 seconds
{% endhighlight %}

### Following up

Now that we can start to work with HBase, further posts will focus on designing schemas and processing data into the store.

