---
layout: post
title: Hadoop 2 (2.2.0) setup on Debian
date: 2014-01-09
comments: false
---

Today's post will just be a walk through of the steps required to install Hadoop 2 on Debian Linux. Please note that this is for a single node installation only. This guide is heavily based on the Ubuntu instructions found [here](http://codesfusion.blogspot.com.au/2013/10/setup-hadoop-2x-220-on-ubuntu.html).

### Install Java

{% highlight bash %}	
# install the java jdk
$ sudo apt-get install openjdk-7-jdk
 
# make a jdk symlink
$ cd /usr/lib/jvm
$ ln -s java-7-openjdk-amd64 jdk
 
# make sure that ssh server is installed
$ sudo apt-get install openssh-server
{% endhighlight %}

### Add Hadoop Users and Groups

{% highlight bash %}	
# create a new group for hadoop
$ sudo addgroup hadoop
 
# create the hduser and put them in the hadoop group
$ sudo adduser --ingroup hadoop hduser
 
# add them to the sudo group also
$ sudo adduser hduser sudo
{% endhighlight %}

Now login as "hduser".

### SSH Certificates

{% highlight bash %}	
# generate your key
$ ssh-keygen -t rsa -P ''
 
# set your public key as authorized
$ cat ~/.ssh/id_rsa.pub >> ~/.ssh/authorized_keys
 
# test out ssh
$ ssh localhost
{% endhighlight %}

### Download Hadoop

{% highlight bash %}	
# downoad the package
$ cd ~
$ wget http://mirror.rackcentral.com.au/apache/hadoop/common/hadoop-2.2.0/hadoop-2.2.0.tar.gz
 
# extract the package
$ sudo tar vxzf hadoop-2.2.0.tar.gz -C /usr/local
$ cd /usr/local
$ sudo mv hadoop-2.2.0 hadoop
 
# get the hduser to take ownership
$ sudo chown -R hduser:hadoop hadoop
{% endhighlight %}

### Setup Environment Variables

Add the following lines to your `~/.bashrc`

{% highlight bash %}
# Hadoop variables
export JAVA_HOME=/usr/lib/jvm/jdk/
export HADOOP_INSTALL=/usr/local/hadoop
export PATH=$PATH:$HADOOP_INSTALL/bin
export PATH=$PATH:$HADOOP_INSTALL/sbin
export HADOOP_MAPRED_HOME=$HADOOP_INSTALL
export HADOOP_COMMON_HOME=$HADOOP_INSTALL
export HADOOP_HDFS_HOME=$HADOOP_INSTALL
export YARN_HOME=$HADOOP_INSTALL
{% endhighlight %}

Add the following lines to `/usr/local/hadoop/etc/hadoop/hadoop-env.sh`

{% highlight bash %}
# modify JAVA_HOME
export JAVA_HOME=/usr/lib/jvm/jdk/
{% endhighlight %}

Re-login to your machine as hduser, and check the hadoop version.

{% highlight bash %}
$ hadoop version
{% endhighlight %}

### Configure Hadoop

Add the following lines into the `<configuration>` node within `/usr/local/hadoop/etc/hadoop/core-site.xml`

{% highlight xml %}
<property>
   <name>fs.default.name</name>
   <value>hdfs://localhost:9000</value>
</property>
{% endhighlight %}

Add the following lines into the `<configuration>` node within `/usr/local/hadoop/etc/hadoop/yarn-site.xml`

{% highlight xml %}
<property>
   <name>yarn.nodemanager.aux-services</name>
   <value>mapreduce_shuffle</value>
</property>
<property>
   <name>yarn.nodemanager.aux-services.mapreduce.shuffle.class</name>
   <value>org.apache.hadoop.mapred.ShuffleHandler</value>
</property>
{% endhighlight %}

Make a copy of the mapred-site template file

{% highlight bash %}	
$ mv mapred-site.xml.template mapred-site.xml
$ vi mapred-site.xml
{% endhighlight %}

Add the following lines into the `<configuration>` node within `/usr/local/hadoop/etc/hadoop/mapred-site.xml`

{% highlight xml %}
<property>
   <name>mapreduce.framework.name</name>
   <value>yarn</value>
</property>
{% endhighlight %}

### Prepare the Filesystem

{% highlight bash %}	
# create the physical directories
$ cd ~
$ mkdir -p mydata/hdfs/namenode
$ mkdir -p mydata/hdfs/datanode
{% endhighlight %}

Add the following lines into the `<configuration>` node `/usr/local/hadoop/etc/hadoop/hdfs-site.xml`

{% highlight xml %}
<property>
   <name>dfs.replication</name>
   <value>1</value>
 </property>
 <property>
   <name>dfs.namenode.name.dir</name>
   <value>file:/home/hduser/mydata/hdfs/namenode</value>
 </property>
 <property>
   <name>dfs.datanode.data.dir</name>
   <value>file:/home/hduser/mydata/hdfs/datanode</value>
 </property>
{% endhighlight %}

Format the namenode

{% highlight bash %}	
$ hdfs namenode -format
{% endhighlight %}

### Start Hadoop

{% highlight bash %}	
$ start-dfs.sh
$ start-yarn.sh
 
# check that services are running
$ jps
{% endhighlight %}

Run the Example

{% highlight bash %}	
$ cd /usr/local/hadoop
$ hadoop jar ./share/hadoop/mapreduce/hadoop-mapreduce-examples-2.2.0.jar pi 2 5
{% endhighlight %}
