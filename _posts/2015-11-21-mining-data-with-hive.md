---
layout: post
title: Mining data with Hive
date: 2015-11-21
comments: false
categories: [ "hadoop", "hive", "mapreduce", "mining", "analytics" ]
---

[Apache Hive](https://hive.apache.org/) is a database analytics technology that can be used to mine, structured, well formatted data. From the website:

> The Apache Hive™ data warehouse software facilitates querying and managing large datasets residing in distributed storage. Hive provides a mechanism to project structure onto this data and query the data using a SQL-like language called HiveQL. At the same time this language also allows traditional map/reduce programmers to plug in their custom mappers and reducers when it is inconvenient or inefficient to express this logic in HiveQL.

In today's post, I'm going to walk through getting up and running to your first query with Hive. 

### CSV

Probably the easiest place to start, is a [CSV](https://en.wikipedia.org/wiki/Comma-separated_values) file. Information in the file has its fields terminated by a comma `,` and lines by a newline `\n`. The example that I'll use to day contains the following data:

{% highlight text %}
id,first_name,last_name,age,country
1,John,Smith,24,ZA
2,Katie,Brown,27,AU
3,Stacey,Green,21,NZ
4,Joe,Taylor,34,US
5,Bob,Smith,20,US
{% endhighlight %}

Before Hive can get its hands on this information, we'll need to make it available to cluster by uploading it to HDFS.

{% highlight bash %}
bin/hadoop fs -put people.csv /user/root/people.csv
{% endhighlight %}

Now we can startup Hive and create the table structure that we'll be working with.

{% highlight text %}
hive> CREATE TABLE people (
    > id INT,
    > first_name STRING,
    > last_name STRING,
    > age INT,
    > country STRING
    > ) ROW FORMAT DELIMITED FIELDS TERMINATED BY ','
    > STORED AS TEXTFILE
    > TBLPROPERTIES ("skip.header.line.count"="1");
OK
Time taken: 1.195 seconds
{% endhighlight %}

There's a fair bit in this data definition. The full documentation on Hive's DDL can be found [here](https://cwiki.apache.org/confluence/display/Hive/LanguageManual+DDL). There are so many ways that you can accomplish things, and the example that I've listed is very simple.

`ROW FORMAT DELIMITED` tells Hive to use the default SerDe. We could have specified a regular expression here to interpret a line of the file, or specified our own custom SerDe but because this is so standard we only needed a field delimiter which is denoted by the `FIELDS TERMINATED BY`. There is also a `LINES TERMINATED BY` should you need to specify something other than `\n` as the terminator.

`STORED AS TEXTFILE` is the default. Our data is being stored in textfiles. Finally, `TBLPROPERTIES` allows arbitrary information to be applied to the create. We just wanted to tell the table that the first line in the files that it'll encounter should be discarded as it's the header line.

### Load in the data

Now that we've built a data structure, we can now put some data in it.

{% highlight text %}
hive> LOAD DATA INPATH '/user/root/people.csv' OVERWRITE INTO TABLE people;
Loading data to table default.people
Table default.people stats: [numFiles=1, numRows=0, totalSize=133, rawDataSize=0]
{% endhighlight %}

We're good to run that first query now!

{% highlight text %}
hive> SELECT * FROM people;
OK
1 John  Smith 24  ZA
2 Katie Brown 27  AU
3 Stacey  Green 21  NZ
4 Joe Taylor  34  US
5 Bob Smith 20  US
Time taken: 0.277 seconds, Fetched: 5 row(s)
{% endhighlight %}

Once we involve aggregates, these queries start to get submitted at MapReduce jobs:

{% highlight text %}
hive> SELECT country, AVG(age)
    > FROM people
    > GROUP BY country;
Query ID = root_20151122062444_77533aaa-5c95-4d2e-8742-b3891226c393
Total jobs = 1
Launching Job 1 out of 1
Number of reduce tasks not specified. Estimated from input data size: 1
In order to change the average load for a reducer (in bytes):
  set hive.exec.reducers.bytes.per.reducer=<number>
In order to limit the maximum number of reducers:
  set hive.exec.reducers.max=<number>
In order to set a constant number of reducers:
  set mapreduce.job.reduces=<number>
Starting Job = job_1448188576608_0002, Tracking URL = http://d62d018a5b3f:8088/proxy/application_1448188576608_0002/
Kill Command = /usr/local/hadoop/bin/hadoop job  -kill job_1448188576608_0002
Hadoop job information for Stage-1: number of mappers: 1; number of reducers: 1
2015-11-22 06:24:49,989 Stage-1 map = 0%,  reduce = 0%
2015-11-22 06:24:56,214 Stage-1 map = 100%,  reduce = 0%, Cumulative CPU 1.08 sec
2015-11-22 06:25:02,440 Stage-1 map = 100%,  reduce = 100%, Cumulative CPU 2.68 sec
MapReduce Total cumulative CPU time: 2 seconds 680 msec
Ended Job = job_1448188576608_0002
MapReduce Jobs Launched: 
Stage-Stage-1: Map: 1  Reduce: 1   Cumulative CPU: 2.68 sec   HDFS Read: 8041 HDFS Write: 32 SUCCESS
Total MapReduce CPU Time Spent: 2 seconds 680 msec
OK
AU  27.0
NZ  21.0
US  27.0
ZA  24.0
Time taken: 19.166 seconds, Fetched: 4 row(s)
{% endhighlight %} 

### Next steps

The [examples page](https://cwiki.apache.org/confluence/display/Hive/GettingStarted#GettingStarted-ApacheWeblogData) on the Hive site has some more complex data definitions, including being able to specify your own SerDe using python as well as processing an Apache web server log.

