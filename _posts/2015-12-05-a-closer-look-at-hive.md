---
layout: post
title: A closer look at Hive
date: 2015-12-05
comments: false
categories: [ "hadoop", "hive", "data" ]
---

In a [previous post]({% post_url 2015-11-21-mining-data-with-hive %}) we went through a fairly simple example of how to get up and running quickly with [Apache Hive](https://hive.apache.org/). In today's post I'll take a deeper dive a look a little closer at the different aspects of using it.

Everything that I mention in this article can be found in the [language manual](https://cwiki.apache.org/confluence/display/Hive/LanguageManual) on the [Apache wiki](https://cwiki.apache.org/confluence/dashboard.action).

For the examples that are listed in this blogpost, I'm using data that can be downloaded from the [FAA site](https://www.faa.gov/data_research/).

### Databases

Your first job, much the same with any database system is to [create a database](https://cwiki.apache.org/confluence/display/Hive/LanguageManual+DDL#LanguageManualDDL-Create/Drop/AlterDatabase).

{% highlight text %}
hive> CREATE DATABASE first;
OK
Time taken: 0.793 seconds

hive> USE first;
OK
Time taken: 0.037 seconds
{% endhighlight %}

You can also use `EXISTS` in your creation and destruction statements to ensure something is or isn't there.

{% highlight text %}
hive> CREATE DATABASE IF NOT EXISTS first;
OK
Time taken: 0.065 seconds

hive> DROP DATABASE IF EXISTS first;
OK
Time taken: 0.26 seconds
{% endhighlight %}

### Tables

To create a table that's managed by the hive warehouse, we can use the following.

{% highlight text %}
hive> CREATE TABLE airports (
    > iata STRING, airport STRING, city STRING, 
    > state STRING, country STRING, 
    > lat DECIMAL, long DECIMAL
    > ) ROW FORMAT DELIMITED FIELDS TERMINATED BY ",";
OK
Time taken: 0.324 seconds
{% endhighlight %}

This table can then be filled with data that is sourced locally:

{% highlight text %}
hive> LOAD DATA LOCAL INPATH '/srv/airports.csv' 
    > OVERWRITE INTO TABLE airports;
Loading data to table faa.airports
Table faa.airports stats: [numFiles=1, numRows=0, totalSize=244383, rawDataSize=0]
OK
Time taken: 1.56 seconds
{% endhighlight %}

You can also create an external table using the following syntax:

{% highlight text %}
hive> CREATE EXTERNAL TABLE carriers ( 
    > code STRING, description STRING
    > ) ROW FORMAT DELIMITED FIELDS TERMINATED BY "," 
    > LOCATION '/user/root/carriers';
OK
Time taken: 0.408 seconds
{% endhighlight %}

You can see that this has used a file hosted on [HDFS](https://hadoop.apache.org/docs/r1.2.1/hdfs_design.html) as the data source. The idea is that the existing file (that we'd specified in the `LOCATION` statement) will now be accessible to hive through this table.

From the wiki:

> The `EXTERNAL` keyword lets you create a table and provide a `LOCATION` so that Hive does not use a default location for this table. This comes in handy if you already have data generated. When dropping an `EXTERNAL` table, data in the table is NOT deleted from the file system.

> An `EXTERNAL` table points to any HDFS location for its storage, rather than being stored in a folder specified by the configuration property hive.metastore.warehouse.dir.

It's important to note that when you `DROP` an external table, the underlying data is **NOT** deleted.

### Views

You can provide a more targeted representation of your data to you users by offering them views. Views allow you to also specify aggregate functions as columns. In the following view, we simple retrieve all of the countries that an airport is located; along with the number of airports located in that country.

{% highlight text %}
hive> CREATE VIEW airports_per_country_vw
    > AS
    > SELECT country, COUNT(*) AS country_count 
    > FROM airports 
    > GROUP BY country;
OK
Time taken: 0.134 seconds
{% endhighlight %}

### Partitions and Buckets

Because you'll be working with very large data sets, Hive offers you the ability to partition data on columns that you nominate. These partitions are then broken down even further with into buckets.

From the wiki:

> Partitioned tables can be created using the `PARTITIONED BY` clause. A table can have one or more partition columns and a separate data directory is created for each distinct value combination in the partition columns. Further, tables or partitions can be bucketed using `CLUSTERED BY` columns, and data can be sorted within that bucket via `SORT BY` columns. This can improve performance on certain kinds of queries.

So this technique does change the way data is physically structured on disk. It tried to structure it in such a way that it'll bias towards the performance of the queries that you're running. Of course, this is up to you as you need to define which fields to partition and cluster by.

Here's the `airports` table, partitioned by `country`.

{% highlight text %}
hive> CREATE EXTERNAL TABLE airport_part_by_country (
    > iata STRING, airport STRING, city STRING, 
    > state STRING, lat DECIMAL, long DECIMAL
    > ) PARTITIONED BY (country STRING) 
    > ROW FORMAT DELIMITED FIELDS TERMINATED BY "," 
    > LOCATION '/user/root/partitioned';
OK
Time taken: 0.128 seconds
{% endhighlight %}

When this table gets clustered into buckets, the database developer needs to specify the number of buckets to possible distribute across. From here, hive will make decisions on which bucket to place the data into with the following formula:

{% highlight text %}
target_bucket = hash_value(bucket_column) % bucket_count
{% endhighlight %}

We then create and fill the bucketed store like so:

{% highlight text %}
-- create the bucketed store
hive> CREATE EXTERNAL TABLE airports_b (
    > iata string, airport string, city string, 
    > state string, lat decimal, long decimal
    > ) PARTITIONED BY (country string) 
    > CLUSTERED BY (state) INTO 100 BUCKETS;

-- fill the bucketed store
hive> set hive.enforce.bucketing = true;
hive> FROM airports 
    > INSERT OVERWRITE TABLE airports_b 
    > PARTITION (country='USA') 
    > SELECT iata, airport, city, state, lat, long;
{% endhighlight %}

