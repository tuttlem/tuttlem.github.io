---
layout: post
title: Monitoring PostgreSQL Databases
date: 2016-02-28
comments: false
categories: [ "monitoring", "admin", "postgres", "postgresql" ]
---

Understanding what your database is doing and when is essential to runtime administration, maintenance, monitoring and reporting. Gaining insight into how your system responds to different workloads can also tell you how your current deployment is or isn't serving your purpose.

There are [many](http://www.postgresql.org/docs/9.1/static/monitoring.html) [great](http://www.postgresql.org/docs/9.2/static/monitoring-stats.html) [articles](https://wiki.postgresql.org/wiki/Monitoring) on this particular topic already. In today's post, I'm going to walk through a couple of the simple things you can do to check your system's runtime status.

### Unix Tools

When your database is being hosted in a unix-like environment, you're given the greatest tools at your disposal to understand what's happening.

[ps](http://linux.die.net/man/1/ps) can show you running processes on your system. Paired with [grep](http://linux.die.net/man/1/grep), you can focus [ps](http://linux.die.net/man/1/ps) to look at *postgres* processes only:

{% highlight bash %}
ps -ef | grep postgres
{% endhighlight %}

{% highlight text %}
postgres     1     0  0 11:05 ?        00:00:00 postgres
postgres    17     1  0 11:05 ?        00:00:00 postgres: checkpointer process  
postgres    18     1  0 11:05 ?        00:00:00 postgres: writer process  
postgres    19     1  0 11:05 ?        00:00:00 postgres: wal writer process  
postgres    20     1  0 11:05 ?        00:00:00 postgres: autovacuum launcher process  
postgres    21     1  0 11:05 ?        00:00:00 postgres: stats collector process  
{% endhighlight %}

[iostat](http://linux.die.net/man/1/iostat) and [vmstat](http://linux.die.net/man/8/vmstat) will also give you some operating system level insight to how your database application is performing.

### Statistics Collector

An important, integrated piece of the Postgres architecture is the [statistics collector](http://www.postgresql.org/docs/9.2/static/monitoring-stats.html). Using this, you can query to a very low level many pieces of information surrounding your system's performance.

The following except is just a small sample of all of the views offered by the statistics collector; which are made available to the developer.

| View Name           | Description                                 |
|---------------------|---------------------------------------------|
| `pg_stat_activity`    | One row per server process, showing information related to the current activity of that process, such as state and current query. See `pg_stat_activity` for details. |
| `pg_stat_bgwriter`    | One row only, showing statistics about the background writer process's activity. See `pg_stat_bgwriter` for details. |
| `pg_stat_database`    | One row per database, showing database-wide statistics. See `pg_stat_database` for details. |
| `pg_stat_all_tables`  | One row for each table in the current database, showing statistics about accesses to that specific table. See `pg_stat_all_tables` for details. |
| `pg_stat_sys_tables`  | Same as `pg_stat_all_tables`, except that only system tables are shown. |
| `pg_stat_user_tables` | Same as `pg_stat_all_tables`, except that only user tables are shown. |

The full list can be found [here](http://www.postgresql.org/docs/9.2/static/monitoring-stats.html#MONITORING-STATS-VIEWS-TABLE).

