---
layout: post
title: DROP DATABASE and other users
date: 2016-02-22
comments: false
categories: [ "postgres", "postgresql", "database", "drop", "db" ]
---

From time to time, when I've gone and issued `DROP DATABASE` on my postgres server, I'm returned with the following error:

{% highlight plain %}
ERROR:  database "xyz" is being accessed by other users
DETAIL:  There is 1 other session using the database.
{% endhighlight %}

All this is telling us is that we need to be the only user/connection on the database before performing such an operation. 

First of all, we need to prevent other users from making a connection to the database from this point. To do this, we'll use `REVOKE CONNECT` like so:

{% highlight sql %}
REVOKE CONNECT ON DATABASE "xyz" FROM public;
{% endhighlight %}

Next, we kill off every connection:

{% highlight sql %}
SELECT pg_stat_activity.pid, pg_terminate_backend(pg_stat_activity.pid)
FROM pg_stat_activity
WHERE pg_stat_activity.datname = 'xyz';
{% endhighlight %}

Now you can issue your `DROP DATABASE`.