---
layout: post
title: Range generation in PostgreSQL
date: 2019-04-06
comments: false
categories: [ "database", "postgres", "range" ]
---

Generating ranges in [PostgreSQL]() can be a very useful tool for the creation of virtual tables to join to. Should your report require you to generate an entire range; left joining only to the values that need to be filled out.

The following code snippet will allow you to generate such a range:

{% highlight sql %}
WITH RECURSIVE cte_dates AS (
   SELECT '2018-01-01T00:00:00.000'::timestamp AS cd
   UNION ALL
   SELECT cd + interval '1 month'
   FROM cte_dates
   WHERE cd + interval '1 month' <= '2019-01-01T00:00:00.000'::timestamp
)
{% endhighlight %}

This snippet will create a table of dates, 1st of each month for the year 2018. 

The initial line of the CTE allows you to set the start of the range:

{% highlight sql %}
SELECT '2018-01-01T00:00:00.000'::timestamp AS cd
{% endhighlight %}

The frequency at which the range is sampled is then set with this line:

{% highlight sql %}
SELECT cd + interval '1 month'
{% endhighlight %}

Finally, the end of the range is set with the following line:

{% highlight sql %}
WHERE cd + interval '1 month' <= '2019-01-01T00:00:00.000'::timestamp
{% endhighlight %}
