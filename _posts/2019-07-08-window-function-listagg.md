---
layout: post
title: Window Function LISTAGG
date: 2019-07-08
comments: false
categories: [ "aws", "redshift", "window function" ]
---

This article is an extension of the [window function]({ post_url 2019-07-07-windowing-functions-in-redshift }), to follow up on each of the window functions.

The `LISTAGG` function will aggregate values that are collected within the window defined by the partition expression.

In this example, we'll list each sale in the database; adding a column at the end which will list all of the salespeople who made a sale on that particular day.

{% highlight sql %}
SELECT sale_date,
  LISTAGG(salesperson, ', ') OVER (PARTITION BY sale_date)
FROM public.sales
ORDER BY sale_date, sale_id;
{% endhighlight %}

This emits a dataset as follows:

|sale_date|listagg|
|---------|-------|
|2018-05-02|Bob|
|2018-05-13|Sally, June|
|2018-05-13|Sally, June|
|2018-05-14|John|
|2018-05-25|Bob, Sally|
|2018-05-25|Bob, Sally|
|2018-05-26|John|
|2018-05-27|John, June, June|
|2018-05-27|John, June, June|
|2018-05-27|John, June, June|
|2018-06-02|Sally|
|2018-06-03|John, John|
|2018-06-03|John, John|
|2018-06-12|John|
|2018-06-13|Bob, Sally|
|2018-06-13|Bob, Sally|
|2018-06-15|John|
|2018-06-24|Bob, Sally|
|2018-06-24|Bob, Sally|
|2018-06-29|John|
