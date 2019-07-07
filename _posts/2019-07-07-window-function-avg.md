---
layout: post
title: Window Function AVG
date: 2019-07-07
comments: false
categories: [ "aws", "redshift", "window function" ]
---

This article is an extension of the [window function]({ post_url 2019-07-07-windowing-functions-in-redshift }), to follow up on each of the window functions.

The `AVG` function takes the arithmetic mean of the input that it sees.

An example usage over the test data set would be:

{% highlight sql %}
SELECT salesperson, sale_date, (quantity * unit_cost),
	   avg(quantity * unit_cost) OVER (
         PARTITION BY salesperson
       ) as month_avg
FROM public.sales
ORDER BY salesperson, sale_date DESC;
{% endhighlight %}

This takes the average cost value (`quantity * unit_cost`), but personalises the value by `salesperson`. Each salesperson is going to have their average calculated in isolation because of `PARTITION BY salesperson`.

