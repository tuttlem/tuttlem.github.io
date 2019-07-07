---
layout: post
title: Window Function CUME_DIST
date: 2019-07-07
comments: false
categories: [ "aws", "redshift", "window function" ]
---

This article is an extension of the [window function]({ post_url 2019-07-07-windowing-functions-in-redshift }), to follow up on each of the window functions.

The `CUME_DIST` function calculates the cumulative distribution of a value, for the data that's seen.

An example of calculating the cumulative distribution of each salesperson, and the amount of value that they've sold would look like this:

{% highlight sql %}
SELECT salesperson, quantity::decimal * unit_cost,
	   CUME_DIST() OVER (
         PARTITION BY salesperson
         ORDER BY (quantity::decimal * unit_cost)
       ) as distr
FROM public.sales
ORDER BY salesperson;
{% endhighlight %}

As sales people have sold the same amount in different sales, you can see that the cumulative distribution value makes accumulative jumps.

|salesperson|value|distr|
|-----------|-----|-----|
|Bob|26|0.25|
|Bob|52|0.5|
|Bob|192|1.0|
|Bob|192|1.0|
|John|52|0.125|
|John|60|0.25|
|John|96|0.5|
|John|96|0.5|
|John|156|0.875|
|John|156|0.875|
|John|156|0.875|
|John|468|1.0|
|June|20|0.3333333333333333|
|June|156|0.6666666666666666|
|June|468|1.0|
|Sally|20|0.2|
|Sally|26|0.4|
|Sally|60|0.6|
|Sally|130|1.0|
|Sally|130|1.0|

Taking an interesting piece of this window, we focus in on John.

|salesperson|value|distr|
|-----------|-----|-----|
|John|52|0.125|
|John|60|0.25|
|John|96|0.5|
|John|96|0.5|
|John|156|0.875|
|John|156|0.875|
|John|156|0.875|
|John|468|1.0|

The repeated value amounts don't seem to shift the distribution value (because of its cumulativity). By the time the next amount is present, there's a significant jump. This is a property of this set because the data is ordered by the `value` column.
