---
layout: post
title: Window Function FIRST_VALUE and LAST_VALUE
date: 2019-07-07
comments: false
categories: [ "aws", "redshift", "window functions"]
---

This article is an extension of the [window function]({ post_url 2019-07-07-windowing-functions-in-redshift }), to follow up on each of the window functions.

The `FIRST_VALUE` and `LAST_VALUE` functions will allow you to retrieve the first and last values for a given window (respectively).

Taking the sales example for another run, we can compare every single sale for the day to see how it stacked up against the first sale of the day with the following query:

{% highlight sql %}
SELECT sale_date, salesperson, quantity::decimal * unit_cost,
	   FIRST_VALUE(quantity::decimal * unit_cost) OVER (
         PARTITION BY sale_date
         ORDER BY sale_id
         ROWS UNBOUNDED PRECEDING
       ) as sale_number
FROM public.sales
ORDER BY sale_date;
{% endhighlight %}

The result of which is as you'd expect:

|sale_date|salesperson|?column?|sale_number|
|---------|-----------|--------|-----------|
|2018-05-02|Bob|26|26|
|2018-05-13|Sally|60|60|
|2018-05-13|June|156|60|
|2018-05-14|John|96|96|
|2018-05-25|Bob|192|192|
|2018-05-25|Sally|130|192|
|2018-05-26|John|156|156|
|2018-05-27|John|52|52|
|2018-05-27|June|20|52|
|2018-05-27|June|468|52|
|2018-06-02|Sally|26|26|
|2018-06-03|John|60|60|
|2018-06-03|John|156|60|
|2018-06-12|John|96|96|
|2018-06-13|Bob|192|192|
|2018-06-13|Sally|130|192|
|2018-06-15|John|156|156|
|2018-06-24|Bob|52|52|
|2018-06-24|Sally|20|52|
|2018-06-29|John|468|468|

This demonstrates how the first value can be drawn out of a set, per window.

Inversely, you simply use the `LAST_VALUE` function to get the last value; if you wanted to see how each sale stacked up against the last sale of the day:

{% highlight sql %}
SELECT sale_date, salesperson, quantity::decimal * unit_cost,
	   LAST_VALUE(quantity::decimal * unit_cost) OVER (
         PARTITION BY sale_date
         ORDER BY sale_id
         ROWS UNBOUNDED PRECEDING
       ) as sale_number
FROM public.sales
ORDER BY sale_date;
{% endhighlight %}


