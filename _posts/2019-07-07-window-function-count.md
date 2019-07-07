---
layout: post
title: Window Function COUNT
date: 2019-07-07
comments: false
categories: [ "aws", "redshift", "window function" ]
---

This article is an extension of the [window function]({ post_url 2019-07-07-windowing-functions-in-redshift }), to follow up on each of the window functions.

The `COUNT` function will counts each row that contains non-NULL values for the given expression.

An example usage over the test data set would be:

{% highlight sql %}
SELECT sale_date, salesperson,
	   COUNT(*) OVER (
         PARTITION BY sale_date
         ORDER BY sale_id
         ROWS UNBOUNDED PRECEDING
       ) as sale_number
FROM public.sales
ORDER BY sale_date;
{% endhighlight %}

We list the `sale_date` and `salesperson` out for every sale on record. The `COUNT` partitions by the date of the sale made. This makes the `sale_number` tell us what number the sale was for that given date.

|sale_date|salesperson|sales_count|
|---------|-----------|-----------|
|2018-05-02|Bob|1|
|2018-05-13|Sally|1|
|2018-05-13|June|2|
|2018-05-14|John|1|
|2018-05-25|Bob|1|
|2018-05-25|Sally|2|
|2018-05-26|John|1|
|2018-05-27|John|1|
|2018-05-27|June|2|
|2018-05-27|June|3|
|2018-06-02|Sally|1|
|2018-06-03|John|1|
|2018-06-03|John|2|
|2018-06-12|John|1|
|2018-06-13|Bob|1|
|2018-06-13|Sally|2|
|2018-06-15|John|1|
|2018-06-24|Bob|1|
|2018-06-24|Sally|2|
|2018-06-29|John|1|

Taking `2018-05-27`, we can see that John got the first sale, June got the second and third.

