---
layout: post
title: More Window Functions
date: 2019-07-08
comments: false
categories: [ "aws", "redshift", "window function" ]
---

This article is an extension of the [window function]({ post_url 2019-07-07-windowing-functions-in-redshift }), to follow up on each of the window functions.

### MAX

The `MAX` function will retrieve the maximum value that it sees within the window.

### MEDIAN

The `MEDIAN` function will calculate the median value for the range seen, within the window.

### MIN

The `MIN` function will retrieve the minimum value that it sees within the window.

### NTH_VALUE

Where the `LAG` and `LEAD` values are relative to the row in question, `NTH_VALUE` will retain the value at the literal offset specified.

### NTILE

`NTILE` ranks rows into equally proportioned groups within the window seen by the expression.

### PERCENT_RANK

Calculates the percent rank on rows seen by the window. The formula calculation is defined as:

{% highlight text %}
(x - 1) / (the number of rows in the window or partition - 1)
{% endhighlight %}

### PERCENTILE_CONT

`PERCENTIL_CONT` will calculate the linear interpolation between ordered values.

### PERCENTILE_DISC

Returns the value with the smallest cumulative distribution value.

### RATIO_TO_REPORT

Calculates a percentage where the row's value is the divisor, and the total amount for the window is the dividend.

In this example, we'll use `RATIO_TO_REPORT` to show us the percentage each sale makes over the period of a single day.

{% highlight sql %}
SELECT sale_date, salesperson, quantity::decimal * unit_cost,
  RATIO_TO_REPORT(quantity::decimal * unit_cost) OVER (PARTITION BY sale_date)
FROM public.sales
ORDER BY sale_date, sale_id;
{% endhighlight %}

`RATIO_TO_REPORT(quantity::decimal * unit_cost)` is what gives us the value that we're working with in terms of ratio; the `PARTITION BY sale_date` then gives us the window; these ratios need to be calculated for the day.

|sale_date|salesperson|?column?|ratio_to_report|
|---------|-----------|--------|---------------|
|2018-05-02|Bob|26|1.0|
|2018-05-13|Sally|60|0.2777777777777778|
|2018-05-13|June|156|0.7222222222222222|
|2018-05-14|John|96|1.0|
|2018-05-25|Bob|192|0.5962732919254659|
|2018-05-25|Sally|130|0.40372670807453415|
|2018-05-26|John|156|1.0|
|2018-05-27|John|52|0.0962962962962963|
|2018-05-27|June|20|0.037037037037037035|
|2018-05-27|June|468|0.8666666666666667|
|2018-06-02|Sally|26|1.0|
|2018-06-03|John|60|0.2777777777777778|
|2018-06-03|John|156|0.7222222222222222|
|2018-06-12|John|96|1.0|
|2018-06-13|Bob|192|0.5962732919254659|
|2018-06-13|Sally|130|0.40372670807453415|
|2018-06-15|John|156|1.0|
|2018-06-24|Bob|52|0.7222222222222222|
|2018-06-24|Sally|20|0.2777777777777778|
|2018-06-29|John|468|1.0|


### ROW_NUMBER

`ROW_NUMBER` is a utility function that simply gives the row an ordinal value, counting up from 1; over the window.

We count the sales for the day, by applying `ROW_NUMBER` over the sale_date.

{% highlight sql %}
SELECT sale_date, salesperson, quantity::decimal * unit_cost,
  ROW_NUMBER() OVER (PARTITION BY sale_date)
FROM public.sales
ORDER BY sale_date, sale_id;
{% endhighlight %}

|sale_date|salesperson|?column?|row_number|
|---------|-----------|--------|----------|
|2018-05-02|Bob|26|1|
|2018-05-13|Sally|60|1|
|2018-05-13|June|156|2|
|2018-05-14|John|96|1|
|2018-05-25|Bob|192|1|
|2018-05-25|Sally|130|2|
|2018-05-26|John|156|1|
|2018-05-27|John|52|1|
|2018-05-27|June|20|2|
|2018-05-27|June|468|3|
|2018-06-02|Sally|26|1|
|2018-06-03|John|60|1|
|2018-06-03|John|156|2|
|2018-06-12|John|96|1|
|2018-06-13|Bob|192|1|
|2018-06-13|Sally|130|2|
|2018-06-15|John|156|1|
|2018-06-24|Bob|52|1|
|2018-06-24|Sally|20|2|
|2018-06-29|John|468|1|

### STDDEV_SAMP and STDDEV_POP

`STDDEV_SAMP` and `STDDEV_POP` will find the sample and population standard deviation of the values seen in a window.

### SUM

The `SUM` function will retrieve the accumulated sum of an expression over the defined window.

### VAR_SAMP and VAR_POP

`VAR_SAMP` and `VAR_POP` will find the sample and population variance of the values seen in a window.



