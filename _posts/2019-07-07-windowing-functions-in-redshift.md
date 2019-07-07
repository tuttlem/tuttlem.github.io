---
layout: post
title: Windowing functions in Redshift
date: 2019-07-07
comments: false
categories: [ "aws", "redshift", "window", "analytics" ]
---

### Introduction

[Window functions](https://en.wikipedia.org/wiki/SQL_window_function) allow database developers to perform analysis over partitions of information, very quickly. Prior to window functions, developers would need to create sub-queries (or common table expressions) that would allow their windows to be created. Primary queries would then work over the sub-queries to propgate a result. This now goes away, using the `OVER` syntax.

In today's article we'll walk through the basic structure of how to use window functions.

### Structure

As [documented by AWS](https://docs.aws.amazon.com/redshift/latest/dg/r_Window_function_synopsis.html), 

> the standard window function syntax is:

{% highlight text %}
function (expression) OVER (
[ PARTITION BY expr_list ]
[ ORDER BY order_list [ frame_clause ] ] )
{% endhighlight %}

> where function is one of the functions described in this section and expr_list is:

{% highlight text %}
expression | column_name [, expr_list ]
{% endhighlight %}

> and order_list is:

{% highlight text %}
expression | column_name [ ASC | DESC ]
[ NULLS FIRST | NULLS LAST ]
[, order_list ]
{% endhighlight %}

> and frame_clause is:

{% highlight text %}
ROWS
{ UNBOUNDED PRECEDING | unsigned_value PRECEDING | CURRENT ROW } |

{BETWEEN
{ UNBOUNDED PRECEDING | unsigned_value { PRECEDING | FOLLOWING } |
CURRENT ROW}
AND
{ UNBOUNDED FOLLOWING | unsigned_value { PRECEDING | FOLLOWING } |
CURRENT ROW }}
{% endhighlight %}

The `function` gets applied to an `expression` of columns that you're extracting from your query. The amount of information that the `function` is applied to is controlled by `PARTITION BY`. How the information is ordered within the window is then controlled by the `ORDER BY`.  

This is all text-booky kind of stuff, so let's apply it to some actual data in the database.

### Sample Data

For today's article, lets consider a really simple sales breakdown. No relationships to other table; everything is locallised and contained in here (no that it needs to be - it just makes the example simpler). 

{% highlight sql %}
CREATE TABLE public.sales (
  sale_id			INT PRIMARY KEY,
  sale_date			DATE NOT NULL,
  salesperson		VARCHAR(128) NOT NULL,
  quantity			INTEGER,
  unit_cost			DECIMAL,
  product_code		VARCHAR(32) NOT NULL
);
{% endhighlight %}

I've filled the table with some information, spanning two months.

|sale_id|sale_date|salesperson|quantity|unit_cost|product_code|
|-------|---------|-----------|--------|---------|------------|
|1|2018-05-02|Bob|1|26|VAC01|
|2|2018-05-13|Sally|3|20|BROOM01|
|3|2018-05-13|June|1|156|MOW01|
|4|2018-05-14|John|1|96|TRIM01|
|5|2018-05-25|Bob|2|96|TRIM01|
|6|2018-05-25|Sally|5|26|VAC01|
|7|2018-05-26|John|1|156|MOW01|
|8|2018-05-27|John|2|26|VAC01|
|9|2018-05-27|June|1|20|BROOM01|
|10|2018-05-27|June|3|156|MOW01|
|11|2018-06-02|Sally|1|26|VAC01|
|12|2018-06-03|John|3|20|BROOM01|
|13|2018-06-03|John|1|156|MOW01|
|14|2018-06-12|John|1|96|TRIM01|
|15|2018-06-13|Bob|2|96|TRIM01|
|16|2018-06-13|Sally|5|26|VAC01|
|17|2018-06-15|John|1|156|MOW01|
|18|2018-06-24|Bob|2|26|VAC01|
|19|2018-06-24|Sally|1|20|BROOM01|
|20|2018-06-29|John|3|156|MOW01|

Now that we have some information to work with, let's look at a simple example.

### Simplicity

In this first example, we'll demonstrate the monthly averages for all sales people; and compare each instance of a sale to it. 

Traditionally, we could solve this issue by employing a CTE to perform our aggregation, and join to this calculated set from our main query:

{% highlight sql %}
WITH avg_month AS (
  SELECT date_part('month', sale_date) as month,
  		 date_part('year', sale_date) as year,
  		 avg(quantity * unit_cost) as average
  FROM public.sales
  GROUP BY date_part('month', sale_date), date_part('year', sale_date)
)
SELECT salesperson,
	   sale_date,
       quantity * unit_cost,
       avg_month.average AS month_avg
FROM public.sales
INNER JOIN avg_month ON avg_month.month = date_part('month', sale_date)
AND						avg_month.year = date_part('year', sale_date)
ORDER BY salesperson, sale_date DESC;
{% endhighlight %}

The output of which starts to look like this:

|salesperson|sale_date|?column?|month_avg|
|-----------|---------|--------|---------|
|Bob|2018-06-24|52|135|
|Bob|2018-06-13|192|135|
|Bob|2018-05-25|192|135|
|Bob|2018-05-02|26|135|
|John|2018-06-29|468|135|
|John|2018-06-15|156|135|
|John|2018-06-12|96|135|
|John|2018-06-03|156|135|
|John|2018-06-03|60|135|
|John|2018-05-27|52|135|
|John|2018-05-26|156|135|
|John|2018-05-14|96|135|
|June|2018-05-27|468|135|
|June|2018-05-27|20|135|
|June|2018-05-13|156|135|
|Sally|2018-06-24|20|135|
|Sally|2018-06-13|130|135|
|Sally|2018-06-02|26|135|
|Sally|2018-05-25|130|135|
|Sally|2018-05-13|60|135|

We can simplify the query above greatly with the use of window functions. 

{% highlight sql %}
SELECT salesperson, sale_date, (quantity * unit_cost),
	   avg(quantity * unit_cost) OVER (
         PARTITION BY date_part('month', sale_date), date_part('year', sale_date)
       ) as month_avg
FROM public.sales
ORDER BY salesperson, sale_date DESC;
{% endhighlight %}

This offers the same result, with a much cleaner query experience.

To look into this data a little further, and perform the same operation over the salesperson's averages, we simply swap out the `PARTITION BY` expression:

{% highlight sql %}
SELECT salesperson, sale_date, (quantity * unit_cost),
	   avg(quantity * unit_cost) OVER (
         PARTITION BY salesperson
       ) as month_avg
FROM public.sales
ORDER BY salesperson, sale_date DESC;
{% endhighlight %}

This now tells us how each of the saleperson's sales performed against their average.

### Functions

With a basic understanding of the windowing function framework and how it can be applied to your queries, any of the supported functions can be use. The actual documentation of these functions can be found in the [AWS documentation for Redshift](https://docs.aws.amazon.com/redshift/latest/dg/c_Window_functions.html), so I won't reproduce that material here.

The list of functions are:

* AVG
* COUNT
* CUME_DIST
* DENSE_RANK
* FIRST_VALUE
* LAST_VALUE
* LAG
* LEAD
* LISTAGG
* MAX
* MEDIAN
* MIN
* NTH_VALUE
* NTILE
* PERCENT_RANK
* PERCENTILE_CONT
* PERCENTILE_DISC
* RANK
* RATIO_TO_REPORT
* ROW_NUMBER
* STDDEV_SAMP
* STDDEV_POP
* SUM
* VAR_SAMP
* VAR_POP

In future articles I'll cover these functions and how they can be used to perform analysis over your datasets.


