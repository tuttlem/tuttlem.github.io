---
layout: post
title: Window Function LAG
date: 2019-07-08
comments: false
categories: [ "aws", "redshift", "window function" ]
---

This article is an extension of the [window function]({ post_url 2019-07-07-windowing-functions-in-redshift }), to follow up on each of the window functions.

The `LAG` function returns values in an offset row. The offset is given by the second argument to the function itself. The offset pushes the index above (or before) the selected row.

To demonstrate the effect of this function, we'll take the `sales_id` value, and then we'll also take a lagged `sales_id`. By adjusting the offset value, you'll see how the lagged value slips down the rows.

{% highlight sql %}
SELECT sale_id,
  LAG(sale_id, 1) OVER (ORDER BY sale_id)
FROM public.sales
ORDER BY sale_date, sale_id
LIMIT 5;
{% endhighlight %}

Note how the first row here doesn't have a value, as there isn't a row *above* row 1.

|sale_id|lag|
|-------|---|
|1||
|2|1|
|3|2|
|4|3|
|5|4|

Adjusting the index so that it's now `2`, youll see that both rows 1 and 2 now don't have a value.

|sale_id|lag|
|-------|---|
|1||
|2||
|3|1|
|4|2|
|5|3|

