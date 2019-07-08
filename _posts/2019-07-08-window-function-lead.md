---
layout: post
title: Window Function LEAD
date: 2019-07-08
comments: false
categories: [ "aws", "redshift", "window function" ]
---

This article is an extension of the [window function]({ post_url 2019-07-07-windowing-functions-in-redshift }), to follow up on each of the window functions.

The `LEAD` function returns values in an offset row. The offset is given by the second argument to the function itself. The offset pushes the index below (or after) the selected row.

To demonstrate the effect of this function, we'll take the `sales_id` value, and then we'll also take a led `sales_id`. By adjusting the offset value, you'll see how the leaded value ascends up the rows.

{% highlight sql %}
SELECT sale_id,
  LEAD(sale_id, 1) OVER (ORDER BY sale_id)
FROM public.sales
ORDER BY sale_date, sale_id
LIMIT 5;
{% endhighlight %}

|sale_id|lead|
|-------|----|
|1|2|
|2|3|
|3|4|
|4|5|
|5|6|

Adjusting the index so that it's now `2`; the lead row is now offset by 2.

|sale_id|lead|
|-------|----|
|1|3|
|2|4|
|3|5|
|4|6|
|5|7|

