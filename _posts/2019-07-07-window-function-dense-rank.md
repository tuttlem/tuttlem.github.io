---
layout: post
title: Window Function DENSE_RANK
date: 2019-07-07
comments: false
categories: [ "aws", "redshift", "window function" ]
---

This article is an extension of the [window function]({ post_url 2019-07-07-windowing-functions-in-redshift }), to follow up on each of the window functions.

The `DENSE_RANK` function will use the `ORDER BY` expression to determine a rank of a value amongst groups. If a `PARTITION BY` is present, this is also included in the reset of groups.

The important difference between `RANK` and `DENSE_RANK` is that `RANK` will use to total number of rows to nominate it's highest rank value; whereas `DENSE_RANK` will only use the number of ranks calculated, and the highest rank.

Take the following for example:

{% highlight sql %}
SELECT sale_id, quantity, unit_cost,
   DENSE_RANK() OVER (ORDER BY quantity DESC),
   RANK() OVER (ORDER BY quantity DESC)
FROM sales
ORDER BY quantity, sale_id;
{% endhighlight %}

The `sale_id`, `quantity`, and `unit_cost` are retrieved and ranked by the `quantity` value. Both `RANK` and `DENSE_RANK` are used in this example to illustrate the difference in value distribution (along the number line).

As there is a change in `quantity`, a new rank value is assigned:

|sale_id|quantity|unit_cost|dense_rank|rank|
|-------|--------|---------|----------|----|
|1|1|26|4|11|
|3|1|156|4|11|
|4|1|96|4|11|
|7|1|156|4|11|
|9|1|20|4|11|
|11|1|26|4|11|
|13|1|156|4|11|
|14|1|96|4|11|
|17|1|156|4|11|
|19|1|20|4|11|
|5|2|96|3|7|
|8|2|26|3|7|
|15|2|96|3|7|
|18|2|26|3|7|
|2|3|20|2|3|
|10|3|156|2|3|
|12|3|20|2|3|
|20|3|156|2|3|
|6|5|26|1|1|
|16|5|26|1|1|

