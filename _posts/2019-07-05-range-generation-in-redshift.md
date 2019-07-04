---
layout: post
title: Range generation in Redshift
date: 2019-07-05
comments: false
categories: [ "database", "aws", "redshift" ]
---

A little while ago, I'd written an article on [Range generation in PostgreSQL]({% post_url 2019-04-06-range-generation-in-postgresql %}), and whilst [Redshift](https://docs.aws.amazon.com/redshift/latest/mgmt/welcome.html) and [PostgreSQL](https://www.postgresql.org/) are similar database beasts, they are quite different in terms of their query capabilities due to their different strengths.

In today's article, we'll walk through a couple of different solutions to range generation.

### generate_series

The [generate_series](https://www.postgresql.org/docs/9.1/functions-srf.html) is a pretty serious tool in terms of achieving exactly what this article is talking about. Unfortunately, there is no analog for this function within the [Redshift](https://docs.aws.amazon.com/redshift/latest/mgmt/welcome.html) database management system. Who knows; maybe one day.

Until then though, we need to create our own.

### The query

{% highlight sql %}
with digit as (
    select 0 as d union all
    select 1 union all select 2 union all select 3 union all
    select 4 union all select 5 union all select 6 union all
    select 7 union all select 8 union all select 9
),
seq as (
    select a.d + (10 * b.d) + (100 * c.d) + (1000 * d.d) as num
    from digit a
        cross join
        digit b
        cross join
        digit c
        cross join
        digit d
    order by 1
),
drange as (
    SELECT (getdate()::date - seq.num)::date       as start_date
    FROM seq
    LIMIT 20
)
SELECT start_date
FROM drange;
{% endhighlight %}

There's a bit going on in here.

The process starts with the `digit` CTE. Is does nothing more than counts from 0, up to 9.

{% highlight sql %}
select 0 as d union all
select 1 union all select 2 union all select 3 union all
select 4 union all select 5 union all select 6 union all
select 7 union all select 8 union all select 9
{% endhighlight %}

The next CTE `seq` uses the `digits` CTE, 4 times. Each time its treated as an increasing power of 10. `a` handles the `10^0`, `b` handles the `10^1`, etc. This catersian effect quickly multiplies our 0-9 range up to 0-9999. If more numbers are required, we simple add another `CROSS JOIN`, and treat the next power of 10.

{% highlight sql %}
select a.d + (10 * b.d) + (100 * c.d) + (1000 * d.d) as num
from digit a
    cross join
    digit b
    cross join
    digit c
    cross join
    digit d
order by 1
{% endhighlight %}

That's it as far a number generation. It's pretty easy.

I've seen other developers materialise this information into a physical table, and simply join to that table also.

### Extending into time

Applying this into some date ranges now, we use the `seq` CTE and a little date arithmatic. My example here just uses the numbers within `seq` to enumerate dates. Adding `LIMIT` to this query also prevents us from utilising all `10000` values produced by `seq`.

{% highlight sql %}
SELECT (getdate()::date - seq.num)::date       as start_date
FROM seq
LIMIT 20
{% endhighlight %}

And that it.

