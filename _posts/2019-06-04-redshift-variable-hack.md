---
layout: post
title: Redshift variable hack
date: 2019-06-04
comments: false
categories: [ "redshift", "aws" ]
---

Being a [data warehouse](https://en.wikipedia.org/wiki/Data_warehouse), [Redshift](https://aws.amazon.com/redshift/) from [AWS](https://aws.amazon.com/) doesn't provide the same level of programming platform that you may have had the luxury of using. 

The concept of variables in [Redshift](https://aws.amazon.com/redshift/) is non existant however with this clever hack you can fake your way into some variable based information.

{% highlight sql %}
WITH vars AS (
  SELECT 'John Smith' AS username,
         '2019-01-01T00:00:00.000Z'::timestamp AS process_date
)
SELECT *
FROM user_processsing
WHERE user == (SELECT username from vars) AND
      date_of_process = (SELECT process_date from vars);
{% endhighlight %}

The same trick can be achieved using `CREATE TEMP TABLE` also. The temporary table version will allow you to use your variables outside of the context of that one query. As above, the variables are bound to the CTE feeding the query.


