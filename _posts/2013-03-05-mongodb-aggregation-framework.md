---
layout: post
title: MongoDB Aggregation Framework
date: 2013-03-05
comments: false
---

### Introduction

Sometimes having the power of MapReduce at your fingertips and applying this technology to simpler aggregate queries can be more hassle than it needs to be. MongoDB provides a simpler solution (for a simpler class of problems) in the form of the [Aggregation framework](http://docs.mongodb.org/manual/applications/aggregation/). This framework allows you to develop queries within the mongo environment that are analogous to [GROUP BY](http://www.w3schools.com/sql/sql_groupby.asp), [HAVING](http://www.w3schools.com/sql/sql_having.asp), [COUNT](http://www.w3schools.com/sql/sql_func_count.asp), [SUM](http://www.w3schools.com/sql/sql_func_sum.asp), etc. that you would normally use in "relational land".

Today's post, I want to walk through a couple of simple queries on using this framework to maximise productivity when pivoting data.

### Marrying the old with the new

As a bit of a cheat's reference, the following table provides the some examples of aggregate queries in a relational database and how they transpose over to the Mongo aggregation environment.

The source of this table can be found [here](http://docs.mongodb.org/manual/reference/sql-aggregation-comparison/).

|Technique						| Relational 				| Aggregation Framework
|-------------------------------|---------------------------|-------------------------------------
Criteria matching 				| WHERE 					| [$match](http://docs.mongodb.org/manual/reference/aggregation/match/#_S_match)
Grouping 						| GROUP BY 					| [$group](http://docs.mongodb.org/manual/reference/aggregation/group/#_S_group)
Aggregate criteria filtering 	| HAVING 					| [$match](http://docs.mongodb.org/manual/reference/aggregation/match/#_S_match)
Result projection 				| SELECT 					| [$project](http://docs.mongodb.org/manual/reference/aggregation/project/#_S_project)
Record sorting 					| ORDER BY 					| [$sort](http://docs.mongodb.org/manual/reference/aggregation/sort/#_S_sort)
Limiting result sets 			| LIMIT or TOP 				| [$limit](http://docs.mongodb.org/manual/reference/aggregation/limit/#_S_limit)
Accumulation 					| SUM 						| [$sum](http://docs.mongodb.org/manual/reference/aggregation/sum/)
Counting 						| COUNT 					| [$sum](http://docs.mongodb.org/manual/reference/aggregation/sum/)
Dropping 						| SKIP 						| [$skip](http://docs.mongodb.org/manual/reference/aggregation/skip/#_S_skip)

### Counting records

Sql example:
{% highlight sql %}	
SELECT COUNT(*) AS count
FROM items
{% endhighlight %}

Mongo example:
	
{% highlight js %}	
db.items.aggregate( [
{ $group: { _id: null,
            count: { $sum: 1 } } }
] )
{% endhighlight %}

### Accumulating values

Sql example:
	
{% highlight sql %}	
SELECT SUM(price) AS total
FROM items
{% endhighlight %}

Mongo example:
	
{% highlight js %}	
db.items.aggregate( [
{ $group: { _id: null,
            total: { $sum: "$price" } } }
] )
{% endhighlight %}


### Aggregation with identifier

Sql example:
	
{% highlight sql %}	
SELECT category_id, SUM(price) AS total
FROM items
GROUP BY category_id
{% endhighlight %}

Mongo example:
	
{% highlight js %}	
db.items.aggregate( [
{ $group: { _id: "$category_id",
            total: { $sum: "$price" } } }
] )
{% endhighlight %}

### Aggregation with identifier and sorting

Sql example:
	
{% highlight sql %}	
SELECT category_id, SUM(price) AS total
FROM items
GROUP BY category_id
ORDER BY total
{% endhighlight %}

Mongo example:
	
{% highlight js %}	
db.items.aggregate( [
{ $group: { _id: "$category_id",
            total: { $sum: "$price" } } },
{ $sort: { total: 1 } }
] )
{% endhighlight %}


### Multiple Aggregation

Sql example:
	
{% highlight sql %}	
SELECT category_id, when SUM(price) AS total
FROM items
GROUP BY category_id, when
{% endhighlight %}

Mongo example:
	
{% highlight js %}	
db.items.aggregate( [
{ $group: { _id: { category_id: "$category_id",
                   when: "$when" },
            total: { $sum: "$price" } } }
] )
{% endhighlight %}


### Aggregate Filtration

Sql example:
	
{% highlight sql %}	
SELECT category_id, count(*)
FROM items
GROUP BY category_id
HAVING count(*) > 1
{% endhighlight %}

Mongo example:
	
{% highlight js %}	
db.items.aggregate( [
{ $group: { _id: "$category_id",
            count: { $sum: 1 } } },
{ $match: { count: { $gt: 1 } } } 
] )
{% endhighlight %}


### Multiple Aggregate Filtration

Sql example:
	
{% highlight sql %}	
SELECT category_id, when, SUM(price) AS total
FROM items
GROUP BY category_id, when
HAVING total > 100
{% endhighlight %}

Mongo example:
	
{% highlight js %}	
db.items.aggregate( [
{ $group: { _id: { category_id: "$category_id",
                   when: "$when" },
            total: { $sum: "$price" } } },
{ $match: { total: { $gt: 100 } } } 
] )
{% endhighlight %}


### Aggregate with Source Filtration

Sql example:
	
{% highlight sql %}	
SELECT category_id, SUM(price) AS total
FROM items
WHERE active = 1
GROUP BY category_id
{% endhighlight %}

Mongo example:
	
{% highlight js %}	
db.items.aggregate( [
{ $match: { active: 1 } },
{ $group: { _id: "$category_id",
            total: { $sum: "$price" } } }
] )
{% endhighlight %}


### Aggregate Filtration with Source Filtration

Sql example:
	
{% highlight sql %}	
SELECT category_id, SUM(price) AS total
FROM items
WHERE active = 1
GROUP BY category_id
HAVING total > 100
{% endhighlight %}

Mongo example:
	
{% highlight js %}	
db.items.aggregate( [
{ $match: { active: 1 } },
{ $group: { _id: "$category_id",
            total: { $sum: "$price" } } },
{ $match: { total: { $gt: 100 } } }
] )
{% endhighlight %}


### Aggregated joins

Sql example:
	
{% highlight sql %}	
SELECT category_id, SUM(co.weight) AS weight
FROM items i, components co
WHERE co.item_id = i.id
GROUP BY category_id
{% endhighlight %}

Mongo example:
	
{% highlight js %}	
db.items.aggregate( [
{ $unwind: "$components" },
{ $group: { _id: "$category_id",
            weight: { $sum: "$components.weight" } } }
] )
{% endhighlight %}


### Sub-query Aggregation

Sql example:
	
{% highlight sql %}	
SELECT COUNT(*)
FROM (SELECT category_id, when
      FROM items
      GROUP BY category_id, when) AS Table1
{% endhighlight %}

Mongo example:
	
{% highlight js %}	
db.items.aggregate( [
{ $group: { _id: { category_id: "$category_id",
                   when: "$when" } } },
{ $group: { _id: null, count: { $sum: 1 } } }
] )
{% endhighlight %}
