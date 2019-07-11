---
layout: post
title: Create tables from queries with Redshift
date: 2019-07-11
comments: false
categories: [ "aws", "redshift", "ctas", "database" ]
---

As a convenience to the developer, [AWS Redshift](https://aws.amazon.com/redshift/) offers [CTAS](https://docs.aws.amazon.com/redshift/latest/dg/r_CREATE_TABLE_AS.html) for those times where you need to materialise a physical table from the result of a query.

### Syntax

{% highlight text %}
CREATE [ [LOCAL ] { TEMPORARY | TEMP } ]
TABLE table_name
[ ( column_name [, ... ] ) ]
[ BACKUP { YES | NO } ]
[ table_attributes ]
AS query

where table_attributes are:
[ DISTSTYLE { EVEN | ALL | KEY } ]
[ DISTKEY ( distkey_identifier ) ]
[ [ { COMPOUND | INTERLEAVED } ] SORTKEY ( column_name [, ...] ) ]
{% endhighlight %}

Re-produced from [the documentation](https://docs.aws.amazon.com/redshift/latest/dg/r_CREATE_TABLE_AS.html).

As you can see, this is basically a `CREATE TABLE` statement, with a `SELECT` query at the end of it.

> The new table is loaded with data defined by the query in the command. The table columns have names and data types associated with the output columns of the query. The CREATE TABLE AS (CTAS) command creates a new table and evaluates the query to load the new table.


