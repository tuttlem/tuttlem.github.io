---
layout: post
title: Moving from SQL Server to PostgreSQL
date: 2015-11-16
comments: false
categories: [ "sql", "database", "postgresql" ]
---

[SQL Server](https://en.wikipedia.org/wiki/Microsoft_SQL_Server) and [PostgreSQL](http://www.postgresql.org/) are both relational database systems and as such share similarities that should allow you to migrate your data structures between them. In today's post, I'm going to go through the process of migrating SQL Server data schema objects over to PostgreSQL.

### Immediate differences

Whilst both relational database systems implement a core set of the [standard language](https://en.wikipedia.org/wiki/SQL), there are implementation-specific features which need special consideration. So long as you are capable of wrangling text in your favorite editor, the conversion task shouldn't be that hard.

The batch terminator `GO` gets replaces by a much more familiar `;`. 

### Tables

First thing to do for tables is to generate your create scripts. Make sure that you:

* *Turn off* `DROP` statement generation for your objects
* *Turn on* index and keys generation

To safely qualify the names of objects within the database, SQL Server will surround its object names with square brackets `[]`, so you'll see definitions like this:

{% highlight sql %}
-- generated from SQL Server

CREATE TABLE [dbo].[Table1] (
    [ID]          INT IDENTITY (1, 1) NOT NULL,
    [Name]        VARCHAR (50) NOT NULL
    CONSTRAINT [PK_Table1] PRIMARY KEY CLUSTERED ([ID] ASC)
)
{% endhighlight %}

PostgreSQL uses double-quotes on object names and doesn't use the owner (in the above case `[dbo]`) to qualify names. 

In the above example, `Table1` is using `IDENTITY` on its primary key field `ID`. This gives us the auto-increment functionality that's so natural in relational database systems. There is a little extra work in PostgreSQL to emulate this behavior through the use of [CREATE SEQUENCE](http://www.postgresql.org/docs/9.0/static/sql-createsequence.html) and [nextval](http://www.postgresql.org/docs/9.0/static/functions-sequence.html).

The SQL Server definition above now looks like this for PostgreSQL:

{% highlight sql %}
-- migrated for PostgreSQL

CREATE SEQUENCE Table1Serial;

CREATE TABLE Table1 (
    ID          INT NOT NULL DEFAULT nextval('Table1Serial'),
    Name        VARCHAR (50) NOT NULL,
    CONSTRAINT PK_Table1 PRIMARY KEY (ID)
);
{% endhighlight %} 

### Stored Procedures

[Stored procedures](https://technet.microsoft.com/en-us/library/aa174792(v=sql.80).aspx) in SQL Server are considered a much more common citizen in the database world than [Stored procedures](http://www.postgresql.org/docs/9.0/static/sql-createfunction.html) in PostgreSQL. If your database design hinges on extensive use of stored procedures, you'll be in for a bit of redevelopment.

Both stored procedures and functions are created using the same syntax in PostgreSQL. The actions that either can perform differ though:

|                       | Stored Procedure | Function           |
|-----------------------|------------------|--------------------|
| Used in an expression | No               | Yes                |
| Return a value        | No               | Yes                |
| Output parameters     | Yes              | No                 |
| Return result set     | Yes              | Yes                |
| Multiple result sets  | Yes              | No                 |

A simple function that will square its input value looks as follows:

{% highlight sql %}
CREATE OR REPLACE FUNCTION SquareNum(n INT) RETURNS INT AS $$
  BEGIN
    RETURN n * n;
  END;
  $$ LANGUAGE plpgsql;
{% endhighlight %}

This can be invoked using `SELECT`.
  
{% highlight sql %}
SELECT SquareNum(5);
{% endhighlight %}

A more in-depth example involves returning a result set from within a stored procedure. You can do this in an unnamed fashion; you won't control the name of the cursor coming back. 

You can pull out a single record set:

{% highlight sql %}
CREATE OR REPLACE FUNCTION retrieve_entries() RETURNS refcursor AS $$
  DECLARE
    ref refcursor;
  BEGIN
    OPEN ref FOR SELECT id, name FROM table1;   
    RETURN ref;                                                       
  END;
  $$ LANGUAGE plpgsql;
{% endhighlight %}

Multiple record sets:

{% highlight sql %}
CREATE OR REPLACE FUNCTION show_entities_multiple() RETURNS SETOF refcursor AS $$
  DECLARE
    ref1 refcursor;           
    ref2 refcursor;                             
  BEGIN
    OPEN ref1 FOR SELECT id, name FROM table1;   
    RETURN NEXT ref1;                                                                              

    OPEN ref2 FOR SELECT id, name FROM table2;   
    RETURN NEXT ref2;
  END;
  $$ LANGUAGE plpgsql;
{% endhighlight %}

Invoking these stored procedures so that you can gather the information being returned, requires you to `FETCH` these details:

{% highlight sql %}
BEGIN;
SELECT retrieve_entities();
FETCH ALL IN "<unnamed portal 2>";
COMMIT;
{% endhighlight %}

Re-writing `retrieve_entities`, we can give the caller the option to name their cursor:

{% highlight sql %}
CREATE OR REPLACE FUNCTION retrieve_entities(ref refcursor) RETURNS refcursor AS $$
  BEGIN
    OPEN ref FOR SELECT id, name FROM table1;   
    RETURN ref;
  END;
  $$ LANGUAGE plpgsql;
{% endhighlight %}

The invocation of this procedure now requires a name:

{% highlight sql %}
BEGIN;
SELECT retrieve_entities('entities_cur');
FETCH ALL IN "entities_cur";
COMMIT;
{% endhighlight %}

A much more comprehensive run down of stored procedures/functions can be found [here](http://www.sqlines.com/postgresql/stored_procedures_functions) and [here](http://www.sqlines.com/postgresql/how-to/return_result_set_from_stored_procedure).

### Views

Views fall into the same category as Tables. The syntax remains very much the same with functions that change between the database platforms.

