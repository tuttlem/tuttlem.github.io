---
layout: post
title: PostgreSQL programming environment
date: 2016-03-05
comments: false
categories: [ "sql", "postgres", "postgresql", "pgsql", "psql" ]
---

Some databases rely heavily on their programming environment to deliver an [imperative programming environment](https://en.wikipedia.org/wiki/Imperative_programming) to where a developer can use high-level programming concepts such as control selection, iteration and flow. 

[Oracle](http://www.oracle.com) supports [PL/SQL](https://en.wikipedia.org/wiki/PL/SQL), [Sql Server](https://www.microsoft.com/en-us/server-cloud/products/sql-server/) supports [T-SQL](https://en.wikipedia.org/wiki/Transact-SQL). [PostgreSQL](http://www.postgresql.org) supports [PL/pgSQL](https://en.wikipedia.org/wiki/PL/pgSQL).

Prior to running through some top-level topics on the programming environment, some very important links:

* [PL/pgSQL Documentation](http://www.postgresql.org/docs/current/static/plpgsql.html)
* [PL/pgSQL on the Postgres Wiki](http://postgres.cz/wiki/PL/pgSQL_%28en%29)
* [Writing PostgreSQL Functions with PL/pgSQL](http://www.onlamp.com/pub/a/onlamp/2006/05/11/postgresql-plpgsql.html)

### Anonymous Code Blocks

Executing an anonymous code block is achieved with the [DO](http://www.postgresql.org/docs/current/static/sql-do.html) keyword. It emulates setting up any other function, with no parameters and no return value. Because you have a function body, you are afforded the ability to use a `DECLARE` section to house variables.

{% highlight sql %}
DO $$
BEGIN

END $$;
{% endhighlight %}

Adding variables to this block:

{% highlight sql %}
DO $$
DECLARE age INT;
BEGIN

  age := 24;

  IF age < 18 THEN
    -- handle the below 18 case
  END IF;
END$$;
{% endhighlight %}

Because of the parameter and return value constraints on anonymous code blocks, getting information out can be a little tricky. There are _some_ ways that we can get some information out though.

`RAISE NOTICE` allows us to present information to the console. This is considerably handy when we're testing a block of code.

{% highlight sql %}
DO $$
DECLARE age INT;
BEGIN

  age := 17;

  IF age < 18 THEN
    RAISE NOTICE 'Underage!';
  END IF;
END$$;
{% endhighlight %}

Of course, if your code is a re-usable block you should be creating a full function for it. This takes care of any input/output parameter issues as you take full control.

### Looking elsewhere

If this isn't to your liking, you can try any of the other operating environments/languages available:

* [PL/Tcl](http://www.postgresql.org/docs/current/static/pltcl.html)
* [PL/Perl](http://www.postgresql.org/docs/current/static/plperl.html)
* [PL/Python](http://www.postgresql.org/docs/current/static/plpython.html)

