---
layout: post
title: Foreign Data Wrappers with Postgres
date: 2016-01-21
comments: false
categories: [ "postgres", "fdw", "foreign", "wrapper", "postgres_fdw" ]
---

[Foreign data wrappers](https://wiki.postgresql.org/wiki/Foreign_data_wrappers) are extensions that can be engaged within [PostgreSQL](http://www.postgresql.org/) that allow you access to remote objects in other databases.

In today's post, I'm going to run through the basic method of gaining access to a table that sits in one PostgreSQL database from another.

### Commands

First of all, you need to install the **fdw** extension with the [CREATE EXTENSION](http://www.postgresql.org/docs/9.1/static/sql-createextension.html) command:

{% highlight sql %}
CREATE EXTENSION postgres_fdw;
{% endhighlight %}

Next, you need to make the target database (the database that you want to import data from) accessible to this database. You define a foreign server using the [CREATE SERVER](http://www.postgresql.org/docs/9.1/static/sql-createserver.html) command:

{% highlight sql %}
CREATE SERVER the_target_db
FOREIGN DATA WRAPPER postgres_fdw
OPTIONS (dbname 'the_target', host 'localhost');
{% endhighlight %}

This is going to address a database called `the_target` on the same host (because of `localhost`).

Next, you need to link up the locally running user to a remote user. This is done using the [CREATE USER MAPPING](http://www.postgresql.org/docs/9.1/static/sql-createusermapping.html) command.

{% highlight sql %}
CREATE USER MAPPING FOR local_user
SERVER the_target_db
OPTIONS (user 'remote_user', password 'password');
{% endhighlight %}

So this links up a local user called `local_user` with a remote user called `remote_user`.

These steps only need to be run once for each remote connection to be established.

### Get some data

To actually start writing some queries against the foreign data interface, you need to create the table using [CREATE FOREIGN TABLE](http://www.postgresql.org/docs/9.1/static/sql-createforeigntable.html). After you've done this, the foreign table will appear as a first-class, queryable object in your database.

{% highlight sql %}
CREATE FOREIGN TABLE "local_name_for_remote_table" (
   "id"   integer        NOT NULL,
   "name" varchar(50)    NOT NULL
) SERVER the_target_db OPTIONS (table_name 'some_remote_table');
{% endhighlight %}

So, this creates a table called `local_name_for_remote_table` which is latched up to `some_remote_table`.

And that's it.

