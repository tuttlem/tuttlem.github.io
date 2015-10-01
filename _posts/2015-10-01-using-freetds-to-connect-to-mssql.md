---
layout: post
title: Using FreeTDS to connect to MSSQL
date: 2015-10-01
comments: false
categories: [ "odbc", "freetds", "mssql", "pyodbc" ]
---

In today's post, I'll outline the steps required to connect to a [Microsoft Sql Server](https://en.wikipedia.org/wiki/Microsoft_SQL_Server) database from within an [Ubuntu Linux](http://www.ubuntu.com/) environment using [FreeTDS](http://www.freetds.org/) and [ODBC](http://www.unixodbc.org/).

### Get the software

Using apt-get, we can satisfy all of the system-level requirements (libraries):

{% highlight bash %}
sudo apt-get install freetds-dev freetds-bin unixodbc-dev tdsodbc
{% endhighlight %}

FreeTDS now needs to be defined as a driver in the `/etc/odbcinst.ini` file. 

{% highlight text %}
[FreeTDS]
Description=FreeTDS Driver
Driver=/usr/lib/odbc/libtdsodbc.so
Setup=/usr/lib/odbc/libtdsS.so
{% endhighlight %} 

### Hitting the can

Now that we've got a driver up and running, we can use a library like [sqlalchemy](http://www.sqlalchemy.org/) to run some queries. Before we can do that though, we need to install python's odbc bindings [pyodbc](https://github.com/mkleehammer/pyodbc).

{% highlight bash %}
pip install pyodbc sqlalchemy
{% endhighlight %}

We can now start running some queries.

{% highlight python %}
import urllib
import sqlalchemy as sa

cstr = urllib.quote_plus('DRIVER=FreeTDS;SERVER=host;PORT=1433;DATABASE=db;UID=user;PWD=password;TDS_Version=8.0;')

engine = sa.create_engine('mssql+pyodbc:///?odbc_connect=' + cstr)
    
for row in engine.execute('SELECT 1 AS Test;'):
    print row.Test
{% endhighlight %}

