---
layout: post
title: Getting started with Berkeley DB
date: 2015-01-01
comments: false
categories: [ "Berkeley", "database", "c" ]
---

In today's post, I'm just going to gloss over some top level details in developing applications that use [Berkeley DB](http://www.oracle.com/us/products/database/berkeley-db/index.html).

### What is it?

The first line of the [Wikipedia article](http://en.wikipedia.org/wiki/Berkeley_DB) for Berkeley DB sums the whole story up pretty well, I think:

> Berkeley DB (BDB) is a software library that provides a high-performance embedded database for key/value data.

Pretty simple. Berkeley DB is going to offer you an in-process database to manage data in your applications in a little more organised approach.

### Getting installed

I'm using [Debian Linux](https://www.debian.org/), more specifically the [testing](https://www.debian.org/releases/jessie/) release. I'm sure the installation process will translate for other Debian releases and/or other Linux distributions with their respective package managers.

If you've already got a standard development/build environment running, you won't need the `build-essential` package listed below.

{% highlight bash %}
sudo apt-get install build-essential libdb-dev
{% endhighlight %}

### Building applications

Once you've finished writing an application using this library, you'll need to link the Berkeley DB library against your application. This is all pretty simple as well:

{% highlight bash %}
gcc -ldb yourapp.o -o yourapp
{% endhighlight %}

The key piece being the `-ldb` linker library switch.

### Some simple operations

The following blocks of code are heavily based off of the information contained in [this pdf](http://docs.oracle.com/cd/E17076_02/html/gsg/C/BerkeleyDB-Core-C-GSG.pdf). That pdf has a heap of information in it well worth the read if you're going to do something serious with Berkeley DB.

First thing you need to do, is to initialize the database structure that you'll use to conduct all of your operations.

{% highlight c %}
DB *db;
int ret;

/* setup the database memory structure */
ret = db_create(&db, NULL, 0);
{% endhighlight %}

`db_create` will allocate and fill out the structure of the `DB` typed pointer. It just sets it up ready for use. Once you've created the database handle, it's time to actually open up a database (or create one).

{% highlight c %}
ret = db->open(
	db, 
	NULL, 
	"test.db",
	NULL,
	DB_BTREE,
	DB_CREATE,
	0
);
{% endhighlight %}

`open` is going to try and find the requested database; (in my case `test.db`) and open it up. Failing that, it'll create it (because of `DB_CREATE`). The format parameter is quite interesting. In the sample above, `DB_BTREE` has been specified. Looking at the [DB->open() documentation](https://docs.oracle.com/cd/E17276_01/html/api_reference/C/dbopen.html):

> The currently supported Berkeley DB file formats (or access methods) are Btree, Hash, Queue, and Recno. The Btree format is a representation of a sorted, balanced tree structure. The Hash format is an extensible, dynamic hashing scheme. The Queue format supports fast access to fixed-length records accessed sequentially or by logical record number. The Recno format supports fixed- or variable-length records, accessed sequentially or by logical record number, and optionally backed by a flat text file. 

This gives you the flexability to use the format that suits your application best.

Once the database is open, writing data into it is as easy as specifying a key and value. There are some further data structures that need to be filled out, but the code is pretty easy to follow:

{% highlight c %}
/* source data values */
char *name = "John Smith";
int id = 5;

DBT key, value;

memset(&key, 0, sizeof(DBT));
memset(&value, 0, sizeof(DBT));

/* setup the key */
key.data = &id;
key.size = sizeof(int);

/* setup the value */
value.data = name;
value.size = strlen(name) + 1;

/* write it into the database */
ret = db->put(
	db, 
	NULL, 
	&key, 
	&value, 
	DB_NOOVERWRITE
);
{% endhighlight %}

As the `data` member of the `DBT` struct is typed out as `void *`, you can store any information you'd like in there. Of course, you must specify the `size` so the system knows how much to write.

Reading values back out into native variables is just as easy:

{% highlight c %}
/* destinations */
char read_name[256];
int read_id = 5;

memset(&key, 0, sizeof(DBT));
memset(&value, 0, sizeof(DBT));

/* setup the key to read */
key.data = &read_id;
key.size = sizeof(int);

/* setup the value to fill */
value.data = read_name;
value.ulen = 256;
value.flags = DB_DBT_USERMEM;

db->get(
	db, 
	NULL, 
	&key, 
	&value, 
	0
);
{% endhighlight %}

Finally, you'll want to close your database once you're done:

{% highlight c %}
if (db != NULL) {
	db->close(db, 0);
	db = NULL;
}
{% endhighlight %}

