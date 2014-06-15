---
layout: post
title: Using tailable cursors on the MongoDB oplog for realtime changes
date: 2014-06-14
comments: false
categories: [ "MongoDB", "tailable", "await", "data", "realtime", "cursor", "oplog" ]
---

[MongoDB](http://www.mongodb.org/) provides the ability to invoke to retrieve cursors of data that are tailable. 

We can exploit this functionality by using on the [oplog](http://docs.mongodb.org/manual/core/replica-set-oplog/) to provide a <em>trigger-like</em> effect on the Mongo database so that we can respond to changes in real-time. 

Using [pymongo](http://api.mongodb.org/python/current/) you can setup a connection to your mongo server's oplog like so:

{% highlight python %}
tail_opts = { 'tailable': True, 'await_data': True }

# connect to the target mongo server
mongo_url = 'mongodb://localhost:27017'
db = MongoClient(mongo_url).local

# get the latest timestamp in the database
last_ts = db.oplog.rs.find().sort('$natural', -1)[0]['ts'];

while True:
  # prepare the tail query and kick it off
  query = { 'ts': { '$gt': last_ts } }
  cursor = db.oplog.rs.find(query, **tail_opts)
  cursor.add_option(_QUERY_OPTIONS['oplog_replay'])

  try:
     while cursor.alive:
        try:
           # grab a document if available
           doc = cursor.next()
           
           # do something interesting with "doc"

        except StopIteration:
           # thrown when the cursor is out of data, so wait
           # for a period for some more data
           time.sleep(10)
  finally:
     cursor.close()
{% endhighlight %}

This constant feedback loop will just keep pumping results down the pipe as they're seen. You can already see that having an oplog setup on your database is a requirement of this solution. Without this, we have no way to measure the transactions that have executed.

The dictionary `tail_opts` is passed as the second argument to the `find` call. You can see that there are a couple of flags set here. The first one is `tailable`. `tailable` tells mongo that we want new results as they appear in scope of the cursor. `await_data` is another option that is set on the cursor to get the server to wait for data as it becomes available.  

According to 10gen:

> The sequence creates a cursor that will wait for few seconds after returning the full result set so that it can capture and return additional data added during the query

I have wrapped this functionality up into a server of its own (and client library) available from [my GitHub repo](https://github.com/tuttlem/mutated-mongo). <strong>mutated-mongo</strong> takes the idea in this article and filters out only messages that particular clients have subscribed to. It's still a work in progress.

