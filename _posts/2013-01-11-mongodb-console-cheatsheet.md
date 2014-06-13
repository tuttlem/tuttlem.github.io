---
layout: post
title: MongoDB console cheatsheet
date: 2013-01-11
comments: false
---

### Basic retrieves

Find all items in a collection

{% highlight js %}
	db.collection.find();
{% endhighlight %}

Count all items in a collection

{% highlight js %}
	db.collection.count();
{% endhighlight %}

Find all items in a collection with criteria (Mongo [operators](http://docs.mongodb.org/manual/reference/operators/) reference)

{% highlight js %}
	db.collection.find({"field": value});
{% endhighlight %}

Reduce the field-set returned by a query

{% highlight js %}
	db.collection.find({}, {"field": 1});
{% endhighlight %}

Sorting by a field

{% highlight js %}
	db.collection.find().sort({"field": 1});
{% endhighlight %}

Limit the number of records returned by a query

{% highlight js %}
	db.collection.find().limit(3);
{% endhighlight %}

Skip over a set of documents

{% highlight js %}
	db.collection.find().skip(3);
{% endhighlight %}

### List processing

Map over a cursor of results

{% highlight js %}
	db.collection.find().map(function(item) {
	    return item;
	});
{% endhighlight %}

Iterate over a cursor of results

{% highlight js %}
	db.collection.find().forEach(function(item) {
	    printjson(item);
	});
{% endhighlight %}

### Updates

Update over an enumerated list

{% highlight js %}
	db.collection.find().forEach(function(item) {
	    // make changes to item
	    db.collection.save(item);
	});
{% endhighlight %}

Update from a query (Mongo [update](http://docs.mongodb.org/manual/applications/update/) reference)

{% highlight js %}
	db.collection.update({"_id": ObjectId("xxxx")}, 
	                     { $set: { "field": value },
	                       $push: { "subdocument": subdocument } 
	                     });
{% endhighlight %}

### Deleting

Destroy an entire collection

{% highlight js %}
	db.collection.drop();
{% endhighlight %}

### Delete a document selectively

{% highlight js %}
	db.collection.remove({"_id": ObjectId("xxxx"));
{% endhighlight %}

### Utilities

Set the record limit returned to the console

{% highlight js %}
	DBQuery.shellBatchSize = 100
{% endhighlight %}
