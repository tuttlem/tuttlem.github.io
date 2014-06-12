---
layout: post
title: A brief MapReduce tutorial for MongoDB
date: 2013-01-03
comments: false
---

### Introduction

This is just a short tutorial on using the [MapReduce](http://www.mongodb.org/display/DOCS/MapReduce) operation from within the [MongoDB](http://www.mongodb.org/) environment.

### What is it and how does it work?

MapReduce is a database operation. Its intended for performing higher-level (or more complex) aggregation tasks across huge data stores. You supply it with two functions, a map function and a reduce function and it will supply you with the aggregated result. As it's defined from the MongoDB documentation, the operation takes the following form:

{% highlight js %}
db.collection.mapReduce(
                         <mapfunction>,
                         <reducefunction>,
                         {
                         	out: <collection>,
                         	query: <document>,
                         	sort: <document>,
                         	limit: <number>,
                         	finalize: <function>,
                         	scope: <document>,
                         	jsMode: <boolean>,
                         	verbose: <boolean>
                         }
)
{% endhighlight %}


You can immediately see the first two parameters to this operation as the map function and the reduce function. The remaining parameter is focused on the delivery of the result back to you.

### The Map Function

The map function is responsible for defining the data that you want to work with. For instance, if you're interested in accumulating the sum of all products sold by their category you will want to return both the amount sold per line item as well as the product's category.

{% highlight js %}
var mapFunction = function() {
	emit(this.category, this.price);
};
{% endhighlight %}

Note the use of the `emit` function. Its use tells the MapReduce operation that the key for this process will be `category` and the value will be `price`. The value part of the `emit` function can also take form of a javascript object itself should you need to perform multiple aggregations across the same data set.

{% highlight js %}
var mapFunction = function() {
    emit(this.category, { price: this.price, count: 1 });
};
{% endhighlight %}

This particular map function gives us a count of 1 per result that comes out so we would be able to not only sum the price but find out how many items made up the sum as well.

### The Reduce Function

The reduce function is responsible for performing the aggregation on the data emitted by the map function. It is passed the key-values emitted by the map function as parameters to it. Performing the reduce function to get the sum of all prices and the number of items making up the sum total would look like this.

{% highlight js %}
var reduceFunction = function(key, values) {
    outValue = { total: 0, count: 0 };

    // aggregate all of the values for this key
    for (var i = 0; i < values.length; i ++) {
        outValue.total += values[i].price;
        outValue.count += values[i].count;
    }

    return outValue;
};
{% endhighlight %}

All of the "magic" happens via-javascript. Pretty easy really.

### Getting a result

Putting your map and reduce function together for the operation ends up looking like this (for a simple scenario).

{% highlight js %}
var res = db.sales.mapReduce(
    mapFunction, 
    reduceFunction, 
    { 
    	out: "result"
    }
);
{% endhighlight %}

This will get the process underway against the database. Once it's finished doing the hard-yards, then you can start basing your aggregation reports off of the "res" variable that we built just above by doing the following.

{% highlight js %}
db[res.result].find();
{% endhighlight %}

This is only a "scratch-the-surface" - "know enough to be dangerous" type write-up. MapReduce is a complex and powerful tool that you should be reading the official documentation about if you want to achieve ninja-like status.