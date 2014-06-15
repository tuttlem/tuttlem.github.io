---
layout: post
title: Stored Javascripts in MongoDB
date: 2013-03-04
comments: false
categories: [ "MongoDB", "Javascript" ]
---

### Introduction

Coming from a relational database background, technologies such as stored procedures and user defined functions have always helped out when building a database infrastructure. MongoDB provides the same sort of code storage in stored javascripts in the database. 

### Creating a stored javascript

Creating a stored javascript into a database is a straight forward process of adding an item to the system.js collection.

{% highlight js %}
> db.collection.js.save({_id: "greaterThan10", 
...                      value: function (x) { return x &gt; 10 }});
{% endhighlight %}

Ok, this isn't the most useful of functions. We're testing if the value passed in the greater than 10. We're able to use this in queries of our own using `$where` syntax like so:

{% highlight js %}
> db.people.find({$where: "greaterThan10(this.age)"})
{% endhighlight %}

This would get all of the "people" documents out of the database where they were over the age of 10. This is quite verbose of such a simple example, but you can see that by filling out the function in the saved javascript with more complex operations, you could achieve a lot with a little.

### Removing a stored javascript

Working with the collection as usual, you can simply remove your stored javascript by id.

{% highlight js %}
> db.collection.js.remove({_id: "greaterThan10"})
{% endhighlight %}

### Testing

As a final note, once you've created your stored javascript you can test it using eval easy enough.

{% highlight js %}
> db.eval("return greaterThan10(9);")
false
> db.eval("return greaterThan10(11);")
true
{% endhighlight %}

This is just a short introduction into the world of stored javascripts. The internal workings of MongoDB is all based on javascript, so it's a good idea to have your skills nice and sharp before going in!