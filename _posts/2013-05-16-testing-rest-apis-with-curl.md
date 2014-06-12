---
layout: post
title: Testing REST APIs with cURL
date: 2013-05-16
comments: false
---

### Introduction

Testing is a large component of any software development done, sometimes though - you don't want to go through a full unit test suite just to see what a REST service is doing. I've come across some interesting concepts with cURL that will certainly be a shortcut benefit to seeing what responses your REST services are returning.

### Requests

You can simulate all of the different HTTP verbs against any URL you'd like using `cURL` with the following syntax at the console:

{% highlight bash %}
# Retrieve person (id: 1)
$ curl -i -X GET http://localhost/service/people/1

# Retrieve all people
$ curl -i -X GET http://localhost/service/people

# Delete person (id: 1)
$ curl -i -X DELETE http://localhost/service/people/1

# Create a new person
$ curl -i -X POST -H 'Content-Type: application/json' -d '{"first_name": "John", "last_name": "Smith"}' http://localhost/service/people

# Modify a person (id: 1)
$ curl -i -X PUT -H 'Content-Type: application/json' -d '{"first_name": "Jane", "last_name": "Smith"}' http://localhost/service/people/1
{% endhighlight %}