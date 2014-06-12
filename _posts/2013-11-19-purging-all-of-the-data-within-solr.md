---
layout: post
title: Purging all of the data within Solr
date: 2013-11-19
comments: false
---

During the test phases of getting your software setup, you'll find it useful to completely toast what ever data you've already indexed to start fresh. This is as simple as issuing a delete query with open criteria `*.*`. The full query should translate to 

{% highlight text %}
<delete><query>*.*</query></delete>
{% endhighlight %}

As a URL it'll look like this:

{% highlight text %}
http://[your solr server]:8080/solr/update?stream.body=%3Cdelete%3E%3Cquery%3E*:*%3C/query%3E%3C/delete%3E&commit=true
{% endhighlight %}

Note that there is a commit at the end of this URL which will perform the delete and commit the result all in the one invocation.
