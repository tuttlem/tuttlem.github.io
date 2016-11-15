---
layout: post
title: Simple web server with Clojure
date: 2016-11-15
comments: false
categories: [ "jetty", "clojure" ]
---

In today's post, we're going to use the _Clojure HTTP server abstraction_ called [ring](https://github.com/ring-clojure/ring) to stand a web server up, and attach some some routes. This allows us to expose our Clojure functions over the web in a relatively simple fashion.

## Getting started

This blog post is mainly centered around the getting started guide from the [ring](https://github.com/ring-clojure/ring) documentation pages, found [here](https://github.com/ring-clojure/ring/wiki/Getting-Started). 

We're going to get started by creating a project using [lein](http://leiningen.org/).

{% highlight shell %}
lein new jetty-test
{% endhighlight %}

After this process finishes, you'll end up with a directory called `jetty-test` that has a project structure something like this:

{% highlight text %}
.
├── CHANGELOG.md
├── doc
│   └── intro.md
├── LICENSE
├── project.clj
├── README.md
├── resources
├── src
│   └── jetty_test
│       └── core.clj
└── test
    └── jetty_test
        └── core_test.clj
{% endhighlight %}

## Dependencies

Now we need to make our newly created project *depend* on [ring](https://github.com/ring-clojure/ring). We need to add references to `ring-core` and `ring-jetty-adapter` in the `project.clj` file. So it should read something like this:

{% highlight clojure %}
(defproject jetty-test "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
  				 [ring/ring-core "1.5.0"]
  				 [ring/ring-jetty-adapter "1.5.0"]])
{% endhighlight %}

We can now install these dependencies into the project.

{% highlight shell %}
lein deps
{% endhighlight %}

## Server code

We can start writing our route code now that the server will respond to. We'll define a function that simply returns the current date and time:

{% highlight clojure %}
(defn now [] (java.util.Date.))
{% endhighlight %}

We'll also create a route that will use this function, and send back the text each time the route is requested:

{% highlight clojure %}
(defn current-time [request]
	{:status 200
	 :headers {"Content-Type" "text/plain"}
	 :body (str (now))})
{% endhighlight %}

That's it for the server code. We still need to fire up Jetty and attach the handler to it. We need to import `ring.adapter.jetty` as it contains `run-jetty` for us:

{% highlight clojure %}
(use 'ring.adapter.jetty)

(run-jetty current-time {:port 3000})
{% endhighlight %}

## Running

We run our project using `lein`:

{% highlight text %}
lein run 
{% endhighlight %}

Our output now looks something like this:

{% highlight text %}
. . .
. . .
. . .

Retrieving clj-time/clj-time/0.11.0/clj-time-0.11.0.jar from clojars
Retrieving ring/ring-core/1.5.0/ring-core-1.5.0.jar from clojars
Retrieving ring/ring-servlet/1.5.0/ring-servlet-1.5.0.jar from clojars
Retrieving clojure-complete/clojure-complete/0.2.4/clojure-complete-0.2.4.jar from clojars
2016-11-15 22:34:11.551:INFO::main: Logging initialized @877ms
2016-11-15 22:34:11.620:INFO:oejs.Server:main: jetty-9.2.10.v20150310
2016-11-15 22:34:11.646:INFO:oejs.ServerConnector:main: Started ServerConnector@795f253{HTTP/1.1}{0.0.0.0:3000}
2016-11-15 22:34:11.647:INFO:oejs.Server:main: Started @973ms
{% endhighlight %}

. . . suggesting that our server is ready to take requests. We can use `curl` to test it out for us:

{% highlight text %}
$ curl -v http://localhost:3000/

*   Trying 127.0.0.1...
* Connected to localhost (127.0.0.1) port 3000 (#0)
> GET / HTTP/1.1
> Host: localhost:3000
> User-Agent: curl/7.47.0
> Accept: */*
> 
< HTTP/1.1 200 OK
< Date: Tue, 15 Nov 2016 22:36:34 GMT
< Content-Type: text/plain; charset=ISO-8859-1
< Content-Length: 28
< Server: Jetty(9.2.10.v20150310)
< 
* Connection #0 to host localhost left intact
Tue Nov 15 22:36:34 UTC 2016
{% endhighlight %}

That's it. Pretty simple.
