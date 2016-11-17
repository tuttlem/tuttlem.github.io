---
layout: post
title: Compojure
date: 2016-11-17
comments: false
categories: [ "compojure", "clojure", "java", "httpkit" ]
---

In a [previous post]({% post_url 2016-11-16-simple-web-server-with-clojure %}), we setup a really simple route and server executing some [Clojure](http://clojure.org/) code for us. In today's post, we're going to use a library called [Compojure](https://github.com/weavejester/compojure) to *fancy-up* a little bit of that route definition.

This should make defining our web application a bit more fun, anyway.

## Getting started

Again, we'll use [Leiningen](http://leiningen.org/) to kick our project off:

{% highlight shell %}
lein new webapp-1
{% endhighlight %}

We're going to add some dependencies to the `project.clj` folder for `compojure` and `http-kit`. `http-kit` is the server that we'll be using today.

{% highlight clojure %}
(defproject webapp-1 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
  				 [compojure "1.1.8"]
  				 [http-kit "2.1.16"]])
{% endhighlight %}

And then, installation.

{% highlight shell %}
lein deps
{% endhighlight %}

## Hello!

To get started, we'll define a root route to greet us.

{% highlight clojure %}
(ns webapp-1.core
	(:require [compojure.core :refer :all]
			  [org.httpkit.server :refer [run-server]]))

(defroutes greeter-app
	(GET "/" [] "Hello!"))

(defn -main []
	(run-server greeter-app {:port 3000}))
{% endhighlight %}

A quick hit through `curl` lets us know that we're up and running:

{% highlight text %}
curl --verbose localhost:3000
* Rebuilt URL to: localhost:3000/
*   Trying 127.0.0.1...
* Connected to localhost (127.0.0.1) port 3000 (#0)
> GET / HTTP/1.1
> Host: localhost:3000
> User-Agent: curl/7.47.0
> Accept: */*
> 
< HTTP/1.1 200 OK
< Content-Type: text/html; charset=utf-8
< Content-Length: 6
< Server: http-kit
< Date: Thu, 17 Nov 2016 02:42:48 GMT
< 
* Connection #0 to host localhost left intact
Hello!
{% endhighlight %}

## Further links

Check out the following:

* [Ring](https://github.com/ring-clojure/ring)
* [Ring defaults](https://github.com/ring-clojure/ring-defaults)
* [Stencil](https://github.com/davidsantiago/stencil)
* [Selmer](https://github.com/yogthos/Selmer)
* [Hiccup](https://github.com/weavejester/hiccup)
* [Markdown](https://github.com/yogthos/markdown-clj)

Good fun.