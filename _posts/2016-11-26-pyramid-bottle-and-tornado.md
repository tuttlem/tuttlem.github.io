---
layout: post
title: Pyramid, Bottle and Tornado
date: 2016-11-26
comments: false
categories: [ "" ]
---

As web application developers, we're given a vast array of web application development frameworks at our disposal, In today's post, I'm going to go through three of these; all based on the [Python](https://www.python.org/) programming language. The frameworks are:

* [Pyramid](http://www.pylonsproject.org/)
* [Bottle](http://bottlepy.org/docs/dev/)
* [Tornado](http://www.tornadoweb.org/en/stable/)

These really are micro-frameworks for this purpose.

## Pyramid

Pyramid, or [the Pylons Project](http://www.pylonsproject.org/) is a straight-forward application framework where most of the focus is placed on the application's configuration. This isn't an ancillary file supplied to the application, but defined in code, in module. From the web site:

> Rather than focusing on a single web framework, the Pylons Project will develop a collection of related technologies. The first package from the Pylons Project was the Pyramid web framework. Other packages have been added to the collection over time, including higher-level components and applications. We hope to evolve the project into an ecosystem of well-tested, well-documented components which interoperate easily.

The Pylons project is a greater umbrella for the Pyramid-piece which is the web application framework.

Following is a "Hello, world" application using this framework.

{% highlight python %}
from wsgiref.simple_server import make_server
from pyramid.config import Configurator
from pyramid.response import Response

def hello_world(request):
  return Response('Hello %(name)s!' % request.matchdict)

if __name__ == '__main__':
  config = Configurator()
  config.add_route('hello', '/hello/{name}')
  config.add_view(hello_world, route_name='hello')
  app = config.make_wsgi_app()
  server = make_server('0.0.0.0', 8080, app)
  server.serve_forever()
{% endhighlight %}

The `Configurator` class holding a lot of the application's runtime, which is where routes and views come together.

## Bottle

[Bottle](http://bottlepy.org/docs/dev/) is a no-frills framework, with four main responsibilities: routing, templates, utilities and server.

It's actually quite amazing (from a minimalist's perspective) exactly how much you can get accomplished in such little code. Here's the "Hello, world" example from their site:

{% highlight python %}
from bottle import route, run, template

@route('/hello/<name>')
def index(name):
    return template('<b>Hello {{name}}</b>!', name=name)

run(host='localhost', port=8080)
{% endhighlight %}

The simplistic feel to the framework certainly makes it very clear. `template` providing a direct text template with a model. `run` performing the job of the server and the `@route` attribute performing route configuration.

They're faithful to their words:

> Bottle is a fast, simple and lightweight WSGI micro web-framework for Python. It is distributed as a single file module and has no dependencies other than the Python Standard Library.

## Tornado

[Tornado](http://www.tornadoweb.org/en/stable/) is a web application framework that has been based around event-driven I/O. It's going to be better suited to some of the persistent connection use-cases that some applications have (like long-polling or web sockets, etc). The following is from their site:

> Tornado is a Python web framework and asynchronous networking library, originally developed at FriendFeed. By using non-blocking network I/O, Tornado can scale to tens of thousands of open connections, making it ideal for long polling, WebSockets, and other applications that require a long-lived connection to each user.

In its own way, Tornado can also be quite minimalist. Here's their example:

{% highlight python %}
import tornado.ioloop
import tornado.web

class MainHandler(tornado.web.RequestHandler):
  def get(self):
    self.write("Hello, world")

def make_app():
  return tornado.web.Application([
      (r"/", MainHandler),
  ])

if __name__ == "__main__":
  app = make_app()
  app.listen(8888)
  tornado.ioloop.IOLoop.current().start()
{% endhighlight %}

Key difference on this particular framework is the involvement of the `IOLoop` class. This really is event-driven web programming.

