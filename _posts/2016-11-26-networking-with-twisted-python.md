---
layout: post
title: Networking with Twisted Python
date: 2016-11-26
comments: false
categories: [ "python", "twisted", "event", "networking" ]
---

Network programming is a delicate mix of sending messages, waiting for events and reacting. [Twisted](https://twistedmatrix.com/trac/wiki) is a python library that aims to simplify this process. From their website:

> Twisted is an event-driven networking engine written in Python

Pretty straight forward.

## Echo Server

The first example (lifted directly from their website) is an Echo Server:

{% highlight python %}
from twisted.internet import protocol, reactor, endpoints

class Echo(protocol.Protocol):
    def dataReceived(self, data):
        self.transport.write(data)

class EchoFactory(protocol.Factory):
    def buildProtocol(self, addr):
        return Echo()

endpoints.serverFromString(reactor, "tcp:1234").listen(EchoFactory())
reactor.run()
{% endhighlight %}

The method `dataReceived` which is provided by the `Protocol` class is called by the reactor when a network event of interest presents itself to your program.

## HTTP

Out of the box, you're also given some tools to talk web actions. Again, lifted from the twisted website is an example web server:

{% highlight python %}
from twisted.web import server, resource
from twisted.internet import reactor, endpoints

class Counter(resource.Resource):
  isLeaf = True
  numberRequests = 0

  def render_GET(self, request):
    self.numberRequests += 1
    request.setHeader(b"content-type", b"text/plain")
    content = u"I am request #{}\n".format(self.numberRequests)
    return content.encode("ascii")

endpoints.serverFromString(reactor, "tcp:8080").listen(server.Site(Counter()))
reactor.run()
{% endhighlight %}

It's a pretty brute-force way to deal with assembling a web server, but it'll get the job done. The `render_GET` method of the `Resource` derived `Counter` class will perform all of the work when a `GET` request is received by the server.

## Chat Server

I'll finish up with some original content here, that is a PubSub example (which twisted website has an example of). 

Getting a leg up using the `LineReceiver` protocol as a base, really simplifies our implementation. This allows us little gems like `connectionMade`, `connectionLost` and `lineReceived` . . all pieces that you'd expect in a chat server:

{% highlight python %}
def connectionMade(self):
  '''When a connection is made, we'll assume that the client wants to implicitly join
     out chat server. They'll gain membership automatically to the conversation'''

  self.factory.clients.add(self)

def connectionLost(self):
  '''When a connection is lost, we'll take the client out of the conversation'''

  self.factory.clients.remove(self)
{% endhighlight %}

We use a really crude regular expression with some basic captures to pull apart the instruction sent by the client:

{% highlight python %}
# our very crude, IRC instruction parser
irc_parser = re.compile('/(join|leave|msg|nick) ([A-Za-z0-9#]*)(| .*)')
{% endhighlight %}

When receiving a line, we can respond back to the client; or we can broadcast to the portfolio of connections:

{% highlight python %}
def lineReceived(self, line):
  '''When a client sends a line of data to the server, it'll be this function that handles
     the action and re-acts accordingly'''

  matches = irc_parser.match(line)

  if matches == None:
    # send an error back (to this client only)
    self.sendLine('error: line did not conform to chat server requirements!')
  else:
    (act, obj, aux) = matches.groups()

    if act == 'join':
      self.broadcast(self.nick + ' has joined the channel ' + obj)
    elif act == 'leave':
      self.broadcast(self.nick + ' has left the channel ' + obj)
    elif act == 'nick':
      client_ip = u"<{}> ".format(self.transport.getHost()).encode("ascii")
      self.broadcast(client_ip + ' is changing nick to ' + obj)
      self.nick = obj
{% endhighlight %}

The only part left out here, is the `broadcast` method. Which is simply a for-loop:

{% highlight python %}
def broadcast(self, line):
  for client in self.factory.clients:
    client.sendLine(line)
{% endhighlight %}

Here's the full example:

{% highlight python %}
from twisted.internet import reactor, protocol, endpoints
from twisted.protocols import basic

import re

# our very crude, IRC instruction parser
irc_parser = re.compile('/(join|leave|msg|nick) ([A-Za-z0-9#]*)(| .*)')

class ChatProtocol(basic.LineReceiver):
  '''The chat server is responsible for maintaing all client connections along with
     facilitating communication between interested chat clients'''

  def __init__(self, factory):
    self.factory = factory

    self.channels = { }

  def connectionMade(self):
    '''When a connection is made, we'll assume that the client wants to implicitly join
       out chat server. They'll gain membership automatically to the conversation'''

    self.factory.clients.add(self)

  def connectionLost(self):
    '''When a connection is lost, we'll take the client out of the conversation'''

    self.factory.clients.remove(self)

  def lineReceived(self, line):
    '''When a client sends a line of data to the server, it'll be this function that handles
       the action and re-acts accordingly'''

    matches = irc_parser.match(line)

    if matches == None:
      # send an error back (to this client only)
      self.sendLine('error: line did not conform to chat server requirements!')
    else:
      (act, obj, aux) = matches.groups()

      if act == 'join':
        self.broadcast(self.nick + ' has joined the channel ' + obj)
      elif act == 'leave':
        self.broadcast(self.nick + ' has left the channel ' + obj)
      elif act == 'nick':
        client_ip = u"<{}> ".format(self.transport.getHost()).encode("ascii")
        self.broadcast(client_ip + ' is changing nick to ' + obj)
        self.nick = obj

  def broadcast(self, line):
    for client in self.factory.clients:
        client.sendLine(line)

class ChatFactory(protocol.Factory):
  def __init__(self):
      self.clients = set()

  def buildProtocol(self, addr):
      return ChatProtocol(self)

endpoints.serverFromString(reactor, "tcp:1234").listen(ChatFactory())
reactor.run()            
{% endhighlight %}

Writing networked servers couldn't be easier.