---
layout: post
title: Getting started with Akka
date: 2016-02-10
comments: false
categories: [ "akka", "scala" ]
---

[Akka](http://akka.io/) is a library designed for building applications using the [actor model](https://en.wikipedia.org/wiki/Actor_model). From their site:

> Akka is a toolkit and runtime for building highly concurrent, distributed, and resilient message-driven applications on the JVM.
 
In today's post, I'm going to start with some of the primitives to using this framework.

### Messages

Actors process messages that you'll define in your modules. For today's example, I'm going to implement a very basic logging application. Messages sent into this system are expected to be logged out to the console. To start off, we define the messages for this system:

{% highlight scala %}
case object Log
case class LogMessage(when: Date, level: String, text: String)
case class LogString(message: String)
case class LogException(e: Exception)
{% endhighlight %}

Using scala's [case classes](http://docs.scala-lang.org/tutorials/tour/case-classes.html) we can clean up the definition of these log messages. We have a message that will do general logging `LogMessage`, one that will log a string in `LogString` and one that will dissect and log out an exception object `LogException`.

### Actor Logic

We now focus on the logic required to log information out from our actor. This is really quite simple; we're just going to push everything out to the console:

{% highlight scala %}
class LogActor extends Actor {

  def receive = {
    case LogMessage(when, level, text) => println(String.format("%s [%s] %s", when.toString(), level, text))
    case LogString(message) => self ! LogMessage(new Date, "info", message)
    case LogException(e) => self ! LogMessage(new Date, "error", e.toString())
  }

}
{% endhighlight %}

The `receive` method is just a big [pattern matching](http://docs.scala-lang.org/tutorials/tour/pattern-matching.html) statement. Each of the message types are handled in here. Note how `LogString` and `LogException` send messages to `self`. `self` is a built-in, given to us representing **this** actor. All we're doing is just on-forwarding the message in the **string** and **exception** cases.

### Creating a system

We have actors; we have messages to pass between the actors; we now need a system that the actors will participate in.

{% highlight scala %}
// create the system
val system = ActorSystem("myLoggingSystem")

// create an actor
val logger = system.actorOf(Props[LogActor], "logger")
{% endhighlight %}

Using the `tell` and `ask` methods, we can send and send/receive messages to/from this actor. We also can create a logic-less actor that just acts as a message sender/receiver:

{% highlight scala %}
val inbox = Inbox.create(system)
{% endhighlight %}

[Mailboxes](http://doc.akka.io/docs/akka/current/scala/mailboxes.html) are an important abstraction; they hold messages for actors. Each actor has its own mailbox, but we've created one above attached to a system that we can pipe messages into:

{% highlight scala %}
inbox.send(logger, LogString("This is the first line of log"))
inbox.send(logger, LogException(new Exception("DOH!")))
{% endhighlight %}

### Lots of Actors

A slightly more complex topic is to create a pool of actors. In this next snippet, we'll create a [RoundRobinPool](http://doc.akka.io/japi/akka/2.3.8/akka/routing/RoundRobinPool.html).

{% highlight scala %}
val actors = system.actorOf(Props[LogActor].withRouter(RoundRobinPool(5)), name = "LoggingActors")
{% endhighlight %}

Now that we've created a pool, it's time to **smash**!

{% highlight scala %}
Range(1, 1000000).map(i => actors ! LogString(String.format("Message number %s", i.toString())))
{% endhighlight %}

### Scheduled

Finally, we can schedule these messages to be sent . . as if they were sent from no where using the actor system that we'd created earlier:

{% highlight scala %}
system.scheduler.schedule(0.seconds, 1.second, actors, LogString("Yerrr!"))(system.dispatcher, Actor.noSender)
{% endhighlight %}

This will send a `LogString` message to the actor system `actors` after zero seconds and then another message every second there after.

