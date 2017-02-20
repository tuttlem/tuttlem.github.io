---
layout: post
title: Actors in Scala
date: 2017-02-20
comments: false
categories: [ "actor", "scala", "akka" ]
---

The [actor model](https://en.wikipedia.org/wiki/Actor_model) is a software pattern that has been developed to make concurrent programming easier by promoting a lack of shared state. From the wikipedia article:

> The actor model in computer science is a mathematical model of concurrent computation that treats "actors" as the universal primitives of concurrent computation. In response to a message that it receives, an actor can: make local decisions, create more actors, send more messages, and determine how to respond to the next message received. Actors may modify private state, but can only affect each other through messages (avoiding the need for any locks).

In today's article, I'll show you a couple of primitive examples demonstrating the [Akka](http://akka.io/) framework using [Scala](https://www.scala-lang.org/).

## Basic setup

Before starting, you'll need to make your application depend on the [Akka](http://akka.io/) libraries; my `build.sbt` looks as follows:

{% highlight plain %}
name := "actor-basic"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies ++= {

  val akkaVersion = "2.4.17"

  Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaVersion
  )
}
{% endhighlight %}

Only needed to add `akka-actor`. There are a whole host of different sub-libraries all providing their own piece of extra functionality.

## Testing primes

In today's example, we're going to make an `Actor` that tests prime numbers. The code for the `isPrime` function below has been lifted from [here](https://www.scala-lang.org/old/node/230.html). Seems to do the job nicely. 

{% highlight scala %}
case class PotentialPrime(n: Integer)

class PrimeTester extends Actor {
  def receive = {
    case PotentialPrime(n) => println(s"prime: ${isPrime(n)}")
  }

  def isPrime(n: Int) = (2 until n) forall (n % _ != 0)
}
{% endhighlight %}

The first class here, `PotentialPrime` is a message class. It's the class that will hold the information used as input for the Actor to do something. In this case, we're carrying a number that could be a potential prime. This is then received by the `PrimeTester` actor in the `receive` method. You can see that we pattern match for the message type, in this case `PotentialPrime` to start prime testing.

Note that this is one-way. No information is sent back to the caller or to the actor system. The information being passed, and state remains within the actor.

We then setup a small system, an actor and pass it a message:

{% highlight scala %}
object ActorTest extends App {
  val system = ActorSystem("actor-testing")
  val actor1 = system.actorOf(Props[PrimeTester], name="prime-tester-actor")

  actor1 ! PotentialPrime(21)

  system.terminate()
}
{% endhighlight %}

We create the `ActorSystem` and then create an actor within that system using `actorOf`. The `!` means “fire-and-forget”. This will send a message asynchronously and return immediately. This function is also known as `tell`.

We run this application, and as expected:

{% highlight plain %}
prime: false
{% endhighlight %}

## Finding an actor

In a system, you can also find existing actors using their path. Like a file system where you have a hierarchical system of directories and files, actors also have parent/child relationships. In the example above, we would be able to find `actor1` by its path should we use the following:

{% highlight scala %}
val path = system / "prime-tester-actor"
val actorRef = system.actorSelection(path)

actorRef ! PotentialPrime(19)
{% endhighlight %}

## Actors

There are some important pieces to the Actor API that will give you a much finer level of control over your actors.

You can use `unhandled` to define the behavior of your actor when it receives a message that did not get handled. 

{% highlight scala %}
override def unhandled(message: Any): Unit = {
  println("Unhandled message encountered")
}
{% endhighlight %}

`self` is an `ActorRef` that can be used by the actor to send itself messages.

`sender` is the `ActorRef` and `context` provides `ActorContext` telling you the current message and current actor.

`supervisorStrategy` defines the strategy that's undergone when a failure occurs. It can be overridden.

`preStart`, `preRestart`, `postStop` and `postRestart` are all hook functions that you can tap into to add functionality.

## Feedback

Sending information back to the sender is pretty easy. It's a matter of bundling the information you need to send, into a message and sending. Adapting the primes example above a little more, the actor code changes just slightly:

{% highlight scala %}
class PrimeTester extends Actor {
  def receive = {
    case PotentialPrime(n) => sender ! isPrime(n)
  }

  def isPrime(n: Int) = (2 until n) forall (n % _ != 0)
}
{% endhighlight %}

Rather than just printing something out now, we're sending a message back to `sender`.

When we `ask` or `?` an actor for some information back, we don't immediately receive the result. We receive a `Future` that will give us the result once it's ready. So, the calling code becomes a trivial `Future` example:

{% highlight scala %}
val system = ActorSystem("actor-testing")
val actor1 = system.actorOf(Props[PrimeTester], name="prime-tester-actor")

implicit val timeout: Timeout = Timeout(Duration.create(5, TimeUnit.SECONDS))
implicit val ec: ExecutionContext = system.dispatcher

val future = actor1 ? PotentialPrime(21)

val result = future onComplete {
  case Success(b) => println(s"Result was ${b}")
  case Failure(e) => e.printStackTrace()
}

system.terminate()
{% endhighlight %}

You can simplify this further by using `Await`:

{% highlight scala %}
val system = ActorSystem("actor-testing")
val actor1 = system.actorOf(Props[PrimeTester], name="prime-tester-actor")
implicit val timeout = Timeout(Duration.create(5, TimeUnit.SECONDS))

val future = actor1 ? PotentialPrime(21)
val result = Await.result(future, timeout.duration)

println(s"Result is ${result}")

system.terminate()
{% endhighlight %}

That enough acting for today.
