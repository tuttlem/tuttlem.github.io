---
layout: post
title: Starting a microservice with Scala
date: 2017-02-17
comments: false
categories: [ "scala", "microservice", "http", "akka" ]
---

A microservice is an architectural pattern that allows your services to deployed in an isolated fashion. This isolation allows your service to remain focused on its problem (and only its problem) that its trying to solve, as well as simplify telemetry, instrumentation, and measurement metrics. From [Martin Fowler's](https://martinfowler.com/articles/microservices.html) site:

> The term "Microservice Architecture" has sprung up over the last few years to describe a particular way of designing software applications as suites of independently deployable services. While there is no precise definition of this architectural style, there are certain common characteristics around organization around business capability, automated deployment, intelligence in the endpoints, and decentralized control of languages and data.

If you want to learn more about microservices, seriously, [check out google](https://www.google.com.au/search?q=microservices). They're everywhere!

The purpose of today's article is to stand a microservice up in Scala, to get up and running quickly.

## Getting started

In a [previous article]({% post_url 2017-02-16-creating-a-scala-sbt-project-structure %}), I showed you how you can create a scala project structure with a shell script. We'll use that right now to create our project *microservice-one*.

{% highlight text %}
$ new-scala-project microservice-one
$ cd microservice-one
$ tree
.
├── build.sbt
├── lib
├── project
├── src
│   ├── main
│   │   ├── java
│   │   ├── resources
│   │   └── scala
│   └── test
│       ├── java
│       ├── resources
│       └── scala
└── target

12 directories, 1 file
{% endhighlight %}

## Dependencies

Now we'll need to sort out our dependencies.

We'll need [scalatest](http://www.scalatest.org/) for testing, [akka](http://akka.io) and [akka-http](http://doc.akka.io/docs/akka-http/current/index.html) to help us make our API concurrent/parallel as well as available over HTTP. Our `build.sbt` file should look like this:

{% highlight text %}
name := "microservice-one"
organization := "me.tuttlem"
version := "1.0.0"
scalaVersion := "2.12.1"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

libraryDependencies ++= {
  
  val akkaV       = "2.4.16"
  val akkaHttpV   = "10.0.1"
  val scalaTestV  = "3.0.1"
  
  Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaV,
    "com.typesafe.akka" %% "akka-stream" % akkaV,
    "com.typesafe.akka" %% "akka-testkit" % akkaV,
    "com.typesafe.akka" %% "akka-http" % akkaHttpV,
    "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpV,
    "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpV,
    "org.scalatest"     %% "scalatest" % scalaTestV % "test"
  )
  
}
{% endhighlight %}

Update our project now:

{% highlight bash %}
$ sbt update
{% endhighlight %}

## The code

We're going to dump everything into one file today; the main application object. All of the parts are very descriptive though and I'll go through each one. Our microservice is going to have one route, which is a *GET* on `/greeting`. It'll return us a simple message.

First up, we model how the message will look:

{% highlight scala %}
case class Greeting(message: String)
{% endhighlight %}

Using this `case class`, you'd expect messages to be returned that look like this:

{% highlight text %}
{ message: "Here is the message!" }
{% endhighlight %}

We tell the application how to serialize this data over the http channel using `Protocols`:

{% highlight scala %}
trait Protocols extends DefaultJsonProtocol {
  implicit val greetingFormat = jsonFormat1(Greeting.apply)
}
{% endhighlight %}

Now, we can put together our actual service implementation. Take a look specifically at the DSL that scala is provided for route definition:

{% highlight scala %}
trait Service extends Protocols {
  implicit val system: ActorSystem
  implicit def executor: ExecutionContextExecutor
  implicit val materializer: Materializer

  def config: Config
  val logger: LoggingAdapter

  val routes = {
    logRequestResult("microservice-one") {
      pathPrefix("greeting") {
      	get {
      		complete(Greeting("Hello to you!"))
      	}
      }
    }
  }

}
{% endhighlight %}

So, our one route here will constantly just send out _"Hello to you!"_.

Finally, all of this gets hosted in our main application object:

{% highlight scala %}
object MicroserviceOne extends App with Service {
  override implicit val system = ActorSystem()
  override implicit val executor = system.dispatcher
  override implicit val materializer = ActorMaterializer()

  override val config = ConfigFactory.load()
  override val logger = Logging(system, getClass)

  Http().bindAndHandle(routes, config.getString("http.interface"), config.getInt("http.port"))
}
{% endhighlight %}

That's it for the code. In the `src/main/resources` directory, we'll put a `application.conf` file that details a few configurations for us:

{% highlight text %}
akka {
	loglevel = DEBUG
}

http {
	interface = "0.0.0.0"
	port = 3000
}
{% endhighlight %}

## Running

Lets give it a run now.

{% highlight text %}
$ sbt run 
{% endhighlight %}

Once SBT has finished its dirty work, you'll be able to request your route at [http://localhost:3000/greeting](http://localhost:3000/greeting):

{% highlight text %}
$ curl -v http://localhost:3000/greeting
*   Trying 127.0.0.1...
* Connected to localhost (127.0.0.1) port 3000 (#0)
> GET /greeting HTTP/1.1
> Host: localhost:3000
> User-Agent: curl/7.47.0
> Accept: */*
> 
< HTTP/1.1 200 OK
< Server: akka-http/10.0.1
< Date: Thu, 16 Feb 2017 22:37:52 GMT
< Content-Type: application/json
< Content-Length: 27
< 
* Connection #0 to host localhost left intact
{"message":"Hello to you!"}
{% endhighlight %}

Perfect. 

That's all for today.