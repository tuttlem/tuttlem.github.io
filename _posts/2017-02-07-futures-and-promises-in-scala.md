---
layout: post
title: Futures and Promises in Scala
date: 2017-02-07
comments: false
categories: [ "future", "promise", "scala" ]
---

The wikipedia article for [Futures and promises](https://en.wikipedia.org/wiki/Futures_and_promises) opens up with this paragraph, which I thought is the perfect definition:

> In computer science, future, promise, delay, and deferred refer to constructs used for synchronizing program execution in some concurrent programming languages. They describe an object that acts as a proxy for a result that is initially unknown, usually because the computation of its value is yet incomplete.

In today's article, I'll walk you through the creation and management of the *future* and *promise* construct in the [Scala](https://www.scala-lang.org/) language.

## Execution context

Before continuing with the article, we need to make a special note about the `ExecutionContext`. Futures and promises both use the execution context to perform the execution of their computations. 

Any of the operations that you'll write out to start a computation requires an `ExecutionContext` as a parameter. These can be passed implicitly, so it'll be a regular occurrence where you'll see the following definition:

{% highlight scala %}
// define the implicit yourself
implicit val ec: ExecutionContext = ExecutionContext.global

// or - import one already defined
import ExecutionContext.Implicits.global
{% endhighlight %}

`ExecutionContext.global` is an `ExecutionContext` that is backed by a [ForkJoinPool](http://docs.oracle.com/javase/tutorial/essential/concurrency/forkjoin.html). 

## Futures

We create a `Future` in the following ways:

{% highlight scala %}
/* Create a future that relies on some work being done
   and that emits its value */
val getName = Future {
  // simulate some work here
  Thread.sleep(100)
  "John"
}

/* Create an already resolved future; no need to wait
   on the result of this one */
val alreadyGotName = Future.successful("James")

/* Create an already rejected future */
val badNews = Future.failed(new Exception("Something went wrong"))
{% endhighlight %}

With a future, you set some code in place to handle both the success and fail cases. You use the `onComplete` function to accomplish this:

{% highlight scala %}
getName onComplete {
  case Success(name) => println(s"Successfully got $name")
  case Failure(e) => e.printStackTrace()
}
{% endhighlight %}

Using a for-comprehension or map/flatMap, you can perform functional composition on your `Future` so that adds something extra through the pipeline. In this case, we're going to prefix the name with a message should it start with the letter "J":

{% highlight scala %}
val greeting = for {
  name <- getName
  if name.startsWith("J")
} yield s"Hello there, $name!"
{% endhighlight %}

## Blocking

If you really need to, you can make your future block.

{% highlight scala %}
val blockedForThisName = Future {
  blocking {
    "Simon"
  }
}
{% endhighlight %}

## Promises

The different between a `Future` and a `Promise` is that a future can be thought of as a read-only container. A promise is a single-assignment container that is used to complete a future.

Here's an example.

{% highlight scala %}
val getNameFuture = Future { "Tom" }
val getNamePromise = Promise[String]()

getNamePromise completeWith getNameFuture

getNamePromise.future.onComplete {
  case Success(name) => println(s"Got the name: $name")
  case Failure(e) => e.printStackTrace()
}
{% endhighlight %}

`getNamePromise` has a future that we access through the `future` member. We treat it as usual with `onComplete`. It knows that it needs to resolve because of the `completeWith` call, were we're telling `getNamePromise` to finish the `getNameFuture` future.

