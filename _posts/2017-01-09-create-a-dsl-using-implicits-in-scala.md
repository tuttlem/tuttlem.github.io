---
layout: post
title: Create a DSL using implicits in Scala
date: 2017-01-09
comments: false
categories: [ "scala", "dsl", "implicit" ]
---

[Scala]() provides some simple options to get up and running the development of [domain specific languages](). In today's post, I'll take you through the usage of the [implicit]() keyword that easily allows for this to happen.

## End goal

For the purposes of today, I want to create a language that defines simple, directional instructions. "Move left", "move forward", etc. The end goal, I want a language that looks like this:

{% highlight text %}
var jump = 5.0 units up
var shuffle = -2.0 units left
var fall = -6000.0 units down
{% endhighlight %}

## Implicit conversion

The [implicit conversion](http://docs.scala-lang.org/tutorials/tour/implicit-conversions) will allow us to get this process started. It'll provide the ability for us to take that literal, double value (at the start of each of those statements), and convert them into a class instance that we can do more interesting things with.

{% highlight scala %}
implicit def convertDoubleToDirectionUtil(amount: Double) = new DirectionUtil(amount)
{% endhighlight %}
 
Ok, so we don't know what a `DirectionUtil` class is just yet. That's coming. Right now, it's important to focus on this function which is an implicit conversion; allows us to express a double-value like normal and have Scala treat it as a `DirectionUtil` instance.

## Implicit classes

The [implicit class](http://docs.scala-lang.org/overviews/core/implicit-classes.html) allows us to offer a class up to an [implicit conversion](http://docs.scala-lang.org/tutorials/tour/implicit-conversions) when it's in scope.

The definition of our `DirectionUtil` class here, allows us the `units` definition function.

{% highlight scala %}
implicit class DirectionUtil(val amount: Double) {

  def units(dir: String) = {
    dir match {
      case "forward" => scale((0.0, 0.0, 1.0), amount)
      case "backward" => scale((0.0, 0.0, -1.0), amount)
      case "left" => scale((-1.0, 0.0, 0.0), amount)
      case "right" => scale((1.0, 0.0, 0.0), amount)
      case "up" => scale((0.0, -1.0, 0.0), amount)
      case "down" => scale((0.0, 1.0, 0.0), amount)
      case _ => (0.0, 0.0, 0.0)
    }
  }

}
{% endhighlight %}

Really simply, this is just a vector-scaling function. Takes in a tuple, and adjusts it by a magnitude depending on the direction passed in.

## Wrapping it all up

To finish up, we'll put this class and implicit conversion into one neat singleton:

{% highlight scala %}
object DirectionUtil {
  val forward = "forward"
  val backward = "backward"
  val left = "left"
  val right = "right"
  val up = "up"
  val down = "down"

  def scale(vec: (Double, Double, Double), mag: Double) = {
    val (x, y, z) = vec
    (x * mag, y * mag, z * mag)
  }

  implicit def convertDoubleToDirectionUtil(amount: Double) = new DirectionUtil(amount)

  implicit class DirectionUtil(val amount: Double) {

    def units(dir: String) = {
      dir match {
        case "forward" => scale((0.0, 0.0, 1.0), amount)
        case "backward" => scale((0.0, 0.0, -1.0), amount)
        case "left" => scale((-1.0, 0.0, 0.0), amount)
        case "right" => scale((1.0, 0.0, 0.0), amount)
        case "up" => scale((0.0, -1.0, 0.0), amount)
        case "down" => scale((0.0, 1.0, 0.0), amount)
        case _ => (0.0, 0.0, 0.0)
      }
    }

  }
}
{% endhighlight %}

Now that we have this defined, we can start writing code that looks like this:

{% highlight scala %}
object Main extends App {
  import DirectionUtil._

  val jump = 5.0 units up
  println(jump)
}
{% endhighlight %}

. . which, as expected gives us an output like this:

{% highlight text %}
(0.0,-5.0,0.0)
{% endhighlight %}

## In closing

This is a really simple example, but you can see immediately how it can be applied to much more complex scenarios; and how you can be a lot more expressive in your scala source code.

