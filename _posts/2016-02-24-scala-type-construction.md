---
layout: post
title: Scala type construction
date: 2016-02-24
comments: false
categories: [ "scala", "types", "trait", "class" ]
---

[Scala](http://scala-lang.org) gives the developer flexibility when reasoning about and designing type systems for applications. By using classes and traits, a developer can quickly build a complex hierarchy that can assist in describing constraint and relationship information.

In today's post, I'm going to walk through a *useless* but demonstrative example of a type hierarchy and some of the constraint features available to the developer.

### Vehicles

We're going to model some different vehicles. Cars, planes, trucks, skateboards, whatever.

{% highlight scala %}
abstract class Vehicle
{% endhighlight %}

We could start case-classing this base out or directly adding derivatives that specialise down to the exact vehicle types that we want, but we're going to reason about some attributes that these vehicles might have. Wheels and Jets.

{% highlight scala %}
trait HasWheels extends Vehicle {
  def numberOfWheels: Int
}

trait HasJets extends Vehicle {
  def numberOfJets: Int
}
{% endhighlight %}

When a vehicle `HasWheels`, the type is going to require us to specify `numberOfWheels`. Likewise `numberOfJets` for `HasJets`. These traits are extending our abstract `Vehicle` class.

When we have wheels, we should be able to set how fast they're spinning.

{% highlight scala %}
trait WheelPropulsion {
  this: HasWheels =>

  def setWheelSpin(velocity: Double) {
    println("Wheel spin: " + velocity)
  }
}
{% endhighlight %}

Our `WheelPropulsion` trait says that `this` needs to be `HasWheels`. Makes sense. We can't spin wheels if we don't have wheels.

Likewise, we'd want to set the turbine intensity if we have jets.

{% highlight scala %}
trait JetPropoulsion {
  this: HasJets =>

  def setTurbine(strength: Double) {
    println("Setting turbine strength to " + strength)
  }
}
{% endhighlight %}

Even with this very basic level of type description we can start to make some basic vehicles.

{% highlight scala %}
class Motorcycle extends Vehicle with HasWheels
                                 with WheelPropulsion {
  def numberOfWheels = 2                                  
}

class MotorVehicle extends Vehicle with HasWheels
                                   with WheelPropulsion {
  def numberOfWheels = 4
}

class TwinJetPlane extends Vehicle with HasJets
                                   with JetPropoulsion {
  def numberOfJets = 2                                  
}
{% endhighlight %}

### Mixing in

When you're assembling a variable of your own, there's no reason you can't mix in when creating your own types:

{% highlight scala %}
val toyota = new Vehicle() with HasWheels 
                           with WheelPropulsion { 
  def numberOfWheels = 4
}
{% endhighlight %}

Of course, we could have just constructed `toyota` as a `MotorVehicle` for the same effect. This just demonstrates the instance construction flexibility.

### Constraints

Finally, when you're writing functions that work with your types you can specify rich constraint rules so that you can target functionality with as much precision as you require:

{% highlight scala %}
// everything can be painted
def paint(v: Vehicle) = { }

// only a vehicle with wheels can burnout
def doBurnout(v: Vehicle with HasWheels) = { }
{% endhighlight %}

As you can see, you not only use the `with` keyword to define your types; this keyword is also used for variable construction and function signature definition.

