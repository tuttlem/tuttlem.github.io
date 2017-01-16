---
layout: post
title: Implicit values in Scala
date: 2017-01-16
comments: false
categories: [ "scala", "implicit" ]
---

[Scala](http://scala-lang.org/) has the concept of [implicit parameters](http://docs.scala-lang.org/tutorials/tour/implicit-parameters) which allows the developer to implicitly apply a value that has been previously defined. According to the documentation

> A method with _implicit parameters_ can be applied to arguments just like a normal method. In this case the implicit label has no effect. However, if such a method misses arguments for its implicit parameters, such arguments will be automatically provided.

## An example

We'll create a `Person` class that expects a first and last name. There's also a `manager` field to be supplied:

{% highlight scala %}
case class Person(firstName: String, lastName: String)
                 (implicit manager: Person = null)
{% endhighlight %}

Immediately, you can see that the `manager` parameter has been decorated with the `implicit` keyword. What this says is:

* First, any `Person` that can be accessed (at the point of construction) that has been declared `implicit`.
* Second, any `Person` also declared `implicit` in companion modules.

For demonstration purposes, a `showHierarchy` function has been created to show management:

{% highlight scala %}
def showHierarchy() = {

  print(s"$firstName $lastName is ")

  if (manager == null) {
    println("not managed by anybody")
  } else {
    val managerFirstName = manager.firstName
    val managerLastName = manager.lastName

    println(s"managed by $managerFirstName $managerLastName")
  }
}
{% endhighlight %}

When the `Person` has a manager, you'll see their name; otherwise this function will show that they are "not managed by anybody".

## Using the class

With all of this definition, we now take a look at usage. Note that the `manager` parameter has a default value of `null`. So, any invocation where an `implicit` can not be supplied still doesn't need to be specified as it's been defaulted.

{% highlight scala %}
val sam = Person("Sam", "Brown")
val joe = Person("Joe", "Smith")

sam.showHierarchy()
joe.showHierarchy()
{% endhighlight %}

This program shows the following output:

{% highlight text %}
Sam Brown is not managed by anybody
Joe Smith is not managed by anybody
{% endhighlight %}

Makes sense. `sam` wasn't declared as `implicit` and as such, wouldn't be offered in the construction of `joe`. So, we modify the construction of `sam` and add the keyword:

{% highlight scala %}
implicit val sam = Person("Sam", "Brown")
{% endhighlight %}

This has an immediate impact on the output of the program:

{% highlight text %}
Sam Brown is not managed by anybody
Joe Smith is managed by Sam Brown
{% endhighlight %}

As you can see, `sam` has now been implicitly offered to the construction of `joe` and as such, the `manager` parameter gets filled (in the construction of `joe`).

Handy.