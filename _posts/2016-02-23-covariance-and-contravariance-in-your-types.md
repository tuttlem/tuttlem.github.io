---
layout: post
title: Covariance and contravariance in your types
date: 2016-02-23
comments: false
categories: [ "variance", "scala" ]
---

When creating parameterised types, you have control on how those types can be passed. These nuances are referred to as *variance* and scala allows you to explicitly nominate how this works in your own classes. 

An excellent explanation on these terms can be found [here](http://stackoverflow.com/a/8482091). I've reproduced the three main points for this article though:

> That is, if `A` and `B` are types, `f` is a type transformation, and `≤` the subtype relation (i.e. `A ≤ B` means that `A` is a subtype of `B`), we have:
>
> * `f` is covariant if `A ≤ B` implies that `f(A) ≤ f(B)`
> * `f` is contravariant if `A ≤ B` implies that `f(B) ≤ f(A)`
> * `f` is invariant if neither of the above holds

### Invariant

**Invariant** parameter types are what ensures that you can only pass `MyContainer[Int]` to `def fn(x: MyContainer[Int])`. The guarantee is that the type that you're containing (when it's being accessed) is done so as the correct type.

{% highlight scala %}
class MyInvariant[T](var value: T)
{% endhighlight %}

This guarantees the type of `T` when we go to work on it.

{% highlight scala %}
def double(a: MyInvariant[Int]) = { 
  a.value *= 2
}
{% endhighlight %}

You can see here that a good case for **invariant** is for **mutable** data.

To show the error case here, we define a `show` function specialising to `MyInvariant[Any]`

{% highlight scala %}
def show(a: MyInvariant[Any]) = { 
  println("Here is: " + a.value) 
}
{% endhighlight %}

Trying to use this function:

{% highlight text %}
scala> show(new MyInvariant[Int](5))
<console>:13: error: type mismatch;
 found   : MyInvariant[Int]
 required: MyInvariant[Any]
Note: Int <: Any, but class MyInvariant is invariant in type T.
You may wish to define T as +T instead. (SLS 4.5)
       show(new MyInvariant[Int](5))
            ^
{% endhighlight %} 

### Covariant

**Covariant** parameter type is specific. You pass these sorts of types to functions that generalise their inner type access. You need to decorate the type parameter with a `+`.

{% highlight scala %}
class CovariantContainer[+T](var value: T)
{% endhighlight %}

Then your function to generalise over this type:

{% highlight scala %}
def show(a: CovariantContainer[Any]) = { 
  println("The value is " + a.value)
}
{% endhighlight %}

**Covariance** is a good case for **read-only** scenarios.

### Contravariant 

**Contravariance** is defined by decorating the type parameter with a `-`. It's useful in **write-only**  situations.

{% highlight scala %}
class ContravariantContainer[-T](var value: T)
{% endhighlight %}

We write specialised functions for the type, but that are write-only cases:

{% highlight scala %}
def write(a: ContravariantContainer[String]) = {
  println("Writing " + a)
}
{% endhighlight %}

### Rules

When designing types, the following rules are very important when dealing with parameterization of types. 

* Mutable containers should be invariant
* Immutable containers should be covariant
* Transformation inputs should be contravariant
* Transformation outputs should be covariant

### Modeling a function call

Armed with this information, we can generalise function execution into the following type:

{% highlight scala %}
trait Fn[-In, +Out] {
  def apply(i: In): Out
}
{% endhighlight %}

Defining this trait, allows us to generalise the computation of an input to an output like the following:

{% highlight scala %}
val anyToInt = new Fn[Any, Int] {
  def apply(i: Any) = i.toString.toInt
}
{% endhighlight %}

