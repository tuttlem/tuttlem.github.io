---
layout: post
title: Functors in Scala
date: 2017-02-05
comments: false
categories: [ "scala", "functor", "applicative", "monad" ]
---

Scala's type is very rich; even where constructs aren't well defined you can easily piece together anything that you need. In today's article, I'll take you through some different functor types as well as a small primer on variance.

## Variance 

Type variance is what we use to describe sub-class relationships in a class hierarchy. In scala we use the following notations to denote variances:

|           | Description                     | Notation |
|-----------|---------------------------------|----------|
| covariant | `C[T']` is a subclass of `C[T]` | `[+T]` |
| contravariant | `C[T]` is a subclass of `C[T']` | `[-T]` |
| invariant | `C[T]` and `C[T']` are not related | `[T]` |

## Functors

Covariant functors are what provide `map` or `fmap`:

{% highlight scala %}
trait Functor[F[_]] {
  def fmap[A, B](f: A => B): F[A] => F[B]
}
{% endhighlight %}

Contravariant functors provide `contramap`:

{% highlight scala %}
trait Contravariant[F[_]] {
  def contramap[A, B](f: B => A): F[A] => F[B]
}
{% endhighlight %}

Exponential functors are what provide `xmap`:

{% highlight scala %}
trait Exponential[F[_]] {
  def xmap[A, B](f: (A => B, B => A)): F[A] => F[B]
}
{% endhighlight %}

Applicative functors are what provide `apply` and `<*>`:

{% highlight scala %}
trait Applicative[F[_]] {
  def apply[A, B](f: F[A => B]): F[A] => F[B]
}
{% endhighlight %}

Monads provide `bind`, `flatMap` or `=<<`:

{% highlight scala %}
trait Monad[F[_]] {
  def flatMap[A, B](f: A => F[B]): F[A] => F[B]
}
{% endhighlight %}

Comonads provide `extend`, `coflatMap` and `<<=`:

{% highlight scala %}
trait Comonad[F[_]] {
  def coflatMap[A, B](f: F[A] => B): F[A] => F[B]
}
{% endhighlight %}


