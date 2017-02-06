---
layout: post
title: Getting functional with Java 8
date: 2017-02-06
comments: false
categories: [ "java", "functional" ]
---

[Java 8](http://www.oracle.com/technetwork/java/javase/overview/java8-2100321.html) provides developers with [functional programming](https://en.wikipedia.org/wiki/Functional_programming) facilities that are very new to the language itself.

In today's post, I'll go through a primer of the different facilities that you can use.

## Functional Interfaces

Single abstract method interfaces have been taken a step further in Java 8, where the programmer is able to decorate their interface using a [@FunctionalInterface](http://www.java2s.com/Tutorials/Java_Lambda/Lambda_Tutorial/Lambda/FunctionalInterface_Annotation.htm) annotation. These can then be represented as [lambda expressions](https://en.wikipedia.org/wiki/Anonymous_function) and [method references](https://docs.oracle.com/javase/tutorial/java/javaOO/methodreferences.html). These are building blocks for functional programming.

### `Consumer<T>`

Represents an operation that accepts a single input argument and returns no result. Unlike most other functional interfaces, Consumer is expected to operate via side-effects.

* [accept](https://docs.oracle.com/javase/8/docs/api/java/util/function/Consumer.html#accept-T-) 
* [andThen](https://docs.oracle.com/javase/8/docs/api/java/util/function/Consumer.html#andThen-java.util.function.Consumer-)

Uses are passing to the [forEach](https://docs.oracle.com/javase/8/docs/api/java/lang/Iterable.html#forEach-java.util.function.Consumer-) function.

### `Supplier<T>` 

This is a functional interface and can therefore be used as the assignment target for a lambda expression or method reference.

* [get](https://docs.oracle.com/javase/8/docs/api/java/util/function/Supplier.html#get--)

Used in the creation of streams.

### `Predicate<T>` 

This is a functional interface and can therefore be used as the assignment target for a lambda expression or method reference.

* [test](https://docs.oracle.com/javase/8/docs/api/java/util/function/Predicate.html#test-T-) 
* [and](https://docs.oracle.com/javase/8/docs/api/java/util/function/Predicate.html#and-java.util.function.Predicate-) 
* [negate](https://docs.oracle.com/javase/8/docs/api/java/util/function/Predicate.html#negate--) 
* [or](https://docs.oracle.com/javase/8/docs/api/java/util/function/Predicate.html#or-java.util.function.Predicate-) 

Used with [filter](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html#filter-java.util.function.Predicate-)

### `Function<T, R>` 

This is a functional interface and can therefore be used as the assignment target for a lambda expression or method reference.

* [andThen](https://docs.oracle.com/javase/8/docs/api/java/util/function/Function.html#andThen-java.util.function.Function-) 
* [compose](https://docs.oracle.com/javase/8/docs/api/java/util/function/Function.html#compose-java.util.function.Function-) 

Used to pass to [map](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html#map-java.util.function.Function-) 

## Functional Syntax

### Defining a Functional Interface

{% highlight java %}
@FunctionalInterface
public interface TailCall<T> {
  
  TailCall<T> apply();

  default boolean isComplete() { return false; }

}
{% endhighlight %}

A functional interface must:

* Have one abstract, unimplemented method
* May have zero or more default or implemented methods
* May have static methods

## Lambda expressions

{% highlight java %}
// no parameters
() -> func(1)

// explicit definition of parameter types
(final String colour) -> System.out.println(colour)

// implicit parameter definition
(colour) -> System.out.println(colour)
colour -> System.out.println(colour)

// multiples
(x, y) -> x * y

// multi-line lambda
(x) -> {
  func1(x);
  func2(x);
}
{% endhighlight %}

## Composition, method references

{% highlight java %}
products.stream()
        .filter(Pricing::hasDiscount)
        .mapToDouble(Product::getUnitPrice)
        .average()
        .getAsDouble();
{% endhighlight %}

