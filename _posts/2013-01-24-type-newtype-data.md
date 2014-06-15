---
layout: post
title: type, newtype & data
date: 2013-01-24
comments: false
categories: [ "Haskell", "type", "newtype", "Data" ]
---

### Introduction

This post is just a small write up on the differences between these three keywords and their uses within Haskell.

### type

Using the `type` keyword is the same as just referring to the actual type that you've declared. In other words, `type` just lets you make a synonym for the original type.

{% highlight haskell %}
-- a deck is an array of cards
type Deck = [Card]

-- an array of crows would be a murder
type Murder = [Crow]
{% endhighlight %}

Using these, we'll be able to refer to a `Card` list as a `Deck`. They can be used interchangeably as they'll directly cast. All this really does for us, is gives a pre-existing type a more meaningful name to our code.

### newtype

Using the `newtype` keyword, we make a thin wrapper around an existing type. Something that will be treated differently at compile time, but will be directly translatable (and not subject to conversion) at runtime.

{% highlight haskell %}
-- declare the data type
newtype Word = Word { getWord :: String }
  derives (Show, Eq)

-- using the data type
let helloWord = Word "Hello"
let byeWord = Word "Bye"
let greetingWord = Word "Hello"
{% endhighlight %}

`newtype` only allows you one constructor and one field. It's important that its used when you're creating data entries that have these constraints on them. These attributes make `newtype` a great candidate for when you just want to add a typeclass instance to an existing type.

### data

The `data` keyword allows you to build much more complex types. With the `data` keyword, you can have as many constructors and fields as you like.

{% highlight haskell %}
-- create a person data type
data Person = Person String String Int deriving (Show)
let john = (Person "John" "Smith" 35)

-- create a more complex employee type
data Employee = Employee Int Person deriving (Show)
let john = (Employee 6 (Person "John" "Smith" 35))
{% endhighlight %}

Of course, it would make more sense to use "record syntax" when defining these datatypes above.

{% highlight haskell %}
-- define a person
data Person = Person {firstName :: String
                     ,lastName :: String
                     ,age :: Int} deriving (Show)

-- define an employee
data Employee = Employee {employeeId :: Int
                         ,person :: Person} deriving (Show)

-- define "john"
let john = Employee {employeeId = 6
                    ,person = (Person {firstName = "John"
                                      ,lastName = "Smith"
                                      , age = 35}) 
                    }
{% endhighlight %}

### Wrapping up

* Use `type` to give your types more meaningful names
* Use `newtype` if you just want to take an existing type and add a typeclass instance to it
* Use `data` if you want to create your own datatype
