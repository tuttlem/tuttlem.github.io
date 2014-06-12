---
layout: post
title: Derived Instances for Types in Haskell
date: 2013-01-02
comments: false
---

### Introduction

When constructing your own types in Haskell, you can make your type support a particular behavior by making it an instance of the behavioral type class required. I'll walk through each of these derivable behaviours and how they can help.

### [Eq](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html#t:Eq)

`Eq` gives your type a sense of equality amongst values of the same type. It allows you to use the `==` operator as it was intended, returning you a boolean.

{% highlight haskell %}
data Employee = Employee { firstName :: String
                         , lastName :: String
                         , department :: String 
                         } deriving (Eq)

let mary = Employee { firstName = "Mary", lastName = "Jones", department = "Finance" }
let johnIT = Employee { firstName = "John", lastName = "Smith", department = "IT" }
let johnHR = Employee { firstName = "John", lastName = "Smith", department = "HR" }

-- would be False
mary == johnIT

-- would be True
johnIT /= johnHR

-- would be True
johnHR == Employee { firstName = "John", lastName = "Smith", department = "HR" }
{% endhighlight %}

From now on, `==` will do a comparison on the contents of the three strings in the Employee record for us.

### [Show](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html#t:Show)

In my personal experience when defining types, I would be out of my mind not to make them derive `Show`. `Show` allows a value of your type to be put into string format - very useful for debug situations.

{% highlight haskell %}
data Employee = Employee { firstName :: String
                         , lastName :: String
                         , department :: String 
                         } deriving (Show)
 
let mary = Employee { firstName = "Mary", lastName = "Jones", department = "Finance" }

-- Will print "Employee { firstName = "Mary", lastName = "Jones", department = "Finance" }"
putStr $ (show mary)
{% endhighlight %}

Just for the printing value, you can see how `Show` is worth its weight in gold.

### [Read](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html#t:Read)

`Read` provides the reverse-service of what `Show` does. You'll be able to take a type in its serialized format and re-construct a type from it. Again, rather useful in debug situations.

{% highlight haskell %}
data Employee = Employee { firstName :: String
                         , lastName :: String
                         , department :: String 
                         } deriving (Show, Read)
 
let maryStr = "Employee { firstName = \"Mary\", lastName = \"Jones\", department = \"Finance\" }"
 
-- mary will now be a constructed Employee 
let mary = read $ maryStr :: Employee
{% endhighlight %}

I've also used this to do user-input rather cheaply. Probably not quite a "production solution" though having your users enter type data directly.

### [Ord](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html#t:Ord)

`Ord` gives your value order. Once you apply `Ord` you can sort or use operators like `>`, `<`, `>=`, `<=`.

{% highlight haskell %}
data CardValue = Ace | Two | Three | Four | Five
               | Six | Seven | Eight | Nine | Ten
               | Jack | Queen | King
  deriving (Ord)

-- returns true
Four < Nine

-- returns GT
Three `compare` Two
{% endhighlight %}

Quite useful for when you need to do these sorts of comparisons.

### [Bounded](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html#t:Bounded)

`Bounded` will give your type a sense of the lowest and highest values achievable. You'll be able to ask questions of the type to see what these corresponding values are.

{% highlight haskell %}
data CardValue = Ace | Two | Three | Four | Five
               | Six | Seven | Eight | Nine | Ten
               | Jack | Queen | King
  deriving (Bounded)

-- returns Ace
minBound :: CardValue

-- returns King
maxBound :: CardValue
{% endhighlight %}

### [Enum](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html#t:Enum)

`Enum` will give your type a sense of the predecessor and successor values. This is most important when dealing with ranges in using your type. Take a look at the following deck assembly. Without `Bounded` the list comprehensions would not be possible and this code would be a lot more verbose.

{% highlight haskell %}
-- | Makes an ordered deck of cards                               
makeDeck :: [Card]                                                
makeDeck = [Card v s | v <- [Ace .. King], s <- [Heart .. Spade]]
{% endhighlight %}

That's derived instances for you anyway. They're a great help when constructing your own types in Haskell. I think an important follow up to this blog post is being able to use these classes in conjunction with the [instance](http://www.haskell.org/haskellwiki/Keywords#instance) keyword so that we can supply the implementation to the definition. Using the card example, we could supply an `Eq` and `Show` instance as follows.

{% highlight haskell %}
data CardSuit = Diamond | Heart | Club | Spade

-- Provide eq implementation
instance Eq CardSuit where
  Diamond == Diamond = True
  Heart == Heart = True
  Club == Club = True
  Spade == Spade = True
  _ == _ = False

-- Provide show implementation
instance Show CardSuit where
  show Diamond = "Diamonds"
  show Heart = "Hearts"
  show Club = "Clubs"
  show Spade = "Spades"
{% endhighlight %}

You can see here that it's quite counter-productive to supply our own `Eq` implementation, but if we did have some funky rules on how we wanted equality operators to work it would be worth it. In the show implementation, I've tried to make the suits read a little more humanly. Around the card table, you would normally hear someone say <em>"Do you have a 2 of clubs?"</em> rather than <em>"Do you have a 2 of club?"</em>. The trailing "s" has been added in the show implementation. Neat.

### [Functor](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html#t:Functor)

`Functor` is applied to wrapper types. You'll commonly see examples used with `Maybe`. You'll use `Functor` when ever you need to supply an [fmap](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html#v:fmap) implementation. Here is a simple example that creates a wrapper data type.

{% highlight haskell %}
data Wrapper a = Wrapper a
   deriving (Show)

instance Functor Wrapper where
   fmap f (Wrapper a) = Wrapper (f a)

-- produces Wrapper 500
fmap (*50) (Wrapper 10)
{% endhighlight %}

`Functor` is useful for types that contain something, Lists, Maps, etc.