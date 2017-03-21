---
layout: post
title: Type Families language pragma
date: 2017-03-22
comments: false
categories: [ "haskell", "pragma", "type", "families" ]
---

The [Type Families](https://wiki.haskell.org/GHC/Type_families) language pragma provides the developer the ability to attribute an association between two different types. This is going to allow us to write the same function for different types.

In this example, the type class `Falsey` is somewhat of a loose boolean test on every day values . . not just `Bool` values. By allowing the developer to specify `type` within the `class` and `instance` we establish the association between the types:

{% highlight haskell %}
{-# LANGUAGE TypeFamilies #-}

class Falsey a where
  type Value a
  isFalsey :: a -> Bool

instance Falsey [a] where
  type Value [a] = a
  isFalsey [] = True
  isFalsey _  = False

instance Falsey Bool where
  type Value Bool = Bool
  isFalsey x  = x

main :: IO ()
main = do
  print $ isFalsey []
  print $ isFalsey [1, 2, 3]
  print $ isFalsey True
  print $ isFalsey False
{% endhighlight %}

