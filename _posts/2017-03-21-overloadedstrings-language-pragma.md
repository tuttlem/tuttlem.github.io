---
layout: post
title: OverloadedStrings Language Pragma
date: 2017-03-21
comments: false
categories: [ "haskell", "ghc", "string" ]
---

The `OverloadedStrings` language pragma can be enabled either by passing the `-XOverloadedStrings` switch to GHC or you can just add the following to the top of your Haskell source:

{% highlight haskell %}
{-# LANGUAGE OverloadedStrings #-}
{% endhighlight %}

The `OverloadedStrings` language pragma changes the way that literal strings identify themselves, in a way that favours performance. `[Char]` is a rather cumbersome type to be used when dealing with something as primitive as a string.

{% highlight text %}
Prelude> :t "Hello, world"
"Hello, world" :: [Char]
Prelude> :set -XOverloadedStrings
Prelude> :t "Hello, world"
"Hello, world" :: Data.String.IsString t => t
{% endhighlight %}

The literal string `"Hello, world"` now identifies as a call to the `fromString` function out of the `IstString` type class. You can define instances like so:

{% highlight haskell %}
import GHC.Exts ( IsString(..) )

data Colour = Red | Green | Blue | Other String deriving Show

instance IsString Colour where
  fromString "Red" = Red
  fromString "Green" = Green
  fromString "Blue" = Blue 
  fromString xs = Other xs
{% endhighlight %}

Now we just cast our strings to our type, and the `fromString` functions are invoked for us:

{% highlight text %}
Prelude GHC.Exts> "Red" :: Colour
Red
Prelude GHC.Exts> "Yellow" :: Colour
Other "Yellow"
{% endhighlight %}

