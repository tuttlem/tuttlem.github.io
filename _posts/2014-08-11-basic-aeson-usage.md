---
layout: post
title: Basic Aeson Usage
date: 2014-08-11
comments: false
categories: ["haskell", "aeson", "json"]
---

### Introduction

[JSON](http://en.wikipedia.org/wiki/JSON) is a common interchange data format used across the web these days. It's so popular because it's easy to work with, marries directly with [Javascript](http://en.wikipedia.org/wiki/JavaScript) <em>(really helping out the web guys)</em> and its structure allows you to specify complex information in a simple, readable format.

In today's post I'm going to walk through some basic usage of the Haskell library [aeson](https://hackage.haskell.org/package/aeson) which provides some tools for working with JSON.

### Getting started

First of all, you'll need the aeson library installed locally to work with it. You can install this with cabal:

{% highlight bash %}
$ cabal install aeson
{% endhighlight %}

While that's installing, take a look at the [examples](https://github.com/bos/aeson/tree/master/examples) up in the [repo for aeson](https://github.com/bos/aeson/).

### Defining your data structure

The first example, [Simple.hs](https://github.com/bos/aeson/blob/master/examples/Simplest.hs) starts with defining the data structure that we're expecting to work with:

{% highlight haskell %}
data Coord = Coord { x :: Double, y :: Double }
             deriving (Show)
{% endhighlight %}

This is pretty simple. Just a 2d co-ordinate. The example goes on to define instances of [ToJSON](http://hackage.haskell.org/package/aeson-0.8.0.0/docs/Data-Aeson.html#t:ToJSON) and [FromJSON](http://hackage.haskell.org/package/aeson-0.8.0.0/docs/Data-Aeson.html#t:FromJSON) to facilitate the serialization and deserialization of this data (respectively).

{% highlight haskell %}
instance ToJSON Coord where
  toJSON (Coord xV yV) = object [ "x" .= xV,
                                  "y" .= yV ]

instance FromJSON Coord where
  parseJSON (Object v) = Coord <$>
                         v .: "x" <*>
                         v .: "y"
  parseJSON _ = empty
{% endhighlight %}

The only really curly bit about this, is the use of the [(.=)](https://hackage.haskell.org/package/aeson-0.8.0.0/docs/Data-Aeson.html#v:.-61-) and the [(.:)](https://hackage.haskell.org/package/aeson-0.8.0.0/docs/Data-Aeson.html#v:.:) operators. These will pair-up or extract data in context of your JSON object.

### Simplify

With all of this said, now take a look at [Generic.hs](https://github.com/bos/aeson/blob/master/examples/Generic.hs). This file makes use of the [DeriveGeneric](http://www.haskell.org/haskellwiki/Generics) language extension to write the ToJSON and FromJSON implementations above. Those type class instances now read as follows:

{% highlight haskell %}
instance FromJSON Coord
instance ToJSON Coord
{% endhighlight %}

The type of Coord needs to be augmented slightly to include the [Generic](https://hackage.haskell.org/package/base-4.6.0.1/docs/GHC-Generics.html#t:Generic) type class.

{% highlight haskell %}
data Coord = Coord { x :: Double, y :: Double }
             deriving (Show, Generic)
{% endhighlight %}

Pretty easy.

### Reading and writing

Finally, we need to actually poke a string into this thing and pull built objects back out of it. The main covers this:

{% highlight haskell %}
main :: IO ()
main = do
  let req = decode "{\"x\":3.0,\"y\":-1.0}" :: Maybe Coord
  print req
  let reply = Coord 123.4 20
  BL.putStrLn (encode reply)
{% endhighlight %}

You can see that we can turn a string into a Coord object with little effort now.

Extending this a little further to read values off of disk, we can lean on `readFile` from `Data.ByteString.Lazy`:

{% highlight haskell %}
λ> x <- (eitherDecode <$> B.readFile "coord.json") :: IO (Either String Coord)
λ> x
Right (Coord {x = 3.5, y = -2.2})
{% endhighlight %}

`eitherDecode` was either going to give us an error message on the `Left`, or the built object on the `Right`.

That's it for today.

