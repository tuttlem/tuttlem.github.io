---
layout: post
title: A Quick Lap with the Writer Monad
date: 2014-03-16
comments: false
categories: [ "Haskell", "Writer", "Monad" ]
---

### Introduction

The [Writer monad](http://hackage.haskell.org/package/mtl-2.1.2/docs/Control-Monad-Writer-Lazy.html#t:Writer) allows functions to accumulate information as functions execute. According to the Hackage page:

> A writer monad parameterized by the type w of output to accumulate. 

Perhaps not the most verbose of descriptions, however this is rather simple to explain with a well known example. In previous programming disciplines you would have needed to log information out of your code as your program "did things". The [Writer monad](http://hackage.haskell.org/package/mtl-2.1.2/docs/Control-Monad-Writer-Lazy.html#t:Writer) allows you to write out information in a log form. This doesn't necessarily have to be in textual log format; an example I have seen recently is to keep track of calculations used to come to a final result. The calculations put into that log sped up calculations on other figures.

The idea here is to not clutter your code having to support things like logging/tracing, etc. Employing this monad gives your code the ability to produce this output on the side without getting in the way.

### Key Pieces

* Functions in the [Writer monad](http://hackage.haskell.org/package/mtl-2.1.2/docs/Control-Monad-Writer-Lazy.html#t:Writer) are decorated with `Writer l r`. `l` in this case is the type that you'll be logging out where `r` is the result being returned from your function.
* The function `tell` is what's used to push another value into the log/trace/writer.
* Operations in the [Writer monad](http://hackage.haskell.org/package/mtl-2.1.2/docs/Control-Monad-Writer-Lazy.html#t:Writer) can be chained together using `>>=`
* `runWriter` is what you'll use to run something in the [Writer monad](http://hackage.haskell.org/package/mtl-2.1.2/docs/Control-Monad-Writer-Lazy.html#t:Writer) to get your result back.

### An Example

{% highlight haskell %}
import Control.Monad.Writer

-- | Starts a value off.
-- This function doesn't perform any calculation at all, it just prepares an
-- initial value to start in the calculation pipeline
--
start :: Int -> Writer [String] Int
start x = do
  tell (["Starting with " ++ show x])
  return x

-- | Halve a value
-- Any value passed into this function gets halved
--
half :: Int -> Writer [String] Int
half x = do
  tell (["Halving " ++ show x])
  return (x `div` 2)

-- | Squares a value
-- Any value passed into this function gets squared
--
sqr :: Int -> Writer [String] Int
sqr x = do
  tell (["Squaring " ++ show x])
  return (x * x)

main :: IO ()
main = do
  let work = runWriter $ start 10 >>= half >>= sqr >>= half
  let ans  = fst work
  let log  = snd work

  putStrLn $ "Answer: " ++ show ans
  putStrLn ""
  putStrLn " ==== Log ==== "

  mapM_ putStrLn log
{% endhighlight %}

