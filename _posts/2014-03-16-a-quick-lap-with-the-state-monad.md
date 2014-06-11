---
layout: post
title: A Quick Lap with the State Monad
date: 2014-03-16
comments: false
---

### Introduction

The [State monad](http://hackage.haskell.org/package/mtl-2.1.2/docs/Control-Monad-State-Class.html#t:MonadState) gives functionality of both the [Reader monad](http://hackage.haskell.org/package/mtl-1.1.0.2/docs/Control-Monad-Reader.html) and [Writer monad](http://hackage.haskell.org/package/mtl-2.1.2/docs/Control-Monad-Writer-Lazy.html#t:Writer) in one. When using the [State monad](http://hackage.haskell.org/package/mtl-2.1.2/docs/Control-Monad-State-Class.html#t:MonadState) you're able to read the state at any time and then set it back again, providing read/write access.

### Key Points

* The function `get` is used to read the current state
* The function `put` is used to set the state
* `runState` is used to manage execution of functions that run in the [State monad](http://hackage.haskell.org/package/mtl-2.1.2/docs/Control-Monad-State-Class.html#t:MonadState)
* Operations in the [State monad](http://hackage.haskell.org/package/mtl-2.1.2/docs/Control-Monad-State-Class.html#t:MonadState) can use `>>=` to be chained together
* Functions in the [State monad](http://hackage.haskell.org/package/mtl-2.1.2/docs/Control-Monad-State-Class.html#t:MonadState) are decorated with `State s v`. Where `s` is the type of the state and `v` is the return type from the function<

### An Example

{% highlight haskell %}
import Control.Monad.State

-- | Starts a value off.
-- This function doesn't perform any calculation at all, it just prepares an
-- initial value to start in the calculation pipeline
--
start :: Int -> State [String] Int
start x = do
  put ["Starting with " ++ show x]
  return x

-- | Halve a value
-- Any value passed into this function gets halved
--
half :: Int -> State [String] Int
half x = do
  s <- get
  let ns = s ++ ["Halving " ++ show x]
  put ns
  return (x `div` 2)

-- | Squares a value
-- Any value passed into this function gets squared
--
sqr :: Int -> State [String] Int
sqr x = do
  s <- get
  let ns = s ++ ["Squaring " ++ show x]
  put ns
  return (x * x)

main :: IO ()
main = do
  let c = runState $ start 10 >>= half >>= sqr >>= half
  let work = c [""]
  let ans  = fst $ work
  let log  = snd $ work

  putStrLn $ "Answer: " ++ show ans
  putStrLn ""
  putStrLn " ==== Log ==== "

  mapM_ putStrLn log
{% endhighlight %}
