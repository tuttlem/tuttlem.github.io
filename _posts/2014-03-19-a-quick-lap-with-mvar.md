---
layout: post
title: A Quick Lap with MVar
date: 2014-03-19
comments: false
categories: [ "Haskell", "Programming", "MVar" ]
---

### Introduction

Concurrent programming is hard. It's made a lot easier with good tools and MVar is one of them. MVars is just a location for a value. It can contain a value or contain nothing and the API will block accordingly, providing a safe concurrent programming environment for mutable state.

From the Hackage page for `Control.Concurrent.MVar`:

> An MVar t is mutable location that is either empty or contains a value of type t. It has two fundamental operations: putMVar which fills an MVar if it is empty and blocks otherwise, and takeMVar which empties an MVar if it is full and blocks otherwise. 

### Key Points

* `newEmptyMVar` creates an MVar that has no value to begin with
* `newMVar` creates an MVar that has an initial value
* `takeMVar` returns the current value of the MVar. It'll block until the MVar contains a value
* `putMVar` puts a value into the MVar. It'll block until the MVar doesn't contain a value

### An Example

{% highlight haskell %}
import Control.Concurrent
import Control.Concurrent.MVar
 
main :: IO ()
main = do
	-- create an empty mvar
	m <- newEmptyMVar
	-- get another thread to put a value in it
	forkIO $ putMVar m "A value"
	-- take the value
	x <- takeMVar m
	putStrLn x
{% endhighlight %}