---
layout: post
title: Simple forkIO Usage
date: 2014-01-21
comments: false
---

Creating new threads in Haskell is quite easy (once you know how). Here's a simple snippet for using [forkIO](http://hackage.haskell.org/package/base-4.6.0.1/docs/Control-Concurrent.html#v:forkIO) and [myThreadId](http://hackage.haskell.org/package/base-4.6.0.1/docs/Control-Concurrent.html#v:myThreadId) to get you started.

{% highlight haskell %}
module Main where

import Control.Concurrent

main :: IO ()
main = do
    -- grab the parent thread id and print it
    parentId <- myThreadId
    putStrLn (show parentId)
    
    -- create a new thread (ignore the return)
    _ <- forkIO $ do
      -- grab the child thread id and print it
      childId <- myThreadId
      putStrLn (show childId)
      
    return ()
{% endhighlight %}