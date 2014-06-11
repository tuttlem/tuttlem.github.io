---
layout: post
title: Watching the File System with INotify in Haskell
date: 2014-02-10
comments: false
---

### Introduction

Some applications that you write from time to time may require you to study changes that occur on the file system. These changes could be that a file arrives, is modified, is closed, etc. Your program can then respond accordingly to these interesting events.

In today's post, I'll show you how to monitor the file system for changes using the [hinotify](http://hackage.haskell.org/package/hinotify-0.3.1/docs/System-INotify.html) package.

### What is inotify?

inotify is short for inode notify. It's a piece of the Linux Kernel that adds notifications at the filesystem level so that userspace programs can take advantage of these events. The [Wikipedia page on inotify](http://en.wikipedia.org/wiki/Inotify) has a good explanation for further reading.

### The Code

There are 3 main jobs that we need to take care of here:

* Create awareness with INotify
* Register your interest in changes
* Respond to the changes

{% highlight haskell %}
module Main where

import Control.Concurrent (threadDelay)
import System.INotify

main :: IO ()
main = do
  -- the paths that we'll monitor
  let paths = [ "/tmp", "/home/user" ]
  
  -- setup INotify
  withINotify $ \n -> do
    -- monitor each predefined path, and respond using printEvent
    mapM_ (\f -> addWatch n [Modify, CloseWrite] f (printEvent f)) paths
    
    -- this gives "addWatch" some time to collect some data
    threadDelay 10000000
    
  where
    -- print the file and event to the console
    printEvent :: FilePath -> Event -> IO ()
    printEvent f e = putStrLn (f ++ ": " ++ show e)
{% endhighlight %}

I've tried to comment this code as best I can to show you what's going on. It's all pretty straight forward. Delaying the main thread may seem unintuitive, however without this call being made the program will finish execution without collecting data (because INotify doesn't block!).

Nifty.