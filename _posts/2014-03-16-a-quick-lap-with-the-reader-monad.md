---
layout: post
title: A Quick Lap with the Reader Monad
date: 2014-03-16
comments: false
---

### Introduction

The [Reader monad](http://hackage.haskell.org/package/mtl-1.1.0.2/docs/Control-Monad-Reader.html) allows functions to use shared state (or a shared environment) to operate with. According to the Hackage page:

> The Reader monad (also called the Environment monad). Represents a computation, which can read values from a shared environment, pass values from function to function, and execute sub-computations in a modified environment. 

If many of your functions require the same shared values (think like a config file, application settings or just shared state), rather than adding a new parameter to all of your functions that require this information you can put your functions into the [Reader monad](http://hackage.haskell.org/package/mtl-1.1.0.2/docs/Control-Monad-Reader.html) which will give you access to this state.

### Key Pieces

* The Reader constructor takes the form of `Reader s v` where `s` is your state type and `v` is your function return type.
* The `ask` function is what you'll use to retrieve the state value for use in your own functions.
* To run the [Reader monad](http://hackage.haskell.org/package/mtl-1.1.0.2/docs/Control-Monad-Reader.html) you use the `runReader` function.

### An Example

{% highlight haskell %}
import Control.Monad.Reader

-- | Shared configuration for this application.
-- Rather trivial (and useless), it just configures how our application will
-- address the user 
--
data SalutationConfig = SalutationConfig { formal :: Bool }

-- | Returns a greeting
-- Takes in someone's name and returns a greeting string
--
greeter :: String -> Reader SalutationConfig String
greeter name = do
  -- grab the configuration out
  cfg <- ask
  -- grab the "formal" setting from the config
  let f = formal cfg
  
  -- send out the value
  return (makeSalutation f ++ name)

-- | Makes a salutation for a "formal" setting
makeSalutation :: Bool -> String
makeSalutation True = "Good day, "
makeSalutation False = "Wasaaaaaaaap, "

main :: IO ()
main = do
  -- create the configuration
  let cfg = SalutationConfig { formal = False}
  -- run the reader with the configuration for a guy named "Michael"
  let msg = runReader (greeter "Michael") $ cfg

  -- "Wasaaaaaaaaaap, Michael"
  putStrLn msg
{% endhighlight %}
