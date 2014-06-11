---
layout: post
title: A Quick Lap with Lens
date: 2014-03-19
comments: false
---

### Introduction

When working with complex data structures in Haskell, burying down to observe a piece of information can be tedious. The [Lens library](http://hackage.haskell.org/package/lens-4.0.7/docs/Control-Lens.html) has been created to ease this problem. From the [Lens wiki page](https://github.com/ekmett/lens/wiki/Overview):

> Lenses are composable functional references. They allow you to access and modify data potentially very deep within a structure! 

The Lens library allows you to interact with your data structures in a composable manner, making your code easier to understand - and more fun to write.

### Key Points

* Whilst there are a lot of different functions, `(^.)` allows you to get some data and `(.~)` allows you to set some data
* `makeLenses` is what does all the magic of creating your accessors
* I haven't found anywhere that specifically says this, but it seems that your fields in a record structure need to be preceded with an underscore

### An Example

{% highlight haskell %}
{-# LANGUAGE TemplateHaskell #-}
 
import Control.Lens
 
data Ball = Ball { _position :: (Double, Double), _velocity :: (Double, Double) }
	deriving (Show)
 
-- create the accessors for the Ball type
makeLenses ''Ball
 
-- | Animates a ball's position with respect to a timestep.
-- Takes a ball's existing position and velocity and animates it
-- by the time step provided
--
animate :: Ball -> Double -> Ball
animate b t = do
	position .~ (px + vx * t, py + vy * t) $ b
  where (px, py) = b ^. position
		(vx, vy) = b ^. velocity
 
main :: IO ()
main = do
	-- the original ball
	let b = Ball { _position = (4.5, 6.2), _velocity = (-0.3, 1.2) }
	-- animate the ball by 1 full timestep
	let b' = animate b 1
 
	putStrLn $ "Initial ball : " ++ (show b)
	putStrLn $ "Animated ball: " ++ (show b')
{% endhighlight %}