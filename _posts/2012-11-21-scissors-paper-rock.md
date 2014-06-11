---
layout: post
title: Scissors, Paper, Rock!
date: 2012-11-21
comments: false
---

Haskell is a strange beast at times. I think you can see from the brute-force approach on this implementation of SPR that I was getting clearly frustrated with even some of the simplest of things.

I really could have used "Maybe"
I really could have used "Read"

It's a learning experience:

{% highlight haskell %} 
module Main where

   import System.IO
   import System.Random

   data Move = Scissors | Paper | Rock | Unknown deriving (Eq,Show)
   data Outcome = Winner | Draw | Loser | ND deriving (Show)

   str2Move :: String -> Move
   str2Move s = do
      case s of
         "s" -> Scissors
         "p" -> Paper
         "r" -> Rock
         _   -> Unknown

   getWinner :: Move -> Move
   getWinner m = do
      case m of
         Scissors -> Rock
         Rock     -> Paper
         Paper    -> Scissors
         Unknown  -> Unknown

   getOutcome :: Move -> Move -> Outcome
   getOutcome player cpu
      | player == Unknown || cpu == Unknown = ND
      | player == cpu = Draw
      | cpu == winner = Loser
      | otherwise = Winner
      where winner = getWinner player

   getCpuMove :: StdGen -> Move
   getCpuMove gen = do
      let (randNumber, newGen) = randomR(1, 3) gen :: (Int, StdGen)
      case randNumber of
         1 -> Rock
         2 -> Scissors
         3 -> Paper

   main = do
      gen <- getStdGen
      putStr "Enter your choice (s, p or r): "
      hFlush stdout
      line <- getLine

      let player = str2Move line
      let cpu = getCpuMove gen
      let outcome = getOutcome player cpu

      putStrLn $ "Player Chose: " ++ (show player)
      putStrLn $ "Cpu Chose   : " ++ (show cpu)
      putStrLn $ "Outcome     : " ++ (show outcome)
{% endhighlight %}

Some who have been nice enough to comment from time to time have suggested that I move forward with this implementation (inclusion of Lizard, Spock).

Anyway, I'll keep struggling.
