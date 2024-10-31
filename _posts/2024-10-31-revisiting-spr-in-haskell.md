---
layout: post
title: Revisiting SPR in Haskell
date: 2024-10-31
comments: false
categories: [ "haskell" ]
---

# Introduction

Quite some time ago, I wrote [a post]({% post_url 2012-11-21-scissors-paper-rock %}) about a very simple Scissors, 
Paper, Rock implementation using Haskell. In today's post, I'd like to revisit that code and clean it up with some tests 
now that I know a little more.

# Avoiding so much `do`

One point is to avoid the use of `do` notation, when it's not needed.

{% highlight haskell %}
-- Map string to Move
str2Move :: String -> Move
str2Move "s" = Scissors
str2Move "p" = Paper
str2Move "r" = Rock
str2Move _   = Unknown

-- Determine the move that beats the given move
getWinner :: Move -> Move
getWinner Scissors = Rock
getWinner Rock     = Paper
getWinner Paper    = Scissors
getWinner Unknown  = Unknown
{% endhighlight %}

These functions were previously `do` notated, can be simplified back to these translations. The usage of pattern 
matching here also improves the readability of the code.

# Improved randomness

What was being used before `getStdGen` has now been replaced with `newStdGen`, which gives us a new random generator 
per game, improving the randomness.

{% highlight haskell %}
main :: IO ()
main = do
   gen <- newStdGen
{% endhighlight %}

# Tests

To verify our game logic, some tests have been added using `Hspec`.

{% highlight haskell %}
-- MainSpec.hs
module MainSpec where

import Test.Hspec
import System.Random (mkStdGen)
import Main  -- Import your module here

main :: IO ()
main = hspec $ do
    describe "str2Move" $ do
        it "converts 's' to Scissors" $
            str2Move "s" `shouldBe` Scissors
        it "converts 'p' to Paper" $
            str2Move "p" `shouldBe` Paper
        it "converts 'r' to Rock" $
            str2Move "r" `shouldBe` Rock
        it "returns Unknown for invalid input" $
            str2Move "x" `shouldBe` Unknown

    describe "getWinner" $ do
        it "Rock beats Scissors" $
            getWinner Scissors `shouldBe` Rock
        it "Paper beats Rock" $
            getWinner Rock `shouldBe` Paper
        it "Scissors beat Paper" $
            getWinner Paper `shouldBe` Scissors
        it "Unknown returns Unknown" $
            getWinner Unknown `shouldBe` Unknown

    describe "getOutcome" $ do
        it "returns Draw when both moves are the same" $
            getOutcome Rock Rock `shouldBe` Draw
        it "returns Winner when player beats CPU" $
            getOutcome Rock Scissors `shouldBe` Winner
        it "returns Loser when CPU beats player" $
            getOutcome Scissors Rock `shouldBe` Loser
        it "returns ND for Unknown player move" $
            getOutcome Unknown Rock `shouldBe` ND
        it "returns ND for Unknown CPU move" $
            getOutcome Rock Unknown `shouldBe` ND

    describe "getCpuMove" $ do
        it "returns Rock for seed 1" $
            getCpuMove (mkStdGen 1) `shouldBe` Rock
        it "returns Scissors for seed 2" $
            getCpuMove (mkStdGen 2) `shouldBe` Scissors
        it "returns Paper for seed 3" $
            getCpuMove (mkStdGen 3) `shouldBe` Paper
{% endhighlight %}

Here is the full code listing:

{% highlight haskell %}
module Main where

import System.IO
import System.Random

data Move = Scissors | Paper | Rock | Unknown deriving (Eq, Show)
data Outcome = Winner | Draw | Loser | ND deriving (Show)

-- Map string to Move
str2Move :: String -> Move
str2Move "s" = Scissors
str2Move "p" = Paper
str2Move "r" = Rock
str2Move _   = Unknown

-- Determine the move that beats the given move
getWinner :: Move -> Move
getWinner Scissors = Rock
getWinner Rock     = Paper
getWinner Paper    = Scissors
getWinner Unknown  = Unknown

-- Calculate the outcome based on player and CPU moves
getOutcome :: Move -> Move -> Outcome
getOutcome player cpu
   | player == Unknown || cpu == Unknown = ND
   | player == cpu = Draw
   | cpu == getWinner player = Loser
   | otherwise = Winner

-- Generate a CPU move based on random number
getCpuMove :: StdGen -> Move
getCpuMove gen = case fst (randomR (1, 3) gen :: (Int, StdGen)) of
   1 -> Rock
   2 -> Scissors
   3 -> Paper
   _ -> Unknown  -- This case is unreachable but keeps pattern exhaustive

main :: IO ()
main = do
   gen <- newStdGen  -- Get a new generator each round for more randomness
   putStr "Enter your choice (s, p, or r): "
   hFlush stdout
   line <- getLine

   let player = str2Move line
   if player == Unknown
      then putStrLn "Invalid input! Please enter 's', 'p', or 'r'."
      else do
         let cpu = getCpuMove gen
         let outcome = getOutcome player cpu

         putStrLn $ "Player Chose: " ++ show player
         putStrLn $ "CPU Chose   : " ++ show cpu
         putStrLn $ "Outcome     : " ++ show outcome
{% endhighlight %}


