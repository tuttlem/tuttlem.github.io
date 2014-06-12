---
layout: post
title: Unit Testing with QuickCheck
date: 2012-12-30
comments: false
---

### Introduction

Seems I'm forever making card games <em>in Haskell</em>, so it only seemed right that I try and make a library of routines and data types that will get me going quicker. Don't get me wrong, I intend on doing something serious with Haskell one day - I just seriously lack the chops to do so right now.

As with any development process, you as a developer should write unit tests. Not only is it good practice but it also gives you a repeatable base of executions to assure you that the last change you put in won't break your masterpiece. Today I want to talk about the [QuickCheck](http://en.wikipedia.org/wiki/QuickCheck) unit testing library for Haskell.

### What are we testing?

To give you an idea of the playing field we're on, I'll just post some of the simple routines that I have done so far. First up is the data types that will help us represent a single playing card.

{% highlight haskell %}
-- | Card values for a standard deck             
data CardValue = Ace | Two | Three | Four | Five 
   | Six | Seven | Eight | Nine | Ten            
   | Jack | Queen | King                         
   deriving (Show, Eq, Enum)                     
                                                 
-- | Possible card suits                         
data CardSuit = Heart | Diamond | Club | Spade   
   deriving (Show, Eq, Enum)                     
                                                 
-- | A card                                      
data Card = Card CardValue CardSuit              
   deriving (Show, Eq)                           
{% endhighlight %}

A card has a suit and a value. Pretty straight forward. I could have made a type that wrapped an array of the `Card` type and called it `Deck`, but I'm happy just handling an array of `Card`. Now to build a deck and to shuffle it!

{% highlight haskell %}
-- | Seeds a list of cards with a random value                    
seedCards :: StdGen -> [Card] -> [(Card, Int)]                    
seedCards g []     = []                                           
seedCards g (c:cs) = x:seedCards ng cs                            
   where (seed, ng) = randomR(1, 10000) g :: (Int, StdGen)        
         x          = (c, seed)                                   
                                                                  
-- | Makes an ordered deck of cards                               
makeDeck :: [Card]                                                
makeDeck = [Card v s | v <- [Ace .. King], s <- [Heart .. Spade]] 
                                                                  
-- | Makes a randomly shuffled deck of cards                      
makeShuffledDeck :: StdGen -> [Card]                              
makeShuffledDeck g = [x | c <- sorted, let x = fst c]             
   where cards  = seedCards g makeDeck                            
         sorted = sortBy (compare `on` snd) cards                 
{% endhighlight %}

When a deck is built with `makeDeck` the cards are ordered just like they are when you open a fresh deck of cards, so we need to shuffle them in order to make this game any fun! `seedCards` assigns a random value to each card that it is passed and then `makeShuffledDeck` saves the day by ordering by this random seed to give a shuffled deck.

That's all pretty simple still and that's where the "testable" parts stop. So, still the question: what are we testing? Well, I'm sure there are plenty of other scenarios, but for today's purposes we'll test the following:

* Are there 52 cards in a deck made by `makeDeck`?
* Are there still 52 cards in a deck after they've been processed by `makeShuffledDeck`?
* Is the deck made by `makeDeck` not in the same order as the deck made by `makeShuffledDeck`?

Great. With these three scenarios in mind, here's how easy it is to assert these facts using QuickCheck.

{% highlight haskell %}
runTests = do                                                      
   quickCheck ((length (makeDeck)) == 52)                          
   quickCheck (\n -> length (makeShuffledDeck (mkStdGen n)) == 52) 
   quickCheck (\n -> makeShuffledDeck (mkStdGen n) /= makeDeck)    
                                                                   
main :: IO ()                                                      
main = runTests                                                    
{% endhighlight %}

As it should be, these tests read rather humanly. And after running this suite of tests we end up with the following results:

{% highlight text %}
+++ OK, passed 1 tests.
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
{% endhighlight %}

<strong>Hold on!<strong> <em>100 tests</em>? We only defined 3 tests though. How can this be? You'll see that for the second and third tests actually have an anonymous function passed to them. Because both of these depend on a random number generator (to shuffle the deck), I've passed in [mkStdGen](http://hackage.haskell.org/packages/archive/random/latest/doc/html/System-Random.html#v:mkStdGen)'s integer that it maps to a generator from the function's parameter list. `QuickCheck` grabbed hold of this and rather than just running 1 test, it went ahead and gave the anonymous function 100 random values. That's much better coverage for what is seemingly the cost of defining the test as an anonymous method. Immediately you can see the power of unit testing with such a simple framework and how you can be productive relatively quickly.
