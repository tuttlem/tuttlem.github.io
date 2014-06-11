---	
layout: post
title: Haskell Cards
date: 2012-11-19
comments: false
---

I need to crawl before I can walk, so a lot of "toy code" is being written using Haskell. It's taken me a while to get to this point, but I now have cards being shuffled to players:

{% highlight haskell %}
module Cards where
 
import System.Random
import Data.Maybe
import Data.List
import Data.Function
 
import Test.HUnit
import Test.QuickCheck
 
-- | Card values for uno cards
data CardValue = Naught | One | Two | Three
	| Four | Five | Six | Seven | Eight | Nine
	| Skip | Reverse | DrawTwo | Wild | WildDrawFour
	deriving (Show, Eq, Enum)
 
-- | Possible card colours
data CardColour = Red | Blue | Green | Yellow
	deriving (Show, Eq, Enum)
 
-- | Defines the attributes of a card
data Card = Card CardValue (Maybe CardColour)
	deriving (Show)
 
-- | Seeds a list of cards with a random value
seedCards :: StdGen -> [Card] -> [(Card, Int)]
seedCards g [] = []
seedCards g (c:cs) = x:seedCards ng cs
	where (seed, ng) = randomR(1, 10000) g :: (Int, StdGen)
				   x = (c, seed)
 
-- | Makes a randomly shuffled deck of cards
makeShuffledDeck :: StdGen -> [Card]
makeShuffledDeck g = [x | c <- sorted, let x = fst c]
	where cards = seedCards g deck
		 sorted = sortBy (compare `on` snd) cards
		   deck = val ++ take 10 (cycle spec)
		    val = [Card v (Just c) | v <- [Naught .. DrawTwo], c <- [Red .. Yellow]]
		   spec = [Card v Nothing | v <- [Wild .. WildDrawFour]]
 
tests = TestList $ map TestCase
	[assertEqual "add tests here" 1 1]
 
prop_empty c1 = (c1::Int) == c1
 
runTests = do
	runTestTT tests
	quickCheck prop_empty
 
-- | Main entry point
main :: IO ()
main = runTests
{% endhighlight %}

There is a lot more to do before this becomes of use. The real think work being conducted here is really between seedCards and makeShuffledDeck. These two functions alone provide randomization to the ordered deck so it can be shuffled.

I hope to post some more on this topic, but I'd like to have a fully functional (or close to) application before I do so.