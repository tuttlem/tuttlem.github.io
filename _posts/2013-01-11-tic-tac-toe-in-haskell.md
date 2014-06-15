---
layout: post
title: Tic Tac Toe in Haskell
date: 2013-01-11
comments: false
categories: [ "Haskell", "tic-tac-toe", "toys" ]
---

### Introduction
Still trying to flex my skills in the Haskell arena and am still chasing a real project to do. Until I find said project, silly little games it is. A little while ago I watched the Haskell Live videos and in these videos a good start is made on a chess framework. Very interesting stuff to see how something like this is put together and if you haven't seen them, I suggest you go and check out [episode 1](http://www.youtube.com/watch?v=ScS8Q32lMxA) and [episode 2](http://www.youtube.com/watch?v=6KkF5-_erns). Anyway, the videos don't yet take you through to a completed implementation and while I'd love to say that I possess the skills to take the chess game through, I'd be punching above my weight. In compromise, I've been able to use some of the concepts that Rein has demonstrated in his videos - in my own implementation of <strong>Tic Tac Toe</strong>. Today's post will take you through my implementation. 

### Functional Specification

Well, if you're really wanting the functional in's-and-out's of the game Tic-tac-toe, I suggest you step our of your bubble and read [this](http://en.wikipedia.org/wiki/Tic-tac-toe). This implementation will involve two rather random computer players battling it out, so no user interactivity (yet). Well, thanks to a cheeky link to a wikipedia article, that's the smallest functional specification I've ever seen. On to the code.

### Pieces

First up, we'll deal with pieces. To this program, a piece can be a Naught or a Cross. When the piece is a Naught, we want to represent it with a 'o', when the piece is a Cross, we want to represent it with an 'x'. Here's the type declaration and functions we'll use to work with pieces.

{% highlight haskell %}
-- | A piece is what a player places in a cell
data Piece = Naught | Cross                   
   deriving (Eq, Show, Read)
 
-- | Converts a piece into the board representation     
showPiece :: Piece -> Char                              
showPiece Naught = 'o'                                  
showPiece Cross  = 'x'                                  
                                                        
-- | Converts the board representation back into a piece
readPiece :: Char -> Piece                              
readPiece 'o' = Naught                                  
readPiece 'x' = Cross                                   
{% endhighlight %}

A piece can only be either a Naught or a Cross, nothing else. The functions are all very straight forward and meeting the requirements of our specification. Naughts will look like "o's" and Crosses will look like "x's". Just to be sure, we can setup a couple of `QuickCheck` tests to assure us that it's ok.

{% highlight haskell %}
quickCheck (readPiece (showPiece Naught) == Naught) 
quickCheck (readPiece (showPiece Cross) == Cross)   
{% endhighlight %}

We're ready to move on.

### Cells

A cell is a representation of one of the nine squares that you find on a tic-tac-toe board. A cell differs from a piece as a cell can have a piece value or it can be empty. In our implementation, empty cells will be represented by a period `.`. Let's take a look at the types and the functions we'll use to interact with cells.

{% highlight haskell %}
-- | A cell is empty or it contains a piece
type Cell = Maybe Piece                    

-- | Shows the value of a cell        
showCell :: Cell -> Char              
showCell Nothing  = '.'               
showCell (Just p) = showPiece p       
                                      
-- | Reads a board cell and transforms
readCell :: Char -> Cell              
readCell '.' = Nothing                
readCell p   = Just $ readPiece p     
{% endhighlight %}

We use `Maybe` to allow either a `Nothing` or a `Piece` value for a cell. With the help of those functions we've already written for pieces, you can see that these cell functions are very simple. We've only had to cater for one extra case, the case of when there's no value in a cell. We'll setup a few more QuickCheck tests just to make sure that our Cell code is working ok.

{% highlight haskell %}
quickCheck (readCell (showCell Nothing) == Nothing)            
quickCheck (readCell (showCell (Just Cross)) == (Just Cross))  
quickCheck (readCell (showCell (Just Naught)) == (Just Naught))
{% endhighlight %}

Great. With this code written we're ready to wrap up a List of these cells into a field that we will play on.

### The Field

A 3x3 grid is normally what's drawn prior to playing a game of tic-tac-toe. We will define the field as an array of an array of cells. Here's the type and supporting functions for a field.

{% highlight haskell %}
-- | A field is a 3x3 array of cells
type Field = [[Cell]]               

-- | Shows a field of cells        
showField :: Field -> [String]     
showField = map showCellRow        
   where showCellRow = map showCell
                                   
-- | Reads a field                 
readField :: [String] -> Field     
readField = map readCellRow        
   where readCellRow = map readCell
{% endhighlight %}

Much as before, you can see that we're leaning on our already-built set of functions to implement all of our field-based functions. At this point, we can define the starting field that all of our games will originate with.

{% highlight haskell %}
initialField = ["..." 
               ,"..." 
               ,"..."]
{% endhighlight %}

You can see that the field is in string format. It's a little more human to read and it's all ready for our `readField` function to use as is demonstrated by the following `QuickCheck` test.

{% highlight haskell %}
quickCheck (showField (readField initialField) == initialField)
{% endhighlight %}

It's at this point where any similarities between the Haskell Live videos and my implementation end. It's probably natural that this is the point as we're going to start dealing with some game logic.

### Winners and Losers

There are a few tests that we have to be constantly running in order to simulate a real-life game of tic-tac-toe. One of which is "do we have a winner yet? Testing if a winner has been established is testing if the same symbol appears on a horizontal, vertical or diagonal line. In each of these configurations, there are only going to be three cells to test, so I made a general purpose array tester. I've use pattern matching in this function to be as clear as possible. It's not as DRY as it could be, but we can improve it later - let's just get it working. Here's the function to test three list elements to see if there's a winner.

{% highlight haskell %}
-- | Tests a prepared row for a winner                       
testRow :: [Cell] -> Maybe Piece                             
testRow [Just Naught, Just Naught, Just Naught] = Just Naught
testRow [Just Cross, Just Cross, Just Cross]    = Just Cross 
testRow _                                       = Nothing    
{% endhighlight %}

Easy. This function looks like it will be very effective in testing winners on the horizontal lines. But what about the vertical lines that occur in different arrays? and the diagonal lines that exist in different arrays and in different indexes across the horizontal axis? My answer here is to create a function that make all of these testable vectors into horizontal rows so that we can use the above function on all of the different win configurations. Here's the code to build an array of these 3 element arrays.

{% highlight haskell %}
-- | Flattens the winnable view of a field into testable rows         
makeRows :: Field -> [[Cell]]                                         
makeRows f = horiz ++ vert ++ diag                                    
   where horiz = [r | r <- f]                                         
         col1  = [r | r <- head f]                                    
         col2  = [r | r <- head $ drop 1 f]                           
         col3  = [r | r <- head $ drop 2 f]                           
         vert  = [col1, col2, col3]                                   
         diag  = [[head col1, head $ drop 1 col2, head $ drop 2 col3] 
                 ,[head $ drop 2 col1, head $ drop 1 col2, head col3]]
{% endhighlight %}

Walking through this one piece-by-piece, we can see a number of items in the where clause. Sure, these could be consolidated into a big-fat-one-liner, but I wanted to make clear exactly what was being concatenated. So, `horiz` is a list comprehension giving you each horizontal line, `col1`, `col2` and `col3` are the vertical columns. `diag` is a little less clearer through its use of head and drop, but it does form the diagonals into testable rows.

### Playing the game

Weather you're a computer or human player, you'll need to know where are the valid spots to move. It'll be intuitive enough from the user interface presented for a human to realise where their next move will be, but our computer player counterparts will need to be told explicitly where they can move. This next function just finds all of the available cells that you can move into.

{% highlight haskell %}
-- | Gathers all of the positions that are free                     
getOptions :: Field -> [Int]                                        
getOptions f = [snd x | x <- zip (concat f) [0..], fst x == Nothing]
{% endhighlight %}

We assign an index (from 0 to 8) to each cell and only return the indexes that are paired with a `Nothing` value. This function will help us later when we go to build a user interface for this application. We'll be able to prompt the user intuitively.

Flattening the array gives us the [0..8] range, which over the 3x3 grid (which is really our tic-tac-toe representation), the range looks like this.

{% highlight text %}
0 1 2
3 4 5
6 7 8
{% endhighlight %}

This is timely information as we're just about to present the function that will place a piece on the field for us. Placing a piece will require us to provide the current game field, the piece we want to apply and the index that we want to apply it at. This function cuts the list on the index that we want to apply and then re-concatenates the broken list around the new piece (obviously dropping the cell that we're putting a piece into). Here's the code.

{% highlight haskell %}
-- | Places a piece on the board                        
placePiece :: Field -> Piece -> Int -> Field            
placePiece f p n = chunksOf 3 $ prior ++ [cell] ++ after
   where flat = concat f                                
         prior = take n $ flat                          
         after = drop (n + 1) $ flat                    
         cell = (Just p) :: Cell                        
{% endhighlight %}

Trying to be as clear as possible with my names here. `prior` being the list before the break, `after` is what comes after. Now it's time to introduce our "artificial intelligence" which really - there isn't any intelligence here. It's based purely off of a random number generator. Here's how our computer players will play a round of tic-tac-toe.

{% highlight haskell %}
-- | Takes a random turn on a field                                      
takeRandomTurn :: StdGen -> Piece -> Field -> (StdGen, Field)            
takeRandomTurn gen p f = (seed, placePiece f p (opts !! idx))            
   where opts        = getOptions f                                      
         (idx, seed) = randomR(0, (length opts) - 1) gen :: (Int, StdGen)
{% endhighlight %}

We're taking in a random number generator, the piece type that the computer will place and the field that it will be placed in. The output to which will be a new random number generator and the changed field. This way, we can repeatedly call this function and not worry about losing our sense of entropy (or field state!). Finally we have the function that will play our game until we have a result (win naught, win cross or tie). I needed to implement a "not" function for my piece type. This is just so I can offer each "player" a go interchangeably. Here's the code.

{% highlight haskell %}
-- | Acts like a boolean "not" for tic-tac-toe pieces                         
otherPiece :: Piece -> Piece                                                  
otherPiece Cross = Naught                                                     
otherPiece Naught = Cross                                                     
                                                                              
-- | Plays a game between two random cpu players                              
playGame :: StdGen -> Piece -> Field -> IO ()                                 
playGame gen p f = if spaces == 0                                             
                      then putStrLn "Game was a tie!"                         
                      else if winner /= Nothing                               
                              then do                                         
                                 putStrLn $ (unlines fstr)                    
                                 putStrLn $ (show winner) ++ " won the game!" 
                              else do                                         
                                 putStrLn $ (unlines fstr)                    
                                 playGame ng (otherPiece p) nf                
   where winner   = hasWinner f                                               
         spaces   = length $ getOptions f                                     
         (ng, nf) = takeRandomTurn gen p f                                    
         fstr     = showField f                                               
{% endhighlight %}

This reads fairly humanly. We assume that if execution breaks into this function, a result wasn't established on the execution prior, so we're comfortable in the first check that if there are no spaces left to put pieces, we must declare the game as a tie. The next test is if the previous call to this function caused the other player to win the game. In the event that this check is confirmed, a message is written out and we do not continue any further. If there still isn't a winner, we get this player to place a piece into a cell and then recurse on this function as the other player. Here is how a game runs when using 1 into `mkStdGen`.

{% highlight text %}
*TicTacToe> let f = readField initialField
*TicTacToe> playGame (mkStdGen 1) Cross f
...
...
...

..x
...
...

o.x
...
...

o.x
...
..x

o.x
...
.ox

oxx
...
.ox

oxx
.o.
.ox

oxx
.o.
xox

oxx
.oo
xox

Game was a tie!
{% endhighlight %}

That's tic-tac-toe.

Next time I'll add some user interactivity so that the computer doesn't have all the fun!