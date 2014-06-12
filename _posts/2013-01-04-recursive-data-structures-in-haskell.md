---
layout: post
title: Recursive Data Structures in Haskell
date: 2013-01-04
comments: false
---

### Introduction

As I've been following along in [Learn You a Haskell for Great Good](http://learnyouahaskell.com/), I've been coming across some really interesting material around the type system. In today's post, I want to walk through the `Tree` data type that they define in this book for the section on recursive data structures.

### What it is?

A recursive data structure is a type that has itself as a type amongst its fields. Pretty wild idea, but really useful for situations of `Child` -> `Parent`, `Tree` -> `Leaf`, etc. In today's example we're going to be using the `Tree` structure which looks like this.

{% highlight haskell %}
data Tree a = EmptyTree | Node a (Tree a) (Tree a)
  deriving (Show)
{% endhighlight %}

This data type reads as, a `Tree` can either be an `EmptyTree` or it can have two nodes (that are also trees). You can start to see that this would be a best-fit as a binary search tree. Using an instance of `Functor`, we can give the the `Tree` data type recursively iterative properties through `fmap`.

{% highlight haskell %}
instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)
{% endhighlight %} 

### Using the type

Getting data into the tree is done cleverly (or so I think) with some basic pattern matching. I think it's done cleverly because it reads so well/humanly (perhaps I'm easily pleased).

{% highlight haskell %}
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
   | x == a = Node x left right
   | x < a  = Node a (treeInsert x left) right
   | x > a  = Node a left (treeInsert x right)
{% endhighlight %}

Inserting an `EmptyTree` node will result in a node with an `EmptyTree` either side. Inserting data for key that exists will overwrite the existing data. Inserting a lesser key (lesser according to `Ord`) will put the node on the left, greater onto the right. Actually building a tree can be as simple as the following.

{% highlight haskell %}
fmap (*1) (foldr treeInsert EmptyTree [5,7,3])
{% endhighlight %}

This results in a tree that looks like this.

{% highlight text %}
*Main> fmap (*1) (foldr treeInsert EmptyTree [5,7,3])
Node 3 EmptyTree (Node 7 (Node 5 EmptyTree EmptyTree) EmptyTree)
{% endhighlight %}

3 is at the trunk with no left-hand-side. 3's right hand side has a 7 with 5 for its left hand side and no right hand sides (for 5 or 7). Reading the dump out from ghci is going to be better for your understanding than trying to read my English describing the tree's structure. The important part that I wanted to illustrate really was the use of `fmap` here. We map the function `(*1)` so that none of the values in our source array change. We apply `treeInsert` in a fold-right starting with `EmptyTree` across the source array [5,7,3].

If you ask me, that's pretty cool.
