---
layout: post
title: Notes on working monadically
date: 2014-06-20
comments: false
categories: ["Haskell", "programming", "monad"]
---

You can do little bits and pieces in your Haskell code to take it from an imperative looking style to being more concise and monadic in your approach. In today's post, I'm going to run through a small sample of functions that should help this process greatly.

One final thought is that it's not so much the use of these functions that's important; it's more the change in thought process to put you in a position where these functions become effective is where the true power lies.

### >>=

The `>>=` function <em>(pronounced "bind")</em> sequences operations together by passing the result of what's on the left hand side, to the right hand side. `>>=` looks like this:

{% highlight haskell %}
(>>=) :: Monad m => m a -> (a -> m a) -> m b
{% endhighlight %}

`>>=` is asking for:

* `a` which is a value wrapped in a monad
* `(a -> m a)` which is a function accepting a value and returning a wrapped value

An example of this in action looks like this:

{% highlight haskell %}
-- | Returns a string in context of a monad
ns :: Monad m => m [Char]
ns = return $ "Hello"

-- | Prints a string to the console
putStrLn :: String -> IO ()

-- Using bind
ns >>= putStrLn
{% endhighlight %}

What's going on in the above snippet is that `>>=` has unwrapped the string returned by `ns` from its `IO` monad so that it's just a string. `>>=` has then applied this raw value to `putStrLn`.

### =<<

The `=<<` function performs the same role as `>>=` only it has its parameters flipped.

{% highlight haskell %}
putStrLn =<< ns
{% endhighlight %}

### >>

The `>>` function performs he same sequencing as what `>>=` does, only the first action specified is discarded. `>>` looks like this:

{% highlight haskell %}
(>>) :: Monad m => m a -> m b -> m b
{% endhighlight %}

This particular function comes in handy where you're interested in not passing along a result from certain links in your sequencing chain, like this:

{% highlight haskell %}
putStrLn "Hello. What is your name? " >>  getLine
                                      >>= putStr
                                      >>  putStrLn "! That's a great name"
{% endhighlight %}

From this particular sequence, you can see that

* The action emitted from the first `putStrLn` is dropped
* The action emitted from `getLine` is passed onto `putStr`
* The action emitted from `putStr` is dropped
* The last action terminates the sequence

### sequence

`sequence` will evaluate all of the actions passed to it from left to right and return out the results of these actions. It's defined like this

{% highlight haskell %}
sequence :: Monad m => [m a] -> m [a]
{% endhighlight %}

What this allows you to do is something like this

{% highlight haskell %}
sequence [putStr "What's your name? " >> getLine
         ,putStr "What's your age? " >> getLine
         ,putStr "What's your favourite colour? " >> getLine
         ]
{% endhighlight %}

This will then give you back an array of the `IO` actions emitted from each array index.

`sequence_` will perform the same task as what `sequence` does, only it'll throw away the result.

### mapM

`mapM` will allow you to perform a monadic action over a list of normal <em>(or unwrapped)</em> values. It looks like this

{% highlight haskell %}
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
{% endhighlight %}

`mapM` wants:

* A function that takes an unwrapped value `a` as its first parameter and returns a wrapped value `m b`
* A list of unwrapped values `[a]`

It will then give you back a list of wrapped outputs `m [b]`. So, this allows you to do something like this

{% highlight haskell %}
-- our list of unwrapped values
let questions = ["What's your name? ", "What's your age? "]

-- print out each question and ask for a response
mapM (\q -> putStr q >> getLine) questions
{% endhighlight %}

Also notice that in the lambda above `(\q -> putStr q >> getLine)`, we've used the `>>` function from above as we don't care for the action emitted from printing a string to the console.

`mapM_` will perform the same task as what `mapM` does, only it'll throw the result away.

### filterM

`filterM` will filter a list based on a `Bool` wrapped in an action. Here's how `filterM` is defined

{% highlight haskell %}
filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
{% endhighlight %}

`filterM` will execute an action `(a -> m Bool)` that wants an `a` as its input and returns you a wrapped `m Bool` which will determine which `a`'s in the passed list `[a]` end up in the resulting wrapped list `m [a]`. Mouthful, I know. Here's an example

{% highlight haskell %}
filterM (\_ -> randomIO >>= return . even) [1..50]
{% endhighlight %}

The lambda here `(\_ -> randomIO >>= return . even)` actually ignores the input parameter by using `\_`. It's using `randomIO` to <em>grab a number out of the hat</em> which is then bound to `(return . even)`, which will return a wrapped `Bool` of if the number supplied by `randomIO` is even or not.


There's heaps more that you can do. Just check out the [Control.Monad namespace](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Monad.html) of the base library for some more. That's it for today though!
