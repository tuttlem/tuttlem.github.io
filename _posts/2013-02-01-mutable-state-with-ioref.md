---
layout: post
title: Mutable State with IORef
date: 2013-02-01
comments: false
categories: [ "Haskell", "IORef", "Mutable" ]
---

### Introduction

Haskell goes to great lengths to control state but one way you can achieve mutable state in Haskell is by use of [IORef](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-IORef.html). `IORef` gives you the ability to assign a reference to a variable in the [IO monad](http://www.haskell.org/ghc/docs/latest/html/libraries/base/System-IO.html#t:IO). This at least decorates your code in such a way that it's obvious to you, the developer and Haskell that there will be side effects. Today's post, I'll create a very simple example usage of `IORef`. We'll construct a counter that we can increment and decrement.

### Declaring our type

{% highlight haskell %}
import Data.IORef                        
                                         
data Counter = Counter { x :: IORef Int }
{% endhighlight %} 

First of all, we import `Data.IORef` to give us access to `IORef`. We declare our counter data type using record style, the only member of which is the value that counts. It's an `IORef Int` to mean it references a variable in the `IO` monad that will be of type `Int`. So, it's not so blatant that you're dragging the integer value around with you, rather you're dragging something closer to a pointer to the value or reference. To build one of our types, we need to use [newIORef](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-IORef.html#v:newIORef) which references our actual value and we wrap it up in our `Counter` data type.

{% highlight haskell %}
makeCounter :: Int -> IO Counter        
makeCounter i = do iref <- newIORef i   
                   return (Counter iref)
{% endhighlight %}

`makeCounter` takes in an initial integer that will seed our counter and returns a `Counter` in the `IO` monad. Getting our hands on the reference and doing something with it is pretty simple with the use of [modifyIORef](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-IORef.html#v:modifyIORef). Using this information, we can increment our counter with the following function.

{% highlight haskell %}
incCounter :: Int -> Counter -> IO ()            
incCounter i (Counter c) = do modifyIORef c (+ i)
{% endhighlight %}

`modifyIORef` actually gives us the ability to pass a function to modify the referenced value. Be careful with `modifyIORef` though. As with a lot of things in Haskell, this is lazy. We're operating on `IO` actions here so it's all "promises to do something" or "will do it later when I need to" type operations, so repeatedly calling this without emitting the value will make these promises pile up. There is a strict and non-lazy evaluated version called `modifyIORef'`. Finally, when we want to get our hands on the referenced value and do something with it (in our example here, we'll just present it to screen) we use [readIORef](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-IORef.html#v:readIORef). `readIORef` will take our `IORef` and just made it a value in the `IO` monad meaning we can simply use `<-` to emit the value.

{% highlight haskell %}
showCounter :: Counter -> IO ()               
showCounter (Counter c) = do c' <- readIORef c
                             print(c')        
{% endhighlight %}

This all pretty straight forward. Seeing this Counter run from GHCI is pretty straight forward.

{% highlight text %}
λ> c <- makeCounter 1
λ> showCounter c
1
λ> incCounter 1 c
λ> showCounter c
2
{% endhighlight %}

There you have it - mutable state with IORef.