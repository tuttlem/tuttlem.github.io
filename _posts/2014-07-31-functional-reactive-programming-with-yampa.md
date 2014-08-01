---
layout: post
title: Functional Reactive Programming with Yampa
date: 2014-07-31
comments: false
categories: ["haskell", "frp", "yampa"]
---

### Introduction

[Functional reactive programming](http://en.wikipedia.org/wiki/Functional_reactive_programming) is a programming paradigm that allows a developer to express interesting events as a stream. Developers place pieces of code in this stream called signals which allow them to target and respond to the events that matter to them.

Haskell offers a library called [Yampa](http://www.haskell.org/haskellwiki/Yampa) for this field of computing. Yampa uses the [Arrow](http://www.haskell.org/arrows/) abstraction to allow developers to build/compose their signals.

In today's post, I'll take you through a few of the pieces I've learned about Yampa so that you can get started quicker.

### What is a Signal Function?

A signal function is what you'll put in the pipeline <em>or stream</em> of events so that you're able to respond to events and ultimately change the course of execution. A signal function is defined as having both input and output as so:

{% highlight haskell %}
data SF a b
{% endhighlight %}

I found it easier to think that you'll write functions that return signal functions. Better yet, you'll write arrow compositions by combining simpler arrows <em>(to make more complex arrows)</em> that will end up as an `SF` type.

### Built in Signal Functions

Out of the box, Yampa gives you some functions returning `SF` types all ready for you to put to work.

The first of these functions is `identity`.

{% highlight haskell %}
identity :: SF a a
{% endhighlight %}

As the type signature suggests, `identity` will return a signal function that will take the input that is given to it and return it.

The next of these functions is `constant`.

{% highlight haskell %}
constant :: b -> SF a b
{% endhighlight %}

`constant` wants an initial argument. It will return a signal function that gives you back that initially supplied value.

The next function is `time`.

{% highlight haskell %}
time :: SF a Time
{% endhighlight %}

This function is a little more interesting. Rather than echoing back to us information that's been supplied, `time` will give you the time that has passed. When moving through a stream of events, you'll always be working with respect to <strong>dt</strong> or time that has passed. 

### Running Signal Functions

Yampa provides us with the `embed` function which gives us the ability to run our signal functions over a pre-defined stream (or array).

{% highlight haskell %}
embed :: SF a b
      -> (a, [(DTime, Maybe a)])
      -> [b]
{% endhighlight %}

The `embed` function says: give me a signal function `SF a b` and a pair `(a, [(DTime, Maybe a)])` and it will give you back the resulting <em>modified</em> stream `[b]`. The second argument to this function could use a little more definition:

The first item in the pair, `a` is an initial value for the stream. It's where the stream starts. If you're dealing with mouse data, this could be `(0, 0)` as the origin point of the mouse cursor or `RobotStateOff` if you're controlling a robot or 0 if you're just animating an integer.

The second item in the pair, `[(DTime, Maybe a)]` is a list of the values to supply to your stream. The pair at each index of the list wants to know `DTime` (how much time has passed) and `Maybe a` (the associated value at this time).

A few practical examples may help clear up my explanations. To make a plain-old-number-example a little more interesting - let's say that we're trying to model the temperature of a cup of coffee as it cools down. We'll get sensor data from our virtual thermometer every 60 seconds. We can model our sensor data like so:

{% highlight haskell %}
let sensorData = (80.0, [(60.0, Just 74.6), (60.0, Just 68.9), (60.0, Just 61.5)])
{% endhighlight %}

So this "sensor data" that we've captured from somewhere is saying that:

* The coffee started at 80 degrees
* After 60 seconds, the temperature dropped to 74.6
* Next 60 seconds, to 68.9
* Next 60 seconds, to 61.5

Then we turned our sensor off. We didn't collect any more data than this.

Using the functions above:

{% highlight haskell %}
λ> embed (constant 77) sensorData
[77,77,77,77]
λ> embed identity sensorData
[80.0,74.6,68.9,61.5]
λ> embed time sensorData
[0.0,60.0,120.0,180.0]
{% endhighlight %}

As we went through above. `constant` just gave us a set of 77's back. This is what we supplied as the input to `constant`. `identity` gave us each of the temperature readings and `time` gave us the time intervals that had passed.

Using `embed` is a great way to see how signal functions react to test data, but what we really want to do is create our own signal functions and run them through.

### Creating signal functions

We compose some of the more fundamental, pre-provided signal functions to make more complex scenarios. In this case, we're going to say that the cup of coffee loses 0.001 degrees for every second that passes.

In this example, we'll say that the coffee cools down at 1 degree per second. Pretty unrealistic, but it'll do for the purposes of this example.

{% highlight haskell %}
let cooling t0 = (constant (-1) >>> integral) >>^ (+ t0)
{% endhighlight %}

Breaking this down, we're integrating our "cool-down constant" of -0.001 over time using `integral` and then applying this to the original temperature passed in `(+ t0)`. Testing this out now using `embed` and some new test data:

{% highlight haskell %}
λ> embed (cooling (80.0 :: Double)) (Nothing, take 20 $ cycle [(1.0, Nothing)])
[80.0,79.0,78.0,77.0,76.0,75.0,74.0,73.0,72.0,71.0,70.0,69.0,68.0,67.0,66.0,65.0,64.0,63.0,62.0,61.0,60.0]
{% endhighlight %}

We can see, using our `cooling` function that the temperature of our coffee is getting colder at a constant rate. We can re-write this function using arrow notation and a `proc` block to make it a little more readable.

{% highlight haskell %}
cooling :: Double -> SF () (Double)
cooling t0 = proc input -> do
               t0' <- integral >>^ (+ t0) -< -1
               returnA -< t0'
{% endhighlight %}

Our function takes the current temperature of the coffee. It returns a `SF () (Double)`, which means that it doesn't have any input to work with `()`, but will be returning our new temerature `(Double)`.

### switch; the reactive part

The whole idea of this programming paradigm is to respond to changes. Whilst we've been modifying attributes of our program with respect to time, we really need to conditionally change course at runtime. This is where `switch` comes into the picture.

{% highlight haskell %}
switch :: SF a (b, Event c)
       -> (c -> SF a b)
       -> SF a b
{% endhighlight %}

From the Yampa [page on switches](http://www.haskell.org/haskellwiki/Yampa/switch).

> A switch in Yampa provides change of behavior of signal functions (SF) during runtime. The function 'switch' is the simplest form which can only be switched once. The signature is read like this: "Be a SF which is always fed a signal of type 'in' and returns a signal of type 'out'. Start with an initial SF of the same type but which may also return a transition event of type 'Event t'. In case of an Event, Yampa feeds the variable 't' to the continuation function 'k' which produces the new SF based on the variable 't', again with the same input and output types." 

Going through the parameters, `switch` expects a signal function `SF a (b, Event c)`. This function is what determines if we actually need to switch <em>or react</em> to a particular condition. The next input parameter `(c -> SF a b)` is a function producing a signal function. It's what we'll switch to if we receive the correct event information supplied from the output of the first parameter.

We'll create a new signal function that will employ both `switch` and our original `cooling` function so that when the temperature reaches a certain point, it'll just maintain that temperature. Sort of like when the temperature reaches room temperature.

{% highlight haskell %}
coolingWithFloor :: Double -> SF () (Double)
coolingWithFloor t0 = switch cooling' atRoomTemp
    where cooling' = proc _ -> do
                       t' <- cooling t0 -< ()
                       e <- edge -< t' <= 18
                       returnA -< (t', e `tag` t')
          atRoomTemp _ = (constant 18)
{% endhighlight %}

There's a little bit to explain in this one. `cooling'` is our new cooling function. It still uses the original `cooling` function under the covers. `cooling'` then uses `edge` which takes a `Bool` as its input and returns a `Event ()`. We finally prepare the signal function for return.

The use of the `tag` function will just perform an `fmap` of `t'` over `e`. What's interesting is that if the case is met in the call to `edge`, we'll receive back a `Event a` - otherwise it'll just be `NoEvent`. This is really the meat and potatoes as to what's driving the decision that we're leaving up to `switch`.

Our test that we're performing is `t' <= 18`, so it makes sense that the function that we'd switch to just sends 18 out. Once the temperature reaches 18, that's where it'll stay. `embed` confirms our requirements through execution.

{% highlight haskell %}
λ> embed (coolingWithFloor 25)
         ((), take 10 $ cycle [(1.0, Just ())])
[25.0,24.0,23.0,22.0,21.0,20.0,19.0,18.0,18.0,18.0,18.0]
{% endhighlight %}

There are other alternatives to `switch` as well that each have their own nuances. `rSwitch` allows you to specify the signal function to move to using the `Event` value. `kSwitch` allows you to freeze a signal function and use its state in a continuation, later on. 

### reactimate

Up until this point, we've been running all of our simulations through `embed`. This has been good for our testing purposes however Yampa also provides the `reactimate` function, which guided by our configurations will manage the stream that our signal functions work on - for us.

{% highlight haskell %}
reactimate
  :: IO a
     -> (Bool -> IO (DTime, Maybe a))
     -> (Bool -> b -> IO Bool)
     -> SF a b
     -> IO ()
{% endhighlight %}

`reactimate` takes 4 arguments.

`IO a` is an `IO` action that performs the initialisation for the process. Here's where you'd get ready for the stream to start.

The second parameter is our input function. <em>Ignore</em> the `Bool` input argument. From all of the reading that I've done, this isn't used. This parameter will be repeatedly called by `reactimate` to feed the stream with sensor data.

The third parameter is our output function. What do you want to do with the data? What ever it is, it goes into this function. The `Bool` input argument is again ignored however, the output of this action determines if `reactimate` continues working. `True` will stop the process for us.

The final input parameter is our signal function that's processing the stream.

Here is `coolingWithFloor` being hosted by `reactimate`, with the current temperature being written to the console.

{% highlight haskell %}
λ> reactimate (return ())
              (\_ -> return (1.0, Nothing))
              (\_ b -> (putStrLn $ show b) >> return False)
              (coolingWithFloor 25.0)
25.0
24.0
23.0
22.0
21.0
20.0
19.0
18.0
18.0
18.0
18.0
{% endhighlight %}

That's it for today's post on functional reactive programming in Haskell using Yampa.


