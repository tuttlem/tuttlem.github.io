---
layout: post
title: Doing math with bash
date: 2015-08-18
comments: false
categories: [ "math", "bash", "shell" ]
---

[bash](http://www.gnu.org/software/bash/) can be used to perform simple arithmatic when needed. It is only limited to integer mathematics though. You can see this by using the [expr](http://www.tldp.org/LDP/abs/html/moreadv.html#EXPRREF).

An excerpt about `expr` from the link above:

> All-purpose expression evaluator: Concatenates and evaluates the arguments according to the operation given (arguments must be separated by spaces). Operations may be arithmetic, comparison, string, or logical.

Some trivial example usage shows that you can get some quick results if you only need integer math.

{% highlight bash %}
$ expr 1 + 1 
2
$ expr 1 - 7
-6
$ expr 6 \* 7
42
$ expr 6.5 \* 7
expr: non-integer argument
{% endhighlight %}

At this point, you could probably go for the full-nuclear option and get [perl](https://www.perl.org/) or [python](https://www.python.org/) to perform floating point calculations; but to keep things a little more shell oriented, you can go with a lighter-weight option, [bc](http://www.gnu.org/software/bc/manual/html_mono/bc.html).

`bc` is <em>an arbitrary precision calculator language</em>. Much like every other good shell tool, you can invoke it so that it'll take its input from `STDIN` and return its output to `STDOUT`. Here are some example invocations:

{% highlight bash %}
$ echo "1+1" | bc
2
$ echo "1.9+1" | bc
2.9
$ echo "76/5" | bc
15
$ echo "scale=2; 76/5" | bc
15.20
{% endhighlight %}

You can see that if you want precision on your answers from integer inputs, you'll need to set the `scale` variable to suit. Only feeding in static values is a bit basic though. To put this to work, you just need some variable data at hand.

What's the percentage battery left on this notebook?

{% highlight bash %}
$ echo "scale=2;" $(cat /sys/class/power_supply/BAT0/charge_now) / $(cat /sys/class/power_supply/BAT0/charge_full) | bc
.30
{% endhighlight %}

<em>Not much!</em>


