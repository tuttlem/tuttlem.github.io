---
layout: post
title: Approximation with the Monte Carlo method
date: 2015-04-18
comments: false
categories: [ "monte carlo", "math", "estimation" ]
---

An interesting way of finding values that fall within a domain is to perform random sample analysis with the [Monte Carlo method](http://en.wikipedia.org/wiki/Monte_Carlo_method). This method of finding values relies on random values (lots of them) being measured against some form of deterministic calculation to determine if the value falls within the source function's scope.

In today's post, I'm going to illustrate how to use this method in some practical scenarios. 

### Approximating π

To approximate the value of π, we're going to treat a square containing a quarter of a unit circle with a lot of random data. We only treat a quarter of a circle (which will contain angles 0 through 90) as we can easily mirror image a single quarter 4 times. Mathematically, you can consider the ratio of the circle's area with respect to the square that contains it.

{% highlight text %}
Circle area = πr^2
Square area = 2r^2 + 2r^2
            = 4r^2

Ratio       = πr^2 / 4r^2
            = π/4
{% endhighlight %}


For every point that we randomly sample, the following must be true in order for us to consider the point as satisfying the circle:

{% highlight text %}
rx^2 + ry^2 <= radius^2
{% endhighlight %}

This tells us, that from the midpoint `0,0`, if the `x` and `y` values that we've randomly selected are within the bounds of the radius; we'll consider it as "in".

Once we've sampled <em>enough</em> data, we'll take the ratio of points that are "in" and points that are "out". We are still only dealing with a quarter of a circle, so we'll multiply our result out as well and we should get close to π if we've sampled enough data. Here's how it looks in python:

{% highlight python %}
runs = 1000000
radius = 1

# get a batch of random points
rand_points = map(lambda x: (random() * radius, random() * radius), range(runs))

# filter out the points that satisfy our equation
in_points = filter(lambda (x, y): ((x * x) + (y * y)) <= (radius * radius), rand_points)

# calculate the ratio of points in the circle vs. points out of the circle
ratio = float(len(in_points)) / float(runs)

# multiply this figure by 4 to get all 4 quadrants considered
estimate = ratio * 4
{% endhighlight %}

`runs` is the number of points that we're going to sample. `radius` is only defined to be clear. If you were to change the radius of the tested area, your output ratio would need to be adjusted also.

Running this code a few times, I get the following results:

{% highlight text %}
3.140356
3.14274
3.14064
3.142
3.140664
{% endhighlight %}

### Area under a curve

When it comes to finding the area under a curve, nothing really beats numeric integration. In some cases though, your source function doesn't quite allow for integration. In these cases, you can use a Monte Carlo simulation to work it out. For the purposes of this post though, I'll work with `x^2`.

Let's integrate it to begin with and work out what the area is between the x-axis points 0 and 3.

{% highlight text %}
 f(x) = x^2
ʃf(x) = x^3/3

area  = ʃf(3) - ʃf(0)
      = 9
{% endhighlight %}

So we're looking for a value close to `9`. It's also important to note the values of our function's output at the start of where we want to take the area from to the end as this will setup the bounds of our test:

{% highlight text %}
f(x) = x^2
f(0) = 0
f(3) = 9
{% endhighlight %}

The area that we'll be testing from is `0, 0` to `3, 9`. The following code looks very similar to the π case. It has been adjusted to test the area and function:

{% highlight python %}
runs = 1000000
max_x = 3
max_y = 9

# get a batch of random points
rand_points = map(lambda x: (random() * max_x, random() * max_y), range(runs))

# filter out the points that satisfy our equation
in_points = filter(lambda (x, y): y <= (x * x), rand_points)

# calculate the ratio of points in the curve area vs. points outside
ratio = float(len(in_points)) / float(runs)

# the estimate is the ratio over the area of the rectangle
estimate = ratio * (max_x * max_y)
{% endhighlight %}

Here's some example outputs. Remember, our answer is `9`; we want something close to that:

{% highlight text %}
9.015219
9.008199
8.986761
8.998317
9.006282
8.995995
9.00693
{% endhighlight %}

These are only a couple of the many applications that you can use these for. Good luck and happy approximating.