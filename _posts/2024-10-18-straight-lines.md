---
layout: post
title: Straight lines
date: 2024-10-18
comments: false
categories: [ "math", "lines", "haskell" ]
---

# Introduction

In mathematics, the straight line equation $$ y = mx + c $$ is one of the simplest yet most foundational equations in 
both algebra and geometry. It defines a linear relationship between two variables, $$ x $$ and $$ y $$, where $$ m $$ 
represents the slope (or gradient) of the line, and $$ c $$ is the y-intercept, the point where the line crosses the 
y-axis.

This article explores key concepts related to the straight line equation, interesting properties, and how we can use 
Haskell to implement some useful functions.

# Understanding the Equation

The equation $$ y = mx + c $$ allows us to describe a straight line in a two-dimensional plane. Here's a breakdown of 
its components:

- $$ m $$: The slope, which measures how steep the line is. It's defined as the change in $$ y $$ divided by the change in $$ x $$ , or $$ \frac{\Delta y}{\Delta x} $$.
- $$ c $$: The y-intercept, which is the value of $$ y $$ when $$ x = 0 $$.

One of the key properties of this equation is that for every unit increase in $$ x $$, the value of $$ y $$ increases 
by $$ m $$. We can illustrate this behavior using some Haskell code.

# Basic Line Function in Haskell

Let's implement the basic straight line function in Haskell. This function will take $$ m $$, $$ c $$, and $$ x $$ as 
inputs and return the corresponding $$ y $$ value.

{% highlight haskell %}
lineFunction :: Float -> Float -> Float -> Float
lineFunction m c x = m * x + c
{% endhighlight %}

This function calculates $$ y $$ for any given $$ x $$ using the slope $$ m $$ and y-intercept $$ c $$.

# Parallel and Perpendicular Lines

An interesting aspect of lines is how they relate to each other. If two lines are parallel, they have the same slope. 
If two lines are perpendicular, the slope of one is the negative reciprocal of the other. In mathematical terms, if one 
line has a slope $$ m_1 $$, the perpendicular line has a slope of $$ -\frac{1}{m_1} $$.

We can express this relationship in Haskell using a function to check if two lines are perpendicular.

{% highlight haskell %}
arePerpendicular :: Float -> Float -> Bool
arePerpendicular m1 m2 = m1 * m2 == -1
{% endhighlight %}

This function takes two slopes and returns `True` if they are perpendicular and `False` otherwise.

# Finding the Intersection of Two Lines

To find the point where two lines intersect, we need to solve the system of equations:

$$ y = m_1x + c_1 $$
$$ y = m_2x + c_2 $$

By setting the equations equal to each other, we can solve for $$ x $$ and then substitute the result into one of the 
equations to find $$ y $$. The formula for the intersection point is:

$$ x = \frac{c_2 - c_1}{m_1 - m_2} $$

Here's a Haskell function that calculates the intersection point of two lines:

{% highlight haskell %}
intersection :: Float -> Float -> Float -> Float -> Maybe (Float, Float)
intersection m1 c1 m2 c2
    | m1 == m2  = Nothing -- Parallel lines never intersect
    | otherwise = let x = (c2 - c1) / (m1 - m2)
    y = m1 * x + c1
  in Just (x, y)
{% endhighlight %}

This function returns `Nothing` if the lines are parallel and `Just (x, y)` if the lines intersect.

# Conclusion

The straight line equation $$ y = mx + c $$ is a simple but powerful tool in both mathematics and programming. We've 
explored how to implement the line equation in Haskell, find parallel and perpendicular lines, and calculate 
intersection points. Understanding these properties gives you a deeper appreciation of how linear relationships work, 
both in theory and in practice.

By writing these functions in Haskell, you can model and manipulate straight lines in code, extending these basic 
principles to more complex problems.


