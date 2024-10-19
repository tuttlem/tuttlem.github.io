---
layout: post
title: Quadratic equations
date: 2024-10-18
comments: false
categories: [ "maths", "quadratic", "haskell" ]
---

# Introduction

The quadratic equation is one of the fundamental concepts in algebra and forms the basis of many more complex topics in 
mathematics and computer science. It has the general form:

$$ ax^2 + bx + c = 0 $$

where $$ a $$, $$ b $$, and $$ c $$ are constants, and $$ x $$ represents the unknown variable.

In this post, we'll explore:
- What the quadratic equation represents
- How to solve it using the quadratic formula
- How to implement this solution in Haskell

# What Is a Quadratic Equation?

A quadratic equation is a second-degree polynomial equation. This means the highest exponent of the variable $$ x $$ is 2. 

Quadratic equations typically describe parabolas when plotted on a graph.

# The Quadratic Formula

The quadratic formula provides a method to find the values of $$ x $$ that satisfy the equation. The formula is:

$$ x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a} $$

Here, the expression $$ b^2 - 4ac $$ is called the **discriminant**, and it plays a key role in determining the nature 
of the solutions:

- **If the discriminant is positive**, the equation has two real and distinct roots.
- **If the discriminant is zero**, the equation has one real (repeated) root.
- **If the discriminant is negative**, the equation has two complex roots.

# Step-by-Step Solution

1. **Calculate the Discriminant**:
   The discriminant, $$ \Delta $$, is given by:
   $$ \Delta = b^2 - 4ac $$

2. **Evaluate the Roots**:
   Using the discriminant, you can find the roots by plugging the values into the quadratic formula:
   $$ x_1 = \frac{-b + \sqrt{\Delta}}{2a}, \quad x_2 = \frac{-b - \sqrt{\Delta}}{2a} $$

If $$ \Delta < 0 $$, the square root term involves imaginary numbers, leading to complex solutions.

# Haskell Implementation

Now let's translate this mathematical solution into a Haskell function. Haskell is a functional programming language 
with a strong emphasis on immutability and mathematical precision, making it an excellent choice for implementing 
mathematical algorithms.

Below, we'll create a function `quadraticSolver` that:
- Takes the coefficients $$ a $$, $$ b $$, and $$ c $$ as inputs.
- Computes the discriminant.
- Determines the nature of the roots based on the discriminant.
- Returns the roots of the quadratic equation.

{% highlight haskell %}
-- Haskell implementation of solving a quadratic equation
import Text.Printf (printf)

-- Function to solve the quadratic equation
quadraticSolver :: (RealFloat a, Show a) => a -> a -> a -> String
quadraticSolver a b c
    | discriminant > 0 = printf "Two real roots: x1 = %.2f, x2 = %.2f" x1 x2
    | discriminant == 0 = printf "One real root: x = %.2f" x1
    | otherwise = printf "Two complex roots: x1 = %.2f + %.2fi, x2 = %.2f - %.2fi" realPart imaginaryPart realPart imaginaryPart
  where
    discriminant = b^2 - 4 * a * c
    x1 = (-b + sqrt discriminant) / (2 * a)
    x2 = (-b - sqrt discriminant) / (2 * a)
    realPart = -b / (2 * a)
    imaginaryPart = sqrt (abs discriminant) / (2 * a)

-- Example usage
main :: IO ()
main = do
    putStrLn "Enter coefficients a, b, and c:"
    a <- readLn
    b <- readLn
    c <- readLn
    putStrLn $ quadraticSolver a b c
{% endhighlight %}

### Code Breakdown:

1. **Imports**:
   We import the `Text.Printf` module to format the output to two decimal places.

2. **quadraticSolver Function**:
    - This function takes three arguments: $$ a $$, $$ b $$, and $$ c $$.
    - It computes the discriminant using the formula $$ \Delta = b^2 - 4ac $$.
    - It checks the value of the discriminant using Haskell's guards (`|`), and based on its value, it computes the roots.
    - If the discriminant is negative, we compute the real and imaginary parts separately and display the complex roots in the form $$ x = a + bi $$.

3. **main Function**:
    - The main function prompts the user to input the coefficients $$ a $$, $$ b $$, and $$ c $$.
    - It then calls `quadraticSolver` to compute and display the roots.

### Example Run

Let’s assume we are solving the equation $$ x^2 - 3x + 2 = 0 $$, where $$ a = 1 $$, $$ b = -3 $$, and $$ c = 2 $$.

{% highlight plain %}
Enter coefficients a, b, and c:
1
-3
2
Two real roots: x1 = 2.00, x2 = 1.00
{% endhighlight %}

If we try solving the equation $$ x^2 + 2x + 5 = 0 $$, where $$ a = 1 $$, $$ b = 2 $$, and $$ c = 5 $$.

{% highlight plain %}
Enter coefficients a, b, and c:
1
2
5
Two complex roots: x1 = -1.00 + 2.00i, x2 = -1.00 - 2.00i
{% endhighlight %}

# Conclusion

The quadratic equation is a simple but powerful mathematical tool. In this post, we derived the quadratic formula, 
discussed how the discriminant affects the solutions, and implemented it in Haskell. The solution handles both real and 
complex roots elegantly, thanks to Haskell's functional paradigm.
