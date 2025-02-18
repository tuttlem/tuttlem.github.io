---
layout: post
title: Writing Your Own Lisp Interpreter in Haskell - Part 7
date: 2025-02-18
comments: false
categories: [ haskell, lisp ]
---

# Introduction

In this update, we're introducing **first-class functions** to our Lisp interpreter! This allows users to define and 
use functions just like they would in a real Lisp environment.

Before this update, function definitions looked like this:

{% highlight lisp %}
(define square (lambda (x) (* x x)))
(square 4) ;; Expected 16
{% endhighlight %}

This worked, but **wasn't very Lisp-like**. In Lisp, we usually define functions using a shorthand syntax:

{% highlight lisp %}
(define (square x) (* x x))
(square 4) ;; Expected 16
{% endhighlight %}

We're going to support **both** styles while keeping our implementation clean.

# Extending `define`

The first change we made was updating `eval` to **detect function definitions** and create a lambda automatically.

We extend `define` to detect **function definitions** and transform them into lambda expressions automatically:

{% highlight haskell %}
eval env (List [Atom "define", List (Atom funcName : params), body]) = do
    let paramNames = map extractParam params
    defineVar env funcName (Lambda paramNames body env)
  where
    extractParam (Atom name) = name
    extractParam nonAtom = error $ "Expected parameter name, got: " ++ show nonAtom
{% endhighlight %}

This means that:

- If `define` receives a **list** as the first argument, it assumes we're defining a function.
- The first element of that list (`funcName`) is **the function name**.
- The remaining elements (`params`) are the **parameter names**.
- The function body is stored as a **lambda expression**.

### Example 

With this change, both of these definitions are now equivalent:

{% highlight lisp %}
;; Using explicit lambda
(define square (lambda (x) (* x x)))

;; Using shorthand function definition
(define (square x) (* x x))

(square 4) ;; 16
{% endhighlight %}

This makes function definitions **much cleaner and easier to read**!

# Fixing `if` Expressions

While making these changes, we discovered a **bug** in our `if` implementation.

Previously, `if` **did not evaluate** the `then` or `else` branch before returning it:

{% highlight haskell %}
eval env (List [Atom "if", condition, thenExpr, elseExpr]) = do
    result <- eval env condition
    case result of
        Bool True  -> return thenExpr  
        Bool False -> return elseExpr  
        _          -> throwError $ TypeMismatch "Expected boolean in if condition" result
{% endhighlight %}

This meant that calling:

{% highlight lisp %}
(if (> 10 5) (* 2 2) (* 3 3))
{% endhighlight %}

Would **return** `(* 2 2)` instead of `4`! 

### The Fix 

We simply **added `eval env`** to ensure the chosen branch is evaluated before being returned:

{% highlight haskell %}
eval env (List [Atom "if", condition, thenExpr, elseExpr]) = do
    result <- eval env condition
    case result of
        Bool True  -> eval env thenExpr  
        Bool False -> eval env elseExpr  
        _          -> throwError $ TypeMismatch "Expected boolean in if condition" result
{% endhighlight %}

Now, `if` expressions behave correctly:

{% highlight lisp %}
(if (> 10 5) (* 2 2) (* 3 3)) ;; 4
(if (< 10 5) (* 2 2) (* 3 3)) ;; 9
{% endhighlight %}

# Recursion

With function definitions **and** the fixed `if` statement, we can now **write recursive functions!**

### Example: Factorial Function

{% highlight lisp %}
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(factorial 5) ;; 120
{% endhighlight %}

### Example: Fibonacci Sequence

{% highlight lisp %}
(define (fib n)
  (if (<= n 1)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(fib 10) ;; 55
{% endhighlight %}

This is a **huge milestone** 🎉 because **recursion is essential** in functional programming!

# Conclusion

In this update, we: 

* **Added shorthand function definitions** (e.g., `(define (square x) (* x x))`).  
* **Fixed `if` expressions** to evaluate branches properly.  
* **Enabled recursion**, making functions like `factorial` and `fib` possible.  
