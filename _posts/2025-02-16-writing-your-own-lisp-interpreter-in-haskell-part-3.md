---
layout: post
title: Writing Your Own Lisp Interpreter in Haskell - Part 3
date: 2025-02-16
comments: false
categories: [ haskell, lisp ]
---

# Introduction

In our [previous post]({% post_url 2025-02-15-writing-your-own-lisp-interpreter-in-haskell-part-2 %}), we introduced 
persistent variables into our Lisp interpreter, making it possible to store and retrieve values across expressions.

Now, it's time to make our Lisp smarter by adding conditionals and logic. 

In this post, we’ll extend our interpreter with:

* `if` expressions
* Boolean logic (`and`, `or`, `xor`, `not`)
* String support for conditionals
* Expanded numeric comparisons (`<=`, `>=`)

By the end of this post, you'll be able to write real conditional logic in our Lisp, compare both numbers and strings, 
and use logical expressions effectively.

# Adding `if` Statements

We start with the classic *Lisp conditional expression*:

{% highlight lisp %}
(if (< 10 5) "yes" "no")  ;; Expected result: "no"
(if (= 3 3) "equal" "not equal")  ;; Expected result: "equal"
{% endhighlight %}

We add support for this by adjusting `eval`:

{% highlight haskell %}
eval env (List [Atom "if", condition, thenExpr, elseExpr]) = do
    result <- eval env condition
    case result of
        Bool True  -> return thenExpr  -- Return without evaluating again
        Bool False -> return elseExpr  -- Return without evaluating again
        _          -> throwError $ TypeMismatch "Expected boolean in if condition" result
{% endhighlight %}

## Testing

We can see this in action now:

{% highlight text %}
λ> (if (> 10 20) "yes" "no")
"no"
λ> (if (= 42 42) "match" "no-match")
"match"
λ> (if #f 10 20)
20
{% endhighlight %}

# Expanding Boolean Logic

Now, we'll add some boolean operators that are standard in conditionals:

* `(and ...)` → Returns `#t` if all values are `#t`.
* `(or ...)` → Returns `#t` if at least one value is `#t`.
* `(xor ...)` → Returns `#t` if exactly one value is `#t`.
* `(not x)` → Returns `#t` if `x` is `#f`, otherwise returns `#f`.

These functions get added to `Eval.hs`:

{% highlight haskell %}
booleanAnd, booleanOr, booleanXor :: [LispVal] -> ThrowsError LispVal

booleanAnd args = return $ Bool (all isTruthy args)  -- Returns true only if all args are true
booleanOr args = return $ Bool (any isTruthy args)  -- Returns true if at least one arg is true

booleanXor args =
    let countTrue = length (filter isTruthy args)
    in return $ Bool (countTrue == 1)  -- True if exactly one is true

notFunc :: [LispVal] -> ThrowsError LispVal
notFunc [Bool b] = return $ Bool (not b)  -- Negates the boolean
notFunc [val] = throwError $ TypeMismatch "Expected boolean" val
notFunc args = throwError $ NumArgs 1 args

isTruthy :: LispVal -> Bool
isTruthy (Bool False) = False  -- Only #f is false
isTruthy _ = True  -- Everything else is true
{% endhighlight %}

These functions get added as primitives to our Lisp:

{% highlight haskell %}
primitives =
  [ ("not", BuiltinFunc notFunc),
    ("and", BuiltinFunc booleanAnd),
    ("or", BuiltinFunc booleanOr),
    ("xor", BuiltinFunc booleanXor)
  ]
{% endhighlight %}

## Testing

We can now exercise these new built-ins:

{% highlight text %}
λ> (and #t #t #t)
#t
λ> (or #f #f #t)
#t
λ> (xor #t #f)
#t
λ> (not #t)
#f
{% endhighlight %}

We now have a full suite of logical operators.

# Strings

Before this point, our Lisp has been very number based. Strings haven't really seen much attention as our focus has 
been on putting together basic functionality first. With conditionals being added into our system, it's time to give 
strings a little bit of attention.

First job is to expand `=` to *also* support strings.

{% highlight haskell %}
numericEquals [Number a, Number b] = return $ Bool (a == b)
numericEquals [String a, String b] = return $ Bool (a == b)  -- Added string support
numericEquals args = throwError $ TypeMismatch "Expected numbers or strings" (List args)
{% endhighlight %}

## Testing

We can see this in action now with a string variable:

{% highlight text %}
λ> (define name "Joe")
"Joe"
λ> (if (= name "Joe") "yes" "no")
"yes"
λ> (if (= name "Alice") "yes" "no")
"no"
{% endhighlight %}

# More Numeric Comparators

To round out all of our comparison operators, we throw in implementations for `<=` and `>=`.

{% highlight haskell %}
numericLessThanEq [Number a, Number b] = return $ Bool (a <= b)
numericLessThanEq args = throwError $ TypeMismatch "Expected numbers" (List args)

numericGreaterThanEq [Number a, Number b] = return $ Bool (a >= b)
numericGreaterThanEq args = throwError $ TypeMismatch "Expected numbers" (List args)
{% endhighlight %}

These also require registration in our primitive set:

{% highlight haskell %}
primitives =
  [ ("<=", BuiltinFunc numericLessThanEq),
    (">=", BuiltinFunc numericGreaterThanEq)
  ]
{% endhighlight %}

# Conclusion

We've added some great features to support conditional process here. As always [part 3](https://github.com/tuttlem/hlisp/releases/tag/part3) 
of the code to follow this tutorial is available.