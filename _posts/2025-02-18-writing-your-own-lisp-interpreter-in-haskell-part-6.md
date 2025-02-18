---
layout: post
title: Writing Your Own Lisp Interpreter in Haskell - Part 6
date: 2025-02-18
comments: false
categories: [ haskell, lisp ]
---

# Introduction

In this update, we extend our Lisp interpreter with **floating point numbers** and **floating point math functions**. 
This brings us closer to full numeric support, allowing for a much richer mathematical capability.

If you're following along, you can find the implementation for this article [here](https://github.com/tuttlem/hlisp/releases/tag/part6).

# Floating Point Type

Until now, our Lisp implementation only supported integers:

{% highlight haskell %}
data LispVal
    = Atom String
    | Number Integer
    | Bool Bool
    | String String
    | Float Double  -- Added floating point support
    | List [LispVal]
    | Pair LispVal LispVal
    | Lambda [String] LispVal Env
    | BuiltinFunc ([LispVal] -> ThrowsError LispVal)
    | BuiltinFuncIO ([LispVal] -> IOThrowsError LispVal)
{% endhighlight %}

We introduced a new constructor `Float Double` to represent floating point numbers.

## Parsing

We needed to update our parser to recognize floating point literals. We do this in such a way to operate alongside the 
current integer math that we currently support. We don't want to disturb the work that we've already done, so we'll use 
that effort here.

{% highlight haskell %}
parseInteger :: Parser LispVal
parseInteger = do
    sign <- optionMaybe (char '-')  -- Look for optional '-'
    digits <- many1 digit
    let number = read digits
    return $ Number $ case sign of
        Just _  -> -number
        Nothing -> number

parseFloat :: Parser LispVal
parseFloat = do
    sign <- optionMaybe (char '-')  -- Look for optional '-'
    whole <- many1 digit
    char '.'
    fractional <- many1 digit
    let number = read (whole ++ "." ++ fractional)
    return $ Float $ case sign of
        Just _  -> -number
        Nothing -> number

parseNumber :: Parser LispVal
parseNumber = try parseFloat <|> parseInteger
{% endhighlight %}

So, we changed the meaning of `parseNumber` from our original implementation. Instead of `parseNumber` handling only 
integers, we split the logic into `parseInteger` and `parseFloat`, ensuring both number types are correctly parsed

This ensures that expressions like `3.14` are correctly interpreted as floating point numbers, while still maintaining 
expressions like `3` being handled as integers.

One extra added feature here is handling **negative numbers**. We never observed the `-` symbol that can appear before 
some numbers, so we've fixed this in the parser at the same time.

# Numeric Coercion

Next, we needed to handle operations between integers and floats. 

Our previous numeric functions assumed **only integers**, so we modified them to **coerce** integers to floats when 
necessary. This means that we can use integers and floats together in our expressions.

{% highlight haskell %}
numericAdd, numericSub, numericMul, numericDiv :: [LispVal] -> ThrowsError LispVal

numericAdd [Number a, Number b] = return $ Number (a + b)
numericAdd [Float a, Float b] = return $ Float (a + b)
numericAdd [Number a, Float b] = return $ Float (fromIntegral a + b)
numericAdd [Float a, Number b] = return $ Float (a + fromIntegral b)
numericAdd args = throwError $ TypeMismatch "Expected numbers" (List args)
{% endhighlight %}

This same logic was applied to subtraction, multiplication, and division.

### Division Considerations

Division needed special attention because it must always return a float when dividing integers:

{% highlight haskell %}
numericDiv [Number a, Number b] =
    if b == 0 then throwError $ TypeMismatch "Division by zero" (Number b)
    else return $ Float (fromIntegral a / fromIntegral b)
numericDiv [Float a, Float b] = return $ Float (a / b)
numericDiv [Number a, Float b] = return $ Float (fromIntegral a / b)
numericDiv [Float a, Number b] = return $ Float (a / fromIntegral b)
numericDiv args = throwError $ TypeMismatch "Expected numbers" (List args)
{% endhighlight %}

This ensures that `(/ 3 2)` evaluates to `1.5` instead of performing integer division.

# Adding Floating Point Math Functions

With float support in place, we introduced **math functions** like `sin`, `cos`, `tan`, `exp`, `log`, and `sqrt`:

{% highlight haskell %}
import Prelude hiding (log)

numericSin, numericCos, numericTan, numericExp, numericLog, numericSqrt :: [LispVal] -> ThrowsError LispVal

numericSin [Float a] = return $ Float (sin a)
numericSin [Number a] = return $ Float (sin (fromIntegral a))
numericSin args = throwError $ TypeMismatch "Expected a number" (List args)

numericCos [Float a] = return $ Float (cos a)
numericCos [Number a] = return $ Float (cos (fromIntegral a))
numericCos args = throwError $ TypeMismatch "Expected a number" (List args)

numericTan [Float a] = return $ Float (tan a)
numericTan [Number a] = return $ Float (tan (fromIntegral a))
numericTan args = throwError $ TypeMismatch "Expected a number" (List args)

numericExp [Float a] = return $ Float (exp a)
numericExp [Number a] = return $ Float (exp (fromIntegral a))
numericExp args = throwError $ TypeMismatch "Expected a number" (List args)

numericLog [Float a] =
    if a <= 0 then throwError $ TypeMismatch "Logarithm domain error" (Float a)
    else return $ Float (log a)
numericLog [Number a] =
    if a <= 0 then throwError $ TypeMismatch "Logarithm domain error" (Number a)
    else return $ Float (log (fromIntegral a))
numericLog args = throwError $ TypeMismatch "Expected a positive number" (List args)

numericSqrt [Float a] =
    if a < 0 then throwError $ TypeMismatch "Square root of negative number" (Float a)
    else return $ Float (sqrt a)
numericSqrt [Number a] =
    if a < 0 then throwError $ TypeMismatch "Square root of negative number" (Number a)
    else return $ Float (sqrt (fromIntegral a))
numericSqrt args = throwError $ TypeMismatch "Expected a non-negative number" (List args)
{% endhighlight %}

We then added them to the built-in function table:

{% highlight haskell %}
primitives =
  [ ("sin", BuiltinFunc numericSin),
    ("cos", BuiltinFunc numericCos),
    ("tan", BuiltinFunc numericTan),
    ("exp", BuiltinFunc numericExp),
    ("log", BuiltinFunc numericLog),
    ("sqrt", BuiltinFunc numericSqrt)
  ]
{% endhighlight %}

# Testing

With these changes, we can now perform floating point math:

{% highlight lisp %}
(sin 0.0)   ;; 0.0
(cos 0.0)   ;; 1.0
(exp 1.0)   ;; 2.718281828
(log 10.0)  ;; 2.302585092
(sqrt 16.0) ;; 4.0
{% endhighlight %}

# Conclusion

In this update, we:

- Added **floating point support**.
- Added **negative** support.
- Introduced **numeric coercion** between integers and floats.
- Implemented **floating point math functions**.
- Ensured **division always returns a float**.

