---
layout: post
title: Writing Your Own Lisp Interpreter in Haskell - Part 4
date: 2025-02-16
comments: false
categories: [ haskell, lisp ]
---


# Introduction

In the [previous post]({% post_url 2025-02-16-writing-your-own-lisp-interpreter-in-haskell-part-3 %}) we added 
conditionals to our basic Lisp interpreter. Now, it's time to introduce list manipulation – one of Lisp’s most 
fundamental features.

This update brings support for:

* **Pairs** (`cons`), first element (`car`), and rest (`cdr`)
* **List predicates** (`null?`)
* **Common list operations** (`append`, `length`, `reverse`)
* **Proper parsing of quoted lists** (`'(...)`)

# Pairs and Lists

In Lisp, lists are built from pairs (cons cells). Each pair contains a head (car) and a tail (cdr). We'll add `Pair` 
into our `LispVal` data type:

{% highlight haskell %}
data LispVal
    = Atom String
    | Number Integer
    | Bool Bool
    | String String
    | List [LispVal]
    | Pair LispVal LispVal  
    | Lambda [String] LispVal Env
    | BuiltinFunc ([LispVal] -> ThrowsError LispVal)
{% endhighlight %}

This allows us to represent both lists and dotted pairs like:

{% highlight lisp %}
(cons 1 2)    ;; (1 . 2)
(cons 1 '(2)) ;; (1 2)
{% endhighlight %}

# `cons`, `car`, and `cdr`

[`cons`](https://en.wikipedia.org/wiki/Cons#:~:text=In%20computer%20programming%2C%20cons%20(%2F,or%20pointers%20to%20two%20values.)) 
is a function in most dialects of Lisp that *cons*tructs memory objects which hold two values or pointers to two values.

{% highlight haskell %}
cons :: [LispVal] -> ThrowsError LispVal
cons [x, List xs] = return $ List (x : xs)  
cons [x, y]       = return $ Pair x y       
cons args         = throwError $ NumArgs 2 args
{% endhighlight %}

This allows us to build pairs and lists alike:

{% highlight lisp %}
(cons 1 2)       ;; (1 . 2)
(cons 1 '(2 3))  ;; (1 2 3)
{% endhighlight %}

(`car` and `cdr`)[https://en.wikipedia.org/wiki/CAR_and_CDR] are list primitives that allow you to return the first or 
second component of a pair.

The expression `(car (cons x y))` evaluates to `x`, and `(cdr (const x y))` evaluates to `y`.

{% highlight haskell %}
car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = return x
car [Pair x _]     = return x  
car [List []]      = throwError $ TypeMismatch "Cannot take car of empty list" (List [])
car [arg]          = throwError $ TypeMismatch "Expected a pair or list" arg
car args           = throwError $ NumArgs 1 args

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)] = return $ List xs  
cdr [Pair _ y]      = return y          
cdr [List []]       = throwError $ TypeMismatch "Cannot take cdr of empty list" (List [])
cdr [arg]           = throwError $ TypeMismatch "Expected a pair or list" arg
cdr args            = throwError $ NumArgs 1 args
{% endhighlight %}

We can now uwe these functions to work with our lists and pairs:

{% highlight lisp %}
(car '(1 2 3))   ;; 1
(car '(a b c))   ;; a
(car '(5 . 6))   ;; 5

(cdr '(1 2 3))   ;; (2 3)
(cdr '(a b c))   ;; (b c)
(cdr '(5 . 6))   ;; 6
{% endhighlight %}

# Checking for Empty Lists

We need a way to determine if our list is empty, and we do that with `isNull`:

{% highlight haskell %}
isNull :: [LispVal] -> ThrowsError LispVal
isNull [List []] = return $ Bool True
isNull [_]       = return $ Bool False
isNull args      = throwError $ NumArgs 1 args
{% endhighlight %}

This is pretty straight forward to use:

{% highlight lisp %}
(null? '())      ;; #t
(null? '(1 2 3)) ;; #f
{% endhighlight %}

# Extending List Operations

Going a little bit further now, we can easily implement `append`, `length`, and `reverse`.

{% highlight haskell %}
listAppend :: [LispVal] -> ThrowsError LispVal
listAppend [List xs, List ys] = return $ List (xs ++ ys)
listAppend [List xs, y] = return $ List (xs ++ [y])  
listAppend [x, List ys] = return $ List ([x] ++ ys)  
listAppend args = throwError $ TypeMismatch "Expected two lists or a list and an element" (List args)

listLength :: [LispVal] -> ThrowsError LispVal
listLength [List xs] = return $ Number (toInteger (length xs))
listLength [arg] = throwError $ TypeMismatch "Expected a list" arg
listLength args = throwError $ NumArgs 1 args

listReverse :: [LispVal] -> ThrowsError LispVal
listReverse [List xs] = return $ List (reverse xs)
listReverse [arg] = throwError $ TypeMismatch "Expected a list" arg
listReverse args = throwError $ NumArgs 1 args
{% endhighlight %}

These functions allow us to perform some more interesting processing of our lists:

{% highlight lisp %}
(append '(1 2) '(3 4))  ;; (1 2 3 4)
(append '(a b) 'c)      ;; (a b c)
(append 'a '(b c))      ;; (a b c)

(length '(1 2 3 4 5))   ;; 5
(length '())            ;; 0

(reverse '(1 2 3 4 5))   ;; (5 4 3 2 1)
(reverse '())            ;; ()
{% endhighlight %}

# Quoted Lists

Finally, Lisp allows `shorthand notation for quoting lists`. For example, `'(1 2 3)` is equivalent to `(quote (1 2 3))`.

{% highlight haskell %}
parseQuote :: Parser LispVal
parseQuote = do
    char '\''
    expr <- parseExpr 
    return $ List [Atom "quote", expr]
{% endhighlight %}

# Conclusion

Our Lisp interpreter is now becoming a little more sophisticated. List processing is so fundamental to how Lisp operates 
that we needed to get this implemented as soon as possible. The code for this particular article is [available up on GitHub](https://github.com/tuttlem/hlisp/releases/tag/part4) 
to pull down and take a look at.

{% highlight lisp %}
(define names '("Sally" "Joe" "Tracey" "Bob"))
("Sally" "Joe" "Tracey" "Bob")

(reverse names)
("Bob" "Tracey" "Joe" "Sally")

(car (cdr (reverse names)))
"Tracey"

(append names '("Stacey" "Peter"))
("Sally" "Joe" "Tracey" "Bob" "Stacey" "Peter")
{% endhighlight %}