---
layout: post
title: Writing Your Own Lisp Interpreter in Haskell - Part 5
date: 2025-02-16
comments: false
categories: [ haskell, lisp ]
---

# Introduction

In our [previous installment]({% post_url 2025-02-16-writing-your-own-lisp-interpreter-in-haskell-part-4 %}), we added 
**basic list operations** to our Lisp interpreter, including `cons`, `car`, `cdr`, `append`, and `reverse`.

Now, we are expanding that functionality by introducing **higher-order list functions**:
- `map`
- `filter`
- `foldl` and `foldr`
- `sort`

Additionally, we're introducing **string manipulation functions** that allow us to treat strings as lists of characters:
- `string->list`
- `list->string`

These changes bring our Lisp interpreter closer to standard Scheme-like behavior.

If you're following along, you can find the updated implementation for this article [here](https://github.com/tuttlem/hlisp/releases/tag/part5).

# Lambda Expressions

A [lambda function](https://en.wikipedia.org/wiki/Anonymous_function) in computer programming is an anonymous function 
that we use to pass to higher-order functions.

To support `map`, `filter`, and `fold`, we **implemented lambda functions properly**. Previously, our Lisp could 
**parse** lambdas but couldn't correctly **apply** them.

## Adding Lambda Support

We modified `eval` to properly **capture the current environment** and store parameter names:

{% highlight haskell %}
eval env (List [Atom "lambda", List params, body]) =
    case mapM getParamName params of
        Right paramNames -> return $ Lambda paramNames body env
        Left err -> throwError err
  where
    getParamName (Atom name) = Right name
    getParamName badArg = Left $ TypeMismatch "Expected parameter name" badArg
{% endhighlight %}

When a lambda function is applied, it creates a **new local environment** that maps function parameters to actual 
arguments.

{% highlight haskell %}
apply (Lambda params body closure) args = do
    env <- liftIO $ readIORef closure
    if length params == length args
        then do
            let localEnv = Map.union (Map.fromList (zip params args)) env
            newEnvRef <- liftIO $ newIORef localEnv
            eval newEnvRef body
        else throwError $ NumArgs (length params) args
{% endhighlight %}

## Example

Now, we can define and call lambda functions:

{% highlight lisp %}
(define square (lambda (x) (* x x)))
(square 5)  ;; 25
{% endhighlight %}

# Higher-Order List Functions

Now that lambda functions work, we can introduce **list processing functions**.

## Map

`map` applies a function to each element in a list and returns a new list.

{% highlight haskell %}
listMap :: [LispVal] -> IOThrowsError LispVal
listMap [Lambda params body env, List xs] =
    List <$> mapM (\x -> apply (Lambda params body env) [x]) xs
listMap args = throwError $ TypeMismatch "Expected a function and a list" (List args)
{% endhighlight %}

## Example
{% highlight lisp %}
(map (lambda (x) (* x 2)) '(1 2 3 4))
;; => (2 4 6 8)
{% endhighlight %}

## Filter

`filter` removes elements that don't satisfy a given predicate.

{% highlight haskell %}
listFilter :: [LispVal] -> IOThrowsError LispVal
listFilter [func@(Lambda _ _ _), List xs] = do
    filtered <- filterM (\x -> do
        result <- apply func [x]
        case result of
            Bool True  -> return True
            Bool False -> return False
            _          -> throwError $ TypeMismatch "Expected boolean return" result
        ) xs
    return $ List filtered
listFilter args = throwError $ TypeMismatch "Expected a function and a list" (List args)
{% endhighlight %}

## Example

{% highlight lisp %}
(filter (lambda (x) (> x 5)) '(2 4 6 8))
;; => (6 8)
{% endhighlight %}

## Fold (Reduce)

`foldl` and `foldr` accumulate values from left to right or right to left.

{% highlight haskell %}
listFoldL :: [LispVal] -> IOThrowsError LispVal
listFoldL [Lambda params body env, initial, List xs] =
    foldM (\acc x -> apply (Lambda params body env) [acc, x]) initial xs
listFoldL args = throwError $ TypeMismatch "Expected a function, initial value, and a list" (List args)
{% endhighlight %}

{% highlight haskell %}
listFoldR :: [LispVal] -> IOThrowsError LispVal
listFoldR [Lambda params body env, initial, List xs] =
    foldM (\acc x -> apply (Lambda params body env) [x, acc]) initial (reverse xs)
listFoldR args = throwError $ TypeMismatch "Expected a function, initial value, and a list" (List args)
{% endhighlight %}

## Example

{% highlight lisp %}
(foldl (lambda (acc x) (+ acc x)) 0 '(1 2 3 4 5))
;; => 15
{% endhighlight %}

This works like `(((((0 + 1) + 2) + 3) + 4) + 5)`.

Whilst swapping out `foldr` for `foldl` in this scenario gives us the same result, it's very different in execution. 
It ends up working like `(1 + (2 + (3 + (4 + (5 + 0)))))`.

# Sort

`sort` Sorts a list in ascending order by default or with a custom comparator.

{% highlight haskell %}
listSort :: [LispVal] -> ThrowsError LispVal
listSort [List xs] =
    case xs of
        [] -> return $ List []
        (Number _:_) -> return $ List (sortBy compareNumbers xs)
        (String _:_) -> return $ List (sortBy compareStrings xs)
        _ -> throwError $ TypeMismatch "Cannot sort mixed types" (List xs)
  where
    compareNumbers (Number a) (Number b) = compare a b
    compareStrings (String a) (String b) = compare a b

listSort [Lambda params body closure, List xs] =
    case xs of
        [] -> return $ List []
        _  -> throwError $ TypeMismatch "Custom sorting requires ThrowsErrorIO" (List xs)
        -- If you later want custom sorting, you'd need `ThrowsErrorIO`
listSort args = throwError $ TypeMismatch "Expected a list (optionally with a comparator function)" (List args)
{% endhighlight %}

## Example

{% highlight lisp %}
(sort '(5 3 8 1 4))
;; => (1 3 4 5 8)
{% endhighlight %}

Optionally, the implementation allows you to specify a predicate to control the sort.

# String Manipulation 

Strings are **now treated as lists of characters** in our Lisp. This can sometimes make things easier to work with when 
dealing with strings.

## Example

We can convert a string into a list (and therefore operate on it like it's a list) by using `string->list`.

{% highlight haskell %}
stringToList :: [LispVal] -> ThrowsError LispVal
stringToList [String s] = return $ List (map (String . (:[])) s)
stringToList args = throwError $ NumArgs 1 args
{% endhighlight %}

{% highlight lisp %}
(string->list "hello")
;; => ("h" "e" "l" "l" "o")
{% endhighlight %}

We reverse this process with `list->string`.

{% highlight haskell %}
listToString :: [LispVal] -> ThrowsError LispVal
listToString [List chars] = case mapM extractChar chars of
    Right strList -> return $ String (concat strList)
    Left err -> throwError err
  where
    extractChar (String [c]) = Right [c]
    extractChar invalid = Left $ TypeMismatch "Expected a list of single-character strings" invalid
listToString args = throwError $ NumArgs 1 args
{% endhighlight %}

{% highlight lisp %}
(list->string '("h" "e" "l" "l" "o"))
;; => "hello"
{% endhighlight %}

# Conclusion

We now how have higher order functions and lambda functions controlling our list processing. Stay tuned for further 
updates as we add more features to our Lisp.
