---
layout: post
title: Writing Your Own Lisp Interpreter in Haskell - Part 2
date: 2025-02-15
comments: false
categories: [ haskell, lisp ]
---

# Introduction

In our [previous post]({% post_url 2025-02-15-writing-your-own-lisp-interpreter-in-haskell-part-1 %}) we put a very 
simple Lisp interpreter together that was capable of some very basic arithmetic. 

In today's update, we'll introduce variable definitions into our Lisp interpreter, allowing users to define and 
retrieve values persistently. This required a number of structural changes to support mutable environments, error 
handling, and function lookups.

All the changes here will set us up to do much more sophisticated things in our system.

If you're following along, you can find the implementation for this article [here](https://github.com/tuttlem/hlisp/releases/tag/part2).

# Mutable Environment

In order to store variables in our environment, we need to make it mutable. This way we can store new values in the 
environment as we define them.

`Env` was originally a pure `Map`:

{% highlight haskell %}
type Env = Map String LispVal
{% endhighlight %}

This meant that variable bindings were immutable and couldn't persist across expressions.

We changed `Env` to an `IORef (Map String LispVal)`, making it *mutable*:

{% highlight haskell %}
type Env = IORef (Map String LispVal)
{% endhighlight %}

We added `nullEnv` to create an empty environment:

{% highlight haskell %}
nullEnv :: IO Env
nullEnv = newIORef Map.empty
{% endhighlight %}

*Why?*

* This allows *variables to persist* across expressions.
* Future changes (like `set!` for modifying variables) require mutability.
* `IORef` enables safe concurrent updates in a controlled manner.

# REPL update

Now we need to update our REPL at the top level to be able to use this mutable state.

Previously, our REPL was just using the `primitiveEnv` value. 

{% highlight haskell %}
main :: IO ()
main = do
    putStrLn "Welcome to Mini Lisp (Haskell)"
    repl primitiveEnv
{% endhighlight %}

We now pass it in as a value. Note that the underlying types have changed.

{% highlight haskell %}
main :: IO ()
main = do
    env <- primitiveEnv  -- Create a new environment
    putStrLn "Welcome to Mini Lisp (Haskell)"
    repl env
{% endhighlight %}

**Why?**

* The REPL now **uses a mutable environment** (`primitiveEnv`).
* This ensures **variables persist across expressions** instead of resetting each time.

# Variable Definition

We introduced `defineVar` to allow defining variables:

{% highlight haskell %}
defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var val = do
    env <- liftIO $ readIORef envRef  -- Read environment
    liftIO $ writeIORef envRef (Map.insert var val env)  -- Update environment
    return val
{% endhighlight %}

This enables us to define variables like this:

{% highlight lisp %}
(define x 10)
{% endhighlight %}

`defineVar` reads the current environment, updates it, and then writes it back.

# Evaluation

Probably the biggest change is that our evaluation no longer returns just a `ThrowsError LispVal`.

{% highlight haskell %}
eval :: Env -> LispVal -> ThrowsError LispVal
{% endhighlight %}

This has had to be upgraded to support our `IO` activity as we now have mutable state.

{% highlight haskell %}
eval :: Env -> LispVal -> IOThrowsError LispVal
{% endhighlight %}

This change allows eval to interact with mutable variables stored in `Env` and perform IO actions when updating 
environment bindings

We also added parsing support for `define`:

{% highlight haskell %}
eval env (List [Atom "define", Atom var, expr]) = do
    val <- eval env expr
    defineVar env var val
{% endhighlight %}

# Variable Lookup

Our `lookupVar` now needs an upgrade:

{% highlight haskell %}
lookupVar :: Env -> String -> ThrowsError LispVal
lookupVar env var = case Map.lookup var env of
    Just val -> Right val
    Nothing  -> Left $ UnboundVar var
{% endhighlight %}

It's not designed to work in a mutable `IORef` environment. We create `getVar` to accomodate.

{% highlight haskell %}
getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef  -- Read environment from IORef
    case Map.lookup var env of
        Just val -> return val
        Nothing  -> throwError $ UnboundVar ("Undefined variable: " ++ var)
{% endhighlight %}

This now allows variables to be defined and retrieved across multiple expressions.

# Builtins

Previously, built-in functions were *not* stored as part of the environment. 

{% highlight haskell %}
primitives :: [(String, LispVal)]
primitives =
  [ ("+", BuiltinFunc numericAdd),
    ("-", BuiltinFunc numericSub),
    ("*", BuiltinFunc numericMul),
    ("/", BuiltinFunc numericDiv)
  ]
{% endhighlight %}

This has changed now with `primitiveEnv` which will store a set of these.

{% highlight haskell %}
primitiveEnv :: IO Env
primitiveEnv = newIORef (Map.fromList primitives)
{% endhighlight %}

This change enables us to dynamically add more built-in functions in the future.

# Fixing `eval`

With the introduction of `IO` into our program, our evaluation logic needed updates to handle variable bindings 
correctly.

{% highlight haskell %}
eval env (List (Atom func : args)) = do
    func' <- eval env (Atom func)
    args' <- mapM (eval env) args
    apply func' args'
{% endhighlight %}

Now, we'll look up the function in the environment:

{% highlight haskell %}
eval env (List (Atom func : args)) = do
    func' <- getVar env func  -- Look up function in the environment
    args' <- mapM (eval env) args  -- Evaluate arguments
    apply func' args'
{% endhighlight %}

Now, we'll find any of our functions in the environment itself.

# Fixing `apply`

Now, we need to look at the `apply` function.

{% highlight haskell %}
apply (BuiltinFunc f) args = f args
{% endhighlight %}

We add support for functions out of the environment with the `liftThrows` helper:

{% highlight haskell %}
apply (BuiltinFunc f) args = liftThrows $ f args
{% endhighlight %}

Provision is also added for user-defined functions (`lambda`):

{% highlight haskell %}
apply (Lambda params body closure) args = do
    env <- liftIO $ readIORef closure  -- Read function's closure environment
    if length params == length args
        then eval closure body
        else throwError $ NumArgs (length params) args
{% endhighlight %}

# `ThrowsError`

Previously, `ThrowsError` was used for error handling:

{% highlight haskell %}
type ThrowsError = Either LispError
{% endhighlight %}

However, since we now interact with `IO`, we introduce `IOThrowsError`:

{% highlight haskell %}
type IOThrowsError = ExceptT LispError IO
{% endhighlight %}

We also add helper functions to manage conversions between them:

{% highlight haskell %}
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT action >>= return . extract
  where
    extract (Left err)  = "Error: " ++ show err
    extract (Right val) = val

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val
{% endhighlight %}

*Why?*

* Allows `IO` operations inside error handling (necessary for mutable Env).
* Prevents mixing `IO` and pure computations incorrectly.
* Enables future features like reading files, user-defined functions, etc.

# Fixing `readExpr`

Finally, we need to fix `readExpr`. It's current defined like this:

{% highlight haskell %}
readExpr :: String -> ThrowsError LispVal
{% endhighlight %}

It changes to support `IOThrowsError`:

{% highlight haskell %}
readExpr :: String -> IOThrowsError LispVal
readExpr input = liftThrows $ case parse parseExpr "lisp" input of
    Left err -> Left $ ParserError (show err)
    Right val -> Right val
{% endhighlight %}

This allows `readExpr` to integrate with our new `IOThrowsError`-based evaluator.

# Running

With all of these pieces in place, we can use `define` to define variables and start to work with them.

{% highlight text %}
Welcome to Mini Lisp (Haskell)
λ> (define a 50)
50
λ> (define b 120)
120
λ> (define c 4)
4
λ> (+ (- b c) a)
166
λ>
{% endhighlight %}

# Conclusion

This update introduced: 

* Persistent variables using `define`
* A mutable environment with `IORef`
* Function lookup inside the environment
* A fully working REPL that retains state across expressions

We'll continue to add to this as we go. See you in the next chapter!