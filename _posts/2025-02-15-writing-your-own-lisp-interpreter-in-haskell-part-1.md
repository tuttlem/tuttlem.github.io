---
layout: post
title: Writing Your Own Lisp Interpreter in Haskell - Part 1
date: 2025-02-15
comments: false
categories: [ haskell, lisp, parsec ]
---

# Introduction

[Lisp](https://en.wikipedia.org/wiki/Lisp_(programming_language)) has long been a favorite language for those interested in metaprogramming, functional programming, and 
symbolic computation. Writing your own Lisp interpreter is one of the best ways to deepen your understanding of 
programming languages. In this series, we’ll build a Lisp interpreter from scratch in Haskell, a language that lends 
itself well to this task due to its strong type system and functional nature.

In this first post, we’ll cover:
* Defining Lisp’s syntax and core data structures
* Writing a simple parser for Lisp expressions
* Implementing an evaluator for basic operations

By the end of this post, you’ll have a working Lisp interpreter that can evaluate basic expressions like `(+ 1 2)`.


If you're following along, you can find the implementation for this article [here](https://github.com/tuttlem/hlisp/releases/tag/part1).

# Setup

We're using [Haskell](https://www.haskell.org/) to implement our Lisp interpreter, so make sure you're installed and 
ready to go.

To get started, create yourself a new project. I use [stack](https://docs.haskellstack.org/en/stable/) so creating my 
new list (called `hlisp`):

{% highlight shell %}
stack new hlisp
{% endhighlight %}

We'll need a few dependencies to begin with. I'm adding my entire Lisp system to my library, leaving my main exe to 
simply be a REPL.

Add `containers`, `mtl`, and `parsec` as dependencies:

{% highlight yaml %}
library:
    source-dirs: src
    dependencies:
        - containers
        - mtl
        - parsec
{% endhighlight %}

# Code

Now we can get started on some code.

## Defining Lisp Expressions

Lisp code is composed of simple data types:

* **Atoms** (symbols, numbers, booleans)
* **Lists** (sequences of expressions)
* **Functions** (built-in or user-defined)

In Haskell, we can represent these using a data type:

{% highlight haskell %}
module Expr where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Except

-- Lisp expression representation
data LispVal
    = Atom String
    | Number Integer
    | Bool Bool
    | List [LispVal]
    | Lambda [String] LispVal Env -- user-defined function
    | BuiltinFunc ([LispVal] -> ThrowsError LispVal) -- built-in functions

instance Show LispVal where
    show (Atom name) = name
    show (Number n) = show n
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (List xs) = "(" ++ unwords (map show xs) ++ ")"
    show (Lambda params body _) =
        "(lambda (" ++ unwords params ++ ") " ++ show body ++ ")"
    show (BuiltinFunc _) = "<builtin function>"

instance Eq LispVal where
    (Atom a) == (Atom b) = a == b
    (Number a) == (Number b) = a == b
    (Bool a) == (Bool b) = a == b
    (List a) == (List b) = a == b
    _ == _ = False  -- Functions and different types are not comparable

-- Environment for variable storage
type Env = Map String LispVal

-- Error handling
data LispError
    = UnboundVar String
    | TypeMismatch String LispVal
    | BadSpecialForm String LispVal
    | NotAFunction String
    | NumArgs Int [LispVal]
    | ParserError String
    deriving (Show)

type ThrowsError = Either LispError
{% endhighlight %}

This defines the core structure of Lisp expressions and introduces a simple error-handling mechanism.

We define `Show` and `Eq` explicitly on `LispVal` because of the `Lambda` and `BuiltinFunc` not really having naturally 
expressed analogs for these type classes. The compiler complains!

`LispVal` allows us to define:
* `Atom` 
* `Number`
* `Bool`
* `List`
* `Lambda`
* `BuiltinFunc`

The `Env` type gives us an environment to operate in keeping track of our variables.

`LispError` defines some high level problems that can occur, and `ThrowsError` is partially applied type where you're 
either going to receive the value (to complete the application), or as the type suggests - you'll get a `LispError`.

## Parsing Lisp Code

To evaluate Lisp code, we first need to parse input strings into our LispVal data structures. We’ll use the Parsec 
library to handle parsing.

{% highlight haskell %}
module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Expr
import Control.Monad
import Numeric

-- Parse an atom (symbol)
parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> oneOf "!$%&|*+-/:<=>?@^_~"
    rest <- many (letter <|> digit <|> oneOf "!$%&|*+-/:<=>?@^_~")
    return $ Atom (first : rest)

-- Parse a number
parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

-- Parse booleans
parseBool :: Parser LispVal
parseBool =
    (string "#t" >> return (Bool True)) <|> (string "#f" >> return (Bool False))

-- Parse lists
parseList :: Parser LispVal
parseList = List <$> between (char '(') (char ')') (sepBy parseExpr spaces)

-- General parser for any expression
parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseNumber <|> parseBool <|> parseList

-- Top-level function to run parser
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> Left $ ParserError (show err)
    Right val -> Right val
{% endhighlight %}

With these parsers defined, we can now evaluate expressions.

## Simple Evaluation

Now, we can use these types to perform some evaluations. We do need to give our interpreter some functions that it can 
execute.

{% highlight haskell %}
module Eval where

import Expr
import Control.Monad.Except
import qualified Data.Map as Map

-- Look up variable in environment
lookupVar :: Env -> String -> ThrowsError LispVal
lookupVar env var = case Map.lookup var env of
  Just val -> Right val
  Nothing -> Left $ UnboundVar var

-- Apply a function (either built-in or user-defined)
apply :: LispVal -> [LispVal] -> ThrowsError LispVal
apply (BuiltinFunc f) args = f args
apply (Lambda params body closure) args =
  if length params == length args
    then eval (Map.union (Map.fromList (zip params args)) closure) body
    else Left $ NumArgs (length params) args
apply notFunc _ = Left $ NotAFunction (show notFunc)

-- Evaluator function
eval :: Env -> LispVal -> ThrowsError LispVal
eval env (Atom var) = lookupVar env var
eval _ val@(Number _) = Right val
eval _ val@(Bool _) = Right val
eval env (List [Atom "quote", val]) = Right val
eval env (List (Atom func : args)) = do
  func' <- eval env (Atom func)
  args' <- mapM (eval env) args
  apply func' args'
eval _ badForm = Left $ BadSpecialForm "Unrecognized form" badForm

-- Sample built-in functions
primitives :: [(String, LispVal)]
primitives =
  [ ("+", BuiltinFunc numericAdd),
    ("-", BuiltinFunc numericSub),
    ("*", BuiltinFunc numericMul),
    ("/", BuiltinFunc numericDiv)
  ]

numericAdd, numericSub, numericMul, numericDiv :: [LispVal] -> ThrowsError LispVal
numericAdd [Number a, Number b] = Right $ Number (a + b)
numericAdd args = Left $ TypeMismatch "Expected numbers" (List args)

numericSub [Number a, Number b] = Right $ Number (a - b)
numericSub args = Left $ TypeMismatch "Expected numbers" (List args)

numericMul [Number a, Number b] = Right $ Number (a * b)
numericMul args = Left $ TypeMismatch "Expected numbers" (List args)

numericDiv [Number a, Number b] =
  if b == 0 then Left $ TypeMismatch "Division by zero" (Number b)
  else Right $ Number (a `div` b)
numericDiv args = Left $ TypeMismatch "Expected numbers" (List args)

-- Initialize environment
primitiveEnv :: Env
primitiveEnv = Map.fromList primitives
{% endhighlight %}

## Creating a REPL

We can now tie all of this together with a REPL.

{% highlight haskell %}
module Main where

import Eval
import Parser
import Expr
import Control.Monad
import System.IO

-- REPL loop
repl :: Env -> IO ()
repl env = do
  putStr "λ> "
  hFlush stdout
  input <- getLine
  unless (input == "exit") $ do
    case readExpr input >>= eval env of
      Left err -> print err
      Right val -> print val
    repl env

main :: IO ()
main = do
  putStrLn "Welcome to Mini Lisp (Haskell)"
  repl primitiveEnv
{% endhighlight %}

# Run

Now we can give this a run!

{% highlight plain %}
hlisp stack exec hlisp-exe
Welcome to Mini Lisp (Haskell)
λ> (+ 6 5)
11
λ> (- 10 (+ 50 4))
-44
λ>
{% endhighlight %}

These simple expressions now evaluate!

# Conclusion

This gives us a pretty solid base!

We now have a working Lisp interpreter that can:
* Parse expressions (atoms, numbers, booleans, lists)
* Evaluate basic arithmetic expressions
* Provide an interactive REPL

In the next post, we’ll add variables, conditionals, and user-defined functions to make our Lisp more powerful!