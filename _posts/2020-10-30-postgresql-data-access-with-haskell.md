---
layout: post
title: PostgreSQL Data Access with Haskell
date: 2020-10-30
comments: false
categories: [ "postgres", "data", "haskell" ]
---

### Introduction

[PostgreSQL](https://www.postgresql.org/) is a very popular relational database which has quite a few different data access libraries available for the [Haskell](https://www.haskell.org/) programming language.

Today's article aims to get you up and running, executing queries against PostgreSQL from your Haskell environment with the least amount of hassle.

### postgresql-simple

The first library that we'll go through is [postgresql-simple](https://www.stackage.org/package/postgresql-simple). This library has a very basic interface, and is really simple to get up an running.

> A mid-level client library for the PostgreSQL database, aimed at ease of use and high performance.

### Prerequisites

Before you get started though, you'll need `libpq` installed.

{% highlight bash %}
pacman -S postgresql-libs
{% endhighlight %}

Now you're ready to develop.

You'll need to add a dependency on the `postgresql-simple` library to your application. The following code will then allow you to connect to your PostgreSQL database, and ru  a simple command.

### Hello, Postgres!

{% highlight haskell %}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Database.PostgreSQL.Simple

localPG :: ConnectInfo
localPG = defaultConnectInfo
        { connectHost = "172.17.0.1"
        , connectDatabase = "clients"
        , connectUser = "app_user"
        , connectPassword = "app_password"
        }

main :: IO ()
main = do
  conn <- connect localPG
  mapM_ print =<< (query_ conn "SELECT 1 + 1" :: IO [Only Int])
{% endhighlight %}

When your application successfully builds and executes, you should be met with the following output:

{% highlight text %}
Only {fromOnly = 2}
{% endhighlight %}

Walking through this code quickly, we first enable `OverloadedStrings` so that we can specify our `Query` values as literal strings. 

{% highlight haskell %}
localPG :: ConnectInfo
localPG = defaultConnectInfo
        { connectHost = "172.17.0.1"
        , connectDatabase = "clients"
        , connectUser = "app_user"
        , connectPassword = "app_password"
        }
{% endhighlight %}

In order to connect to Postgres, we use a `ConnectInfo` value which is filled out for us via `defaultConnectInfo`. We just override those values for our examples. I'm running PostgreSQL in a docker container, therefore I've got my docker network address.

{% highlight haskell %}
conn <- connect localPG
{% endhighlight %}

The `localPG` value is now used to connect to the Postgres database. The `conn` value will be referred to after successful connection to send instructions to.

{% highlight haskell %}
mapM_ print =<< (query_ conn "SELECT 1 + 1" :: IO [Only Int])
{% endhighlight %}

Finally, we run our query `SELECT 1 + 1` using the `query_` function. `conn` is passed to refer to the connecion to execute this query on. 

With this basic code, we can start to build on some examples.

### Retrieve a specific record

In the Hello, World example above, we were adding two static values to return another value. As exampeles get more complex, we need to give the library more information about the data that we're working with. `Int` is very well known already, and already has mechanisms to deal with it (along with other basic data types).

In the `client` database table we have a list of names and ids. We can create a function to retrieve the name of a client, given an id:

{% highlight haskell %}
retrieveClient :: Connection -> Int -> IO [Only String]
retrieveClient conn cid = query conn "SELECT name FROM client WHERE id = ?" $ (Only cid)
{% endhighlight %}

The `Query` template passed in makes use of the `?` character to specify where substitutions will be put. Note the use of `query` rather than `query_`. In this case, `query` also accepts a Tuple containing all of the values for substitution.

Using the `FromRow` type class, our code can define a much stronger API. We can actually retrieve `client` rows from the database and convert them into `Client` values. 

We need `FromRow` first:

{% highlight haskell %}
import Database.PostgreSQL.Simple.FromRow
{% endhighlight %}

The `Client` data type needs definition now. It's how we'll refer to a client within our Haskell program:

{% highlight haskell %}
data Client = Client { id :: Int, name :: String }
  deriving (Show)
{% endhighlight %}

The `Client` data type now gets a `FromRow` instance, which allows `postgresql-simple` to use it.

{% highlight haskell %}
instance FromRow Client where
  fromRow = Client <$> field <*> field
{% endhighlight %}

In order of the fields definitions, we give `fromRow` definition. The `retrieveClient` function only changes to broaden its query, and change its return type!

{% highlight haskell %}
retrieveClient :: Connection -> Int -> IO [Client]
retrieveClient conn cid = query conn "SELECT id, name FROM client WHERE id = ?" $ (Only cid)
{% endhighlight %}

### Create a new record

When creating data, you can use the function `execute`. The `execute` function is all about execution of the query without any return value. 

{% highlight haskell %}
execute conn "INSERT INTO client (name) VALUES (?)" (Only "Sam")
{% endhighlight %}

Extending our API, we can make a `createClient` function; but with a twist. We'll also return the generated identifier (because of the `id` field).

{% highlight haskell %}
createClient :: Connection -> String -> IO [Only Int64]
createClient conn name =
  query conn "INSERT INTO client (name) VALUES (?) RETURNING id" $ (Only name)
{% endhighlight %}

We need a definition for `Int64`. This is what the underlying `SERIAL` in PostgreSQL will translate to inside of your Haskell application.

{% highlight haskell %}
import Data.Int
{% endhighlight %}

We can now use `createClient` to setup an interface of sorts fo users to enter information.

{% highlight haskell %}
main :: IO ()
main = do
  conn <- connect localPG
  putStrLn "Name of your client? "
  clientName <- getLine
  cid <- createClient conn clientName
  putStrLn $ "New Client: " ++ (show cid)
{% endhighlight %}

We've created a data creation interface now.

{% highlight plain %}
Name of your client?
Ringo
New Client: [Only {fromOnly = 4}]
{% endhighlight %}

### Update an existing record

When it comes to updating data, we don't expect much back in return aside from the number of records affected by the instruction. The `execute` function does exactly this. By measuring the return, we can convert the row count into a success/fail style message. I've simply encoded this as a boolean here.

{% highlight haskell %}
updateClient :: Connection -> Int -> String -> IO Bool
updateClient conn cid name = do
  n <- execute conn "UPDATE client SET name = ? WHERE id = ?" (name, cid)
  return $ n > 0
{% endhighlight %}

### Destroying records

Finally, destroying information out of the database will look a lot like the update.

{% highlight haskell %}
deleteClient :: Connection -> Int -> IO Bool
deleteClient conn cid = do
  n <- execute conn "DELETE FROM client WHERE id = ?" $ (Only cid)
  return $ n > 0
{% endhighlight %}

`execute` providing the affected count allows us to perform the post-execution validation again.

### Summary

There's some basic operations to get up and running using `postgresql-simple`. Really looks like you can prototype software all the way through to writing fully blown applications with it. 

Really simple to use.

