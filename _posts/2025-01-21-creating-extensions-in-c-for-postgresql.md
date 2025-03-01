---
layout: post
title: Creating extensions in C for PostgreSQL
date: 2025-01-21
comments: false
categories: [ "postgres", "c", "extensions" ]
---

# Introduction

PostgreSQL allows developers to extend its functionality with custom extensions written in C. This powerful feature can 
be used to add new functions, data types, or even custom operators to your PostgreSQL instance. 

In this blog post, I'll guide you through creating a simple "Hello, World!" C extension for PostgreSQL and demonstrate 
how to compile and test it in a Dockerized environment. Using Docker ensures that your local system remains clean while 
providing a reproducible setup for development.

# Development

There are a few steps that we need to walk through in order to get your development environment up and running as well 
as some simple boilerplate code.

## The Code

First, create a working directory for your project:

{% highlight shell %}
mkdir postgres_c_extension && cd postgres_c_extension
{% endhighlight %}

Now, create a file named `example.c` and add the following code:

{% highlight c %}
#include "postgres.h"
#include "fmgr.h"
#include "utils/builtins.h"  // For cstring_to_text function

PG_MODULE_MAGIC;

PG_FUNCTION_INFO_V1(hello_world);

Datum
hello_world(PG_FUNCTION_ARGS)
{
    text *result = cstring_to_text("Hello, World!");
    PG_RETURN_TEXT_P(result);
}
{% endhighlight %}

This code defines a simple PostgreSQL function `hello_world()` that returns the text "Hello, World!". It uses 
PostgreSQL's C API, and the `cstring_to_text` function ensures that the string is properly converted to a PostgreSQL 
text type.

Let's take a closer look at a few pieces of that code snippet.

### `PG_MODULE_MAGIC`

{% highlight c %}
PG_MODULE_MAGIC;
{% endhighlight %}

This macro is mandatory in all PostgreSQL C extensions. It acts as a marker to ensure that the extension was compiled 
with a compatible version of PostgreSQL. Without it, PostgreSQL will refuse to load the module, as it cannot verify 
compatibility.

### `PG_FUNCTION_INFO_V1`

{% highlight c %}
PG_FUNCTION_INFO_V1(hello_world);
{% endhighlight %}

This macro declares the function `hello_world()` as a PostgreSQL-compatible function using version 1 of PostgreSQL's 
call convention. It ensures that the function can interact with PostgreSQL's internal structures, such as argument 
parsing and memory management.

### `Datum`

{% highlight c %}
Datum hello_world(PG_FUNCTION_ARGS)
{% endhighlight %}

* `Datum` is a core PostgreSQL data type that represents any value passed to or returned by a PostgreSQL function. It is a general-purpose type used internally by PostgreSQL to handle various data types efficiently.
* `PG_FUNCTION_ARGS` is a macro that defines the function signature expected by PostgreSQL for dynamically callable functions. It gives access to the arguments passed to the function.

In this example, `Datum` is the return type of the hello_world function.

### `PG_RETURN_TEXT_P`

{% highlight c %}
text *result = cstring_to_text("Hello, World!");
PG_RETURN_TEXT_P(result);
{% endhighlight %}

* `cstring_to_text`: This function converts a null-terminated C string (`char *`) into a PostgreSQL `text` type. PostgreSQL uses its own `text` structure to manage string data.
* `PG_RETURN_TEXT_P`: This macro wraps a pointer to a `text` structure and converts it into a Datum, which is required for returning values from a PostgreSQL C function.

The flow in this function:

* `cstring_to_text("Hello, World!")` creates a `text *` object in PostgreSQL's memory context.
* `PG_RETURN_TEXT_P(result)` ensures the text * is properly wrapped in a `Datum` so PostgreSQL can use the return value.

## Control and SQL Files

A PostgreSQL extension requires a control file to describe its metadata and a SQL file to define the functions it 
provides.

Create a file named `example.control`:

{% highlight text %}
default_version = '1.0'
comment = 'Example PostgreSQL extension'
{% endhighlight %}

Next, create `example--1.0.sql` to define the SQL function:

{% highlight sql %}
CREATE FUNCTION hello_world() RETURNS text
AS 'example', 'hello_world'
LANGUAGE C IMMUTABLE STRICT;
{% endhighlight %}

## Setting Up the Build System

To build the C extension, you'll need a `Makefile`. Create one in the project directory:

{% highlight text %}
MODULES = example
EXTENSION = example
DATA = example--1.0.sql
PG_CONFIG = pg_config
OBJS = $(MODULES:%=%.o)

PGXS := $(shell $(PG_CONFIG) --pgxs)
include $(PGXS)
{% endhighlight %}

This `Makefile` uses PostgreSQL's `pgxs` build system to compile the C code into a shared library that PostgreSQL can 
load.

## Build Environment

To keep your development environment clean, we'll use Docker. Create a `Dockerfile` to set up a build environment and 
compile the extension:

{% highlight dockerfile %}
FROM postgres:latest

RUN apt-get update && apt-get install -y \
    build-essential \
    postgresql-server-dev-all \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /usr/src/example
COPY . .

RUN make && make install
{% endhighlight %}

Build the Docker image:

{% highlight shell %}
docker build -t postgres-c-extension .
{% endhighlight %}

Start a container using the custom image:

{% highlight shell %}
docker run --name pg-c-demo -e POSTGRES_PASSWORD=postgres -d postgres-c-extension
{% endhighlight %}

## Testing 

Access the PostgreSQL shell in the running container:

{% highlight shell %}
docker exec -it pg-c-demo psql -U postgres
{% endhighlight %}

Run the following SQL commands to create and test the extension:

{% highlight sql %}
CREATE EXTENSION example;
SELECT hello_world();
{% endhighlight %}

You should see the output:

{% highlight text %}
 hello_world 
--------------
 Hello, World!
(1 row)
{% endhighlight %}

## Cleaning Up

When you're finished, stop and remove the container:

{% highlight shell %}
docker stop pg-c-demo && docker rm pg-c-demo
{% endhighlight %}

## Conclusion

By following this guide, you've learned how to create a simple C extension for PostgreSQL, compile it, and test it in a 
Dockerized environment. This example can serve as a starting point for creating more complex extensions that add custom 
functionality to PostgreSQL. Using Docker ensures a clean and reproducible setup, making it easier to focus on 
development without worrying about system dependencies. 







