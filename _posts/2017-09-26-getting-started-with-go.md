---
layout: post
title: Getting started with Go
date: 2017-09-26
comments: false
categories: [ "go" ]
---

[Go](https://golang.org/) is a general purpose programming language aiming at resolving some of the short-comings observed in other languages. Some key features of [Go](https://golang.org/) is that it's statically typed, and has a major focus on making scalability, multiprocessing and networking easy.

In today's post, I'll go through some of the steps that I've taken to prepare a development environment that you can be immediately productive in.

## Code organisation

To take a lot of the think work out of things, as well as present a consistent view from machine-to-machine, there are some strict rules around code organisation. A full run-down on the workspace can be found [here](https://golang.org/doc/code.html#Organization); but for the purposes of today's article we'll look at locating a folder at `~/Source/go`. 

## Docker for development

To not clutter my host system, I make extensive use of [Docker](https://www.docker.com/) containers. [Docker](https://www.docker.com/) containers allow me to run multiple versions of the same software concurrently, but also make all of my environments disposable. Whilst the instructions below will be centralised around the `go` command, all of these will be executed in context of a `golang` container. The following command sets up a container for the duration of one command's execution:

{% highlight bash %}
docker run -ti --rm -v ~/Source/go:/go golang
{% endhighlight %}

`-ti` runs the container interactively allocating a TTY; `--rm` cleans the container up after the command has finished executing; we mount our go source folder inside the container at the pre-configured `/go` directory.

I found it beneficial to make an `alias` in `zsh` wrapping this up for me.

## Hello, world

Getting that *first* application up and running is pretty painless. We need to create a directory for our project, build and run.

{% highlight bash %}
# Create the project folder
mkdir -p src/github.com/user/hello

# Get editing the program
cd src/github.com/user/hello
vim hello.go
{% endhighlight %}

As you'd expect, we create our program:

{% highlight golang %}
package main

import "fmt"

func main() {
  fmt.Printf("Hello, world\n")
}
{% endhighlight %}

Now we can build the program.

{% highlight bash %}
go install github.com/user/hello
{% endhighlight %}

## We're done

You'll have a binary waiting for you to execute now.

{% highlight text %}
bin/hello
Hello, world
{% endhighlight %}


