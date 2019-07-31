---
layout: post
title: Add info at build time to go binaries
date: 2019-07-31
comments: false
categories: [ "version", "go", "build", "make" ]
---

### Introduction

Sometimes it can be useful to capture information about your environment at build time, and have this information injected into the binary that you're building. Some examples centre around versioning, where it might make sense to capture git commit hashes or build serials.

### An example program

{% highlight go %}
package main

import "fmt"

var gitCommit string
var buildSerial string

func main() {
  fmt.Printf("git hash: %s, build serial: %s", gitCommit, buildSerial)
}
{% endhighlight %}

Two variables in this module `gitCommit`, and `buildSerial` are going to hold some version information for us. Running this program yields some rather uninteresting results.

{% highlight text %}
go run main.go
git hash: , build serial: 
{% endhighlight %}

### -X switch

While building a program, you can use the `-X` linker switch which will allow you to supply information into module variables _from the build process_. 

We can obtain the latest build hash using `git` with the following:

{% highlight bash %}
git rev-list -1 HEAD
{% endhighlight %}

We can even synthesize a build number involving the date, perhaps?

{% highlight bash %}
date +%s
{% endhighlight %}

Using the `-ldflags` switch, we can now specify these at the console.

{% highlight bash %}
go build -ldflags "-X main.gitCommit=`git rev-list -1 HEAD` -X main.buildSerial=`date +%s`" main.go
{% endhighlight %}

### Closing up

Now that we have a binary built, it's had its build information applied - these variables now magically receive these values.

{% highlight text %}
./main
git hash: f6f62d9a759a03afffb913a1d24fb64a1bc5507d, build serial: 1564573076
{% endhighlight %}

Remember, these switches can be buried behid a `Makefile` also, so you don't need to be typing these things over and over.

{% highlight text %}
GOOS=linux
GOARCH=386

.PHONY: build

GIT_COMMIT := $(shell git rev-list -1 HEAD)
BUILD_NO := $(shell date +%s)

build:
  GOOS=$(GOOS) GOARCH=$(GOARCH) go build -ldflags "-X main.gitCommit=$(GIT_COMMIT) -X main.buildSerial=$(BUILD_NO)" .
{% endhighlight %}


