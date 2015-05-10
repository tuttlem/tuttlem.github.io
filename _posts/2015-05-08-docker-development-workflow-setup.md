---
layout: post
title: Docker Development Workflow Setup
date: 2015-05-08
comments: false
categories: [ "docker", "development" ]
---

As I write code in a lot of different languages using a lot of different frameworks, it makes sense for me to virtualise my development environments in such a way that they're given their own isolated space so that they can't <em>infect</em> each other. In today's post, I'm going to walkthrough my development environment setup using [Docker](https://www.docker.com/).

### Setting up a Clojure environment

The example that I'll use is the development container that I have setup for [Clojure](http://clojure.org/). First of all, I create a workspace for my Clojure development on my host machine, under my source directory like anything else. I'll put this in `~/src/clojure`.

In that folder, I create two scripts. `run.sh` which just gets a disposable container up and running and `Dockerfile` which is based off of the [clojure:latest](https://registry.hub.docker.com/_/clojure/) image from the docker hub repository, but just adds a couple of extra bits and pieces to help the development environment get started.

### Dockerfile

The `Dockerfile` is pretty straight forward but relies on <em>some magic</em>. Unfortunately, here's my solution loses its portability. DOH! But, I'm still unsure of how to get around this. To keep permissions and ownerships common between the host and the container (because we'll be mounting a volume from the host), I create my developer account called `michael` as the next account after `root`. This is how it is on my host machine, so there's no conflicting user/group ids.

{% highlight text %}
FROM clojure:latest

RUN apt-get update && \
    apt-get install -y sudo && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN adduser --disabled-password --gecos '' michael && \
    adduser michael sudo && \
    echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

WORKDIR /home/michael
ENV HOME /home/michael
VOLUME ["/home/michael"]

USER michael
{% endhighlight %}

A couple of small things to note.

After installing `sudo`, I clean up any dead weight that `apt-get` may have left behind. This is just container maintenance to ensure that the image that's generated is as small as possible.

We add the `michael` account with no password and directly into the `sudo` group. We also adjust the `sudo` config so that users in the `sudo` group can issue administrative commands without needing to supply a password.

From there, it's all about making the home directory of `michael` centre stage for the container and switching to the `michael` user.

### run.sh

The run script is fairly straight forward. All we need it to do is mount our current directory as a volume in the container and start [bash](https://www.gnu.org/software/bash/bash.html). Of course, if you need to publish ports or create other volume mounts; here is where you'd do it.

{% highlight bash %}
#!/bin/bash

docker run -ti --rm -v $(pwd):/home/michael clojure:latest /bin/bash
{% endhighlight %}

### Start developing

You're done now. Everything creates in context of your non-root user. Your tools are available to you in your container and you're free to develop using your editor on your host.

