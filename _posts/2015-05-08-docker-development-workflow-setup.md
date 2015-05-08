---
layout: post
title: Docker Development Workflow Setup
date: 2015-05-08
comments: false
categories: [ "docker", "development" ]
---

As I write code in a lot of different languages using a lot of different frameworks, it makes sense for me to virtualise my development environments in such a way that they're given their own isolated space so that they can't <em>infect</em> each other. In today's post, I'm going to walkthrough my development environment setup using [Docker](https://www.docker.com/).

The whole basic idea here heavily relies on the information in [this post](http://www.yegor256.com/2014/08/29/docker-non-root.html). It's main focus is setting up non-root users in a container. Whilst this side-effect is useful for this workflow, it's more about the execution of custom commands after a standard image boots up.

### Setting up a Clojure environment

The example that I'll use is the development container that I have setup for [Clojure](http://clojure.org/). First of all, I create a workspace for my Clojure development on my host machine, under my source directory like anything else. I'll put this in `~/src/clojure`.

In that folder, I create two scripts. `run.sh` which just gets a disposable container up and running and `setup-for-dev.sh` which is the command invoked by the container setup in the first script.

### run.sh

The run script is fairly straight forward. All we need it to do is mount our current directory as a volume in the container and kick off our `setup-for-dev.sh` script. Of course, if you need to publish ports or create other volume mounts; here is where you'd do it.

{% highlight bash %}
#!/bin/bash

docker run -ti --rm -v $(pwd):/src clojure:latest /src/setup-for-dev.sh
{% endhighlight %}

Note that we're using [clojure:latest](https://registry.hub.docker.com/_/clojure/) from the docker hub repository.

### setup-for-dev.sh

Once the container has been kicked off, I need it to do a couple of utility tasks. When we're using a utility like `lein` inside the container, anything created will be owned by `root`. What I prefer to do is create a standard user that has full root access in the container, but will identify with my host as the same standard user. 

So, I create a `michael` account:

{% highlight bash %}
#!/bin/bash

adduser --disabled-password --gecos '' michael
adduser michael sudo
echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
su - michael -c /bin/bash
{% endhighlight %}

### Start developing

From here, it's just a matter of getting into the `/src` folder inside the container to run your Clojure tools and using a code editor on your host to take care of the text.

 

