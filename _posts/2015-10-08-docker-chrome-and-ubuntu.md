---
layout: post
title: Docker, Chrome and Ubuntu
date: 2015-10-08
comments: false
categories: [ "docker", "chrome", "ubuntu" ]
---

[Docker](https://www.docker.com/) provides a very convenient way of packaging your applications and their dependencies so that they can be moved around without too much effort. Another great side-effect of this type of system design is the isolation that you're given between containers. In today's post, I'll walk through the setup of [Google Chrome](https://www.google.com.au/intl/en/chrome/browser/desktop/index.html) running in an isolated sandbox within [Docker](https://www.google.com.au/intl/en/chrome/browser/desktop/index.html) and so that it's nicely integrated into [Ubuntu](http://www.ubuntu.com/).

### Not starting from zero

I have to admit, most of the hard work had already been done for me in [Jessie Frazelle's post](https://blog.jessfraz.com/post/docker-containers-on-the-desktop/) about hosting desktop applications in docker containers. The Chrome Dockerfile that I have hosted [in my github repository](https://github.com/tuttlem/docker/blob/master/chrome/Dockerfile) is a pretty good rip, directly from Jessie's post.

### Getting started

Putting together a run script that you can repeatedly call from the operating system shouldn't be too hard. It only needed to do three things:

* Create a container when one didn't exist
* Start the container if it already existed
* Open a new window if the container was already started

This is a relatively simple bash script to do this:

{% highlight bash %}
CHROME_RUNNING=$(docker inspect --format="{{ .State.Running }}" chrome 2> /dev/null)
{% endhighlight %}

We want to check if the container is running, first up. I've standardised by calling the container `chrome`. Really creative. Upon successful return from the `docker inspect` command, the `$CHROME_RUNNING` variable should either be `true` or `false`. If the inspect call didn't go to plan, it's most likely because the container doesn't exist and we need to use `run` to kick it into gear:

{% highlight bash %}
docker run \
    -it \
    --net host \
    --cpuset-cpus 0 \
    --memory 512mb \
    -v /tmp/.X11-unix:/tmp/.X11-unix \
    -e DISPLAY=unix$DISPLAY \
    -v $HOME/Downloads:/root/Downloads \
    -v $HOME/.config/google-chrome/:/data \
    --device /dev/snd \
    -v /dev/shm:/dev/shm \
    --name chrome \
    tuttlem/chrome
{% endhighlight %}

This gets the container up and running and the browser under our noses.

In cases where the container already exists, but isn't running we'll use `run`. When the container exists and it is running, the only reason why someone could be invoking this script is to get another browser window running; so we'll use `exec` to get chrome to open up a new window for us:

{% highlight bash %}
if [ "$CHROME_RUNNING" == "false" ]; then
    docker start chrome
else
    docker exec chrome sh -c "/usr/bin/google-chrome '$@' --user-data-dir=/data"
fi
{% endhighlight %}

By using the `$@` variable in the `exec` script, we can take in any web address that's passed into this script. This is what will allow us to integrate this container into our operating system.

### Integration

We've done just about everything now with the run script. I've created myself a menu item with a chrome icon that just points to this run script:

The main key binding of <kbd>Super</kbd> + <kbd>W</kbd>, but the most important is changing the preferred browser so that it invokes the script. `%s` passes the desired web site through for a seamless finish.

