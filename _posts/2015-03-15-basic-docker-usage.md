---
layout: post
title: Basic docker usage
date: 2015-03-15
comments: false
categories: [ "docker" ]
---

[Docker](https://www.docker.com/) is a platform that allows you to bundle up your applications and their dependencies into a distributable container easing the overhead in environment setup and deployment.

The [Dockerfile reference](https://docs.docker.com/reference/builder/) in the docker documentation set goes through the important pieces of building an image. 

In today's post, I'm just going to run through some of the commands that I've found most useful.

### Building a container

{% highlight bash %}
# build an image and assign it a tag
sudo docker build -t username/imagename:tag .
{% endhighlight %}

### Controlling containers

{% highlight bash %}
# run a single command
sudo docker run ubuntu /bin/echo 'Hello world'

# run a container in a daemonized state
sudo docker run -d ubuntu /bin/sh -c "while true; do echo hello world; sleep 1; done"

# run a container interactively
sudo docker run -t -i ubuntu /bin/bash

# connect to a running container
sudo docker attach container_id

# stop a running container
sudo docker stop container_name

# remove a container
sudo docker rm container_name

# remove an image
sudo docker rmi image_name

{% endhighlight %}

When running a container, `-p` will allow you to control port mappings and `-v` will allow you to control volume locations.

### Getting information from docker

{% highlight bash %}
# list images
sudo docker images

# list running containers
sudo docker ps

# list all containers
sudo docker ps -a

# inspecting the settings of a container
sudo docker inspect container_name

# check existing port mappings
sudo docker port container_name 

# retrieve stdout from a running container
sudo docker logs container_name
sudo docker logs -f container_name

{% endhighlight %}
