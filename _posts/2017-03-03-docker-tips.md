---
layout: post
title: Docker tips
date: 2017-03-03
comments: false
categories: [ "docker", "tips" ]
---

Today's post is a collection of helpful tips to manage [Docker](https://www.docker.com/).

## Kill all running containers

Kills all of the containers currently running on your machine.

```
$ docker kill $(docker ps -q)
```

## Remove all containers

Remove all containers (running or stopped) from your machine.

```
$ docker rm $(docker ps -q -a)
```

## Remove dangling images

Any image builds that have failed mid-build will end up in a dangling state. You can remove any of these easily.

```
$ docker rmi $(docker images -q -f "dangling=true")
```

## Remove all images from your machine

If you need to turn over *ALL* of the images in your local repository, you can purge out anything with the following.

```
$ docker rmi $(docker images -q)
```

## Inspect the history of an image

```
$ docker history --no-trunc image_id
```

## Add changes to existing images

If you've got a minor change to make to an already existing image, you can use `commit` to prevent a full build process.

```
$  docker commit --change "ENV DEBUG true" image_id
```

## Tune ulimit

Make sure you have enough file descriptors to work with.

```
$ docker run --ulimit nofile=1024:1024 rest_of_run_arguments
```


