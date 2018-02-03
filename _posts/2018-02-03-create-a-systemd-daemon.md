---
layout: post
title: Create a systemd daemon
date: 2018-02-03
comments: false
categories: [ "systemd", "linux", "admin" ]
---

Like it or hate it, you'll find [systemd](https://www.freedesktop.org/wiki/Software/systemd/) on any modern linux system these days. This management subsystem is the newer replacement for the older init scripts-based systems.

In today's article, I'm going to show you how to create a daemon that will sit under the management of [systemd](https://www.freedesktop.org/wiki/Software/systemd/).

## `.service` file

A service file instructs systemd how to interact with the deployed application. It's referred to as the [service unit configuration](https://www.freedesktop.org/software/systemd/man/systemd.service.html). The following is an example:

{% highlight text %}
[Unit]
Description=My Service
After=network.target

[Service]
ExecStart=/usr/bin/my-service
Restart=on-failure

[Install]
WantedBy=multi-user.target
{% endhighlight %}

## Installation

This file, once you've created it gets deployed to systemd with a `cp`. You also need to notify systemd.

{% highlight bash %}
sudo cp my-service.service /lib/systemd/system

sudo systemctl daemon-reload
sudo systemctl enable my-service
{% endhighlight %}

## Interacting with the service

Once the service is in place, you can start to interact with the daemon using `systemctl`.

{% highlight bash %}
sudo systemctl start my-service
sudo systemctl status my-service
{% endhighlight %} 

You can also get a hold of any log pushed out to the standard system file descriptors:

{% highlight bash %}
journalctl --unit my-service --follow
{% endhighlight %}

## Removal

Once you no longer need your service, you can remove it simply my `rm`'ing the `.service` file. Of course, you'll need to disable your service first.

That's it for creating a systemd service.
