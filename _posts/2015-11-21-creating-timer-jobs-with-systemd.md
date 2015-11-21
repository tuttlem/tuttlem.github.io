---
layout: post
title: Creating timer jobs with systemd
date: 2015-11-21
comments: false
categories: [ "timer", "systemd" ]
---

Creating and executing timer jobs has traditionally been a task for [cron](https://en.wikipedia.org/wiki/Cron). With the arrival of [systemd](https://en.wikipedia.org/wiki/Systemd), this responsibility has been shifted onto services and timers. In today's post, I'll walk you through creating a service and timer schedule.

### Setup

To accomplish this task, we need two files and a couple of shell commands. The basic method to do this is as follows:

* Create a service definition
* Create a timer definition
* Start and enable the timer

In the example today, I'm going to schedule `s3cmd` each week to run over a mounted drive to sync with s3. 

As we're working with [systemd](https://en.wikipedia.org/wiki/Systemd), everything that we'll do is a [unit file](http://www.freedesktop.org/software/systemd/man/systemd.unit.html).

### Create a service definition

The service definition is a unit file which defines the actual work to be done. The following is placed at `/etc/systemd/system/sync-to-s3.service`.

{% highlight ini %}
[Unit]
Description=Runs the sync script for local file shares to s3

[Service]
Type=oneshot
ExecStart=/usr/bin/sh -c 's3cmd sync --check-md5 --follow-symlinks --verbose /mnt/share/ s3://my-s3-bucket/'
{% endhighlight %}

Full particulars on this file structure can be found in the [documentation](http://www.freedesktop.org/software/systemd/man/) about [service unit configuration](http://www.freedesktop.org/software/systemd/man/systemd.service.html).

### Create a timer definition

The timer definition is also another unit file that defines a schedule. The following is named the same as the above, only it gets a `.timer` extension at `/etc/systemd/system/sync-to-s3.timer`.

{% highlight ini %}
[Unit]
Description=Schedules the sync of local file shares out to s3

[Timer]
OnCalendar=weekly
OnBootSec=10min

[Install]
WantedBy=multi-user.target
{% endhighlight %}

Again the [documentation](http://www.freedesktop.org/software/systemd/man/) defines the full definition of the [timer unit configuration](http://www.freedesktop.org/software/systemd/man/systemd.timer.html).

The `OnCalendar` takes a value that needs to be understood by the time span parser, so make sure that it's valid in accordance with the [time span reference](http://www.freedesktop.org/software/systemd/man/systemd.time.html).

### Start and enable the timer

Now that the service and schedule definitions have been created, we can start up the timer:

{% highlight bash %}
sudo systemctl start sync-to-s3.timer
sudo systemctl enable sync-to-s3.timer
{% endhighlight %}

Now that you've got your job up and running, you get the full feature set that systemd offers, including [journald](http://www.freedesktop.org/software/systemd/man/systemd-journald.service.html). You can use this to inspect the current or historical run logs from invocations:

{% highlight bash %}
sudo journalctl -u sync-to-s3
{% endhighlight %}

