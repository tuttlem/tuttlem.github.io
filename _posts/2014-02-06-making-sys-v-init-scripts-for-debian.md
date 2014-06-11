---
layout: post
title: Making Sys-V Init Scripts for Debian
date: 2014-02-06
comments: false
---

### Introduction

Sometimes it's necessary to run programs (applications or daemons) in the background at different stages of a machine's up time. The most common use case of which centralises around starting something at boot and stopping something at shutdown.

In today's post, I'll be doing a write up on preparing init scripts for Sys-V style systems.

### How it works

Debian's Sys-V style init system relies on the scripts under `/etc/init.d` to instruct the operating system on how to start or stop particular programs. A header that sits at the top of these scripts informs the init system under what conditions this script should start and stop.

Here's an example of this header from an excerpt taken from `/etc/init.d/README`

All `init.d` scripts are expected to have a LSB style header documenting
dependencies and default runlevel settings.  The header look like this
(not all fields are required):

{% highlight text %}
### BEGIN INIT INFO
# Provides:          skeleton
# Required-Start:    $remote_fs $syslog
# Required-Stop:     $remote_fs $syslog
# Should-Start:      $portmap
# Should-Stop:       $portmap
# X-Start-Before:    nis
# X-Stop-After:      nis
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# X-Interactive:     true
# Short-Description: Example initscript
# Description:       This file should be used to construct scripts to be
#                    placed in /etc/init.d.
### END INIT INFO
{% endhighlight %}

The insserv man page will take you further in-depth as to what each of these means for your init script.

A full example of an init script is given on the Debian administration website here. It doesn't use the header above - which is acceptable as these fields aren't required by the script.

{% highlight bash %}
#! /bin/sh
# /etc/init.d/blah
#

# Some things that run always
touch /var/lock/blah

# Carry out specific functions when asked to by the system
case "$1" in
  start)
    echo "Starting script blah "
    echo "Could do more here"
    ;;
  stop)
    echo "Stopping script blah"
    echo "Could do more here"
    ;;
  *)
    echo "Usage: /etc/init.d/blah {start|stop}"
    exit 1
    ;;
esac

exit 0
{% endhighlight %}

Putting this script into a text file (into your home directory) and applying execute permissions on the file, allows you to test it at the console. I've put this script into a file called "blah".

{% highlight bash %}
$ chmod 755 blah

$ ./blah
Usage: /etc/init.d/blah {start|stop}

$ ./blah start
Starting script blah
Could do more here

$ ./blah stop
Stopping script blah
Could do more here
{% endhighlight %}

The script implements a "start" and "stop" instruction.

### Installation

Now that you've developed your script and are happy with its operation (after testing it at the console), you can install it on your system.

The first step is to copy your script (in this case "blah") up into `/etc/init.d/`. This makes it available to the init system to use. It won't actually use it though until you establish some symlinks between the script and the runlevel script sets.

You can check that it's available to your system using the "service" command, like so:

{% highlight text %}
$ sudo service --status-all
 [ + ]  acpid
 [ + ]  atd
 [ ? ]  blah
 [ - ]  bootlogs
 [ ? ]  bootmisc.sh
 . . .
 . . .
 . . .
 . . .
{% endhighlight %}

You can see that "blah" is being registered here as an init script and as such can also be started and stopped using the "service" command:

{% highlight bash %}
$ sudo service blah start
Starting script blah
Could do more here

$ sudo service blah stop
Stopping script blah
Could do more here
{% endhighlight %}

Now we'll attach this init script to the startup and shut down of the computer. update-rc.d will help with this process. You can get the script installed with the following command:

{% highlight bash %}
$ sudo update-rc.d blah defaults
{% endhighlight %}

If you no longer want the script to execute on your system, you can remove it from the script sets with the following command:

{% highlight bash %}
$ update-rc.d -f blah remove
{% endhighlight %}

After removing it, you'll still have your script in `/etc/init.d/`, just in case you want to set it up again.

That's it for today. 