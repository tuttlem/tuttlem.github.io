---
layout: post
title: Detaching running processes in bash
date: 2015-10-14
comments: false
categories: [ "bash", "detatch", "admin" ]
---

There are quite a few times where I've run a command on a remote machine and needed to get out of that machine but leave my command running. 

I'll normally start a job that I *know* is going to take a while using [an ampersand](http://bashitout.com/2013/05/18/Ampersands-on-the-command-line.html) like so:

{% highlight bash %}
$ long-running-prog &
{% endhighlight %}

Really, the [nohup](https://en.wikipedia.org/wiki/Nohup) command should also be put on the command line so that the command that you execute will ignore the signal SIGHUP.

{% highlight bash %}
$ hup long-running-prog &
$ exit
{% endhighlight %}

If you're already part-way through a running process, you can get it to continue running in the background (while you make your getaway) by doing the following

{% highlight bash %}
$ long-running-prog
CTRL-Z
$ bg
$ disown pid
{% endhighlight %}

You use *CTRL-Z* to suspend the running process. The [bg](http://linux.die.net/man/1/bg) command then gets the program running in the background. You can confirm that it is running in the background with the [jobs](http://linux.die.net/man/1/jobs) command. Lastly, using [disown](http://linux.die.net/man/1/disown) detatches the process running in the background from your terminal, so that when you exit your session the process will continue.

The [LDP](http://tldp.org/LDP) has a [great article](http://tldp.org/LDP/abs/html/x9644.html) on job control.

