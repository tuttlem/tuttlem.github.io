---
layout: post
title: Find the listening process for a port
date: 2020-12-22
comments: false
categories: [ "admin", "linux", "network" ]
---

### Introduction

In networking, a port is assigned as a logical entity that a socket is established on. These sockets are owned by processes in your operation system. From time to time, it can be unclear which process owns which socket (or who is hogging which port).

In today's article, I'll take you through a few techniques on finding out who is hanging onto particular ports.

### netstat

[netstat](https://linux.die.net/man/8/netstat) is a general purpose network utility that will tell you about activity within your network interfaces.

> netstat - Print network connections, routing tables, interface statistics, masquerade connections, and multicast memberships 

If you can not find `netstat` installed on your system, you can normally get it from the `net-tools` package.

The following command will give you a breakdown of processes listening on port `8080`, as an example:

{% highlight text %}
➜  netstat -ltnp | grep -w ':8080'
(Not all processes could be identified, non-owned process info
 will not be shown, you would have to be root to see it all.)
tcp6       0      0 :::8080                 :::*                    LISTEN      -                   
{% endhighlight %}

An important message appears here. _"Not all processes could be identified, non-owned process info will not be shown, you would have to be root to see it all."_. There will be processes invisible to you unless you run this command as `root`.

Breaking down the netstat invocation:

* `l` will only show listening sockets
* `t` will only show tcp connections
* `n` will show numerical addresses
* `p` will show you the PID

You can see above, that no process is shown. Re-running this command as `root`:

{% highlight text %}
➜  sudo netstat -ltnp | grep -w ':8080'
tcp6       0      0 :::8080                 :::*                    LISTEN      2765/docker-proxy
{% endhighlight %}

### lsof

[lsof](https://linux.die.net/man/8/lsof) will give you a list of open files on the system. Remember, sockets are just files. By using `-i` we can filter the list down to those that match on an internet address.

{% highlight text %}
➜  sudo lsof -i :8080
COMMAND    PID USER   FD   TYPE DEVICE SIZE/OFF NODE NAME
docker-pr 2765 root    4u  IPv6  36404      0t0  TCP *:http-alt (LISTEN)
{% endhighlight %}

### fuser

[fuser](https://man7.org/linux/man-pages/man1/fuser.1p.html) is a program that has overlapping responsibilities with the likes of `lsof`. 

> fuser — list process IDs of all processes that have one or more files open

You can filter the list down directly with the command:

{% highlight text %}
➜  sudo fuser 8080/tcp
8080/tcp:             2765
{% endhighlight %}

This gives us a PID to work with. Again, note this is run as `root`. Now all we need to do is to tranform this PID into a process name. We can use `ps` to finish the job.

{% highlight text %}
➜  ps -p 2765 -o comm=
docker-proxy
{% endhighlight %}


