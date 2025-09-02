---
layout: post
title: "Hello, Jail: A Quick Introduction to FreeBSD Jails"
date: 2025-09-02
tags: [freebsd, jails, containers, systems]
---

[FreeBSD Jails](https://docs.freebsd.org/en/books/handbook/jails/) are one of the earliest implementations of operating 
system-level virtualization—dating back to the early 2000s, long before Docker popularized the idea of lightweight 
containers. Despite their age, jails remain a powerful, flexible, and minimal way to isolate services and processes on 
FreeBSD systems.

This post walks through a minimal "Hello World" setup using Jails, with just enough commentary to orient new users and 
show where jails shine in the modern world of virtualization.

## Why Jails?

A **FreeBSD jail** is a [chroot](https://en.wikipedia.org/wiki/Chroot)-like environment with its own file system, users, 
network interfaces, and process table. But unlike chroot, jails extend control to include process isolation, network 
access, and fine-grained permission control. They're more secure, more flexible, and more deeply integrated into the 
FreeBSD base system.

Here’s how jails compare with some familiar alternatives:

- **Versus VMs**: Jails don't emulate hardware or run separate kernels. They're faster to start, lighter on resources, and simpler to manage. But they're limited to the same FreeBSD kernel as the host.
- **Versus Docker**: Docker containers typically run on a Linux host and rely on a container runtime, layered filesystems, and extensive tooling. Jails are simpler, arguably more robust, and don’t require external daemons. However, they lack some of the ecosystem and portability benefits that Docker brings.

If you're already running FreeBSD and want to isolate services or test systems with minimal overhead, jails are a 
perfect fit.

## Setup

Let’s build a bare-bones jail. The goal here is simplicity: get a jail running with minimal commands. This is the BSD 
jail equivalent of “Hello, World.”

{% highlight bash %}
# Make a directory to hold the jail
mkdir hw

# Install a minimal FreeBSD userland into that directory
sudo bsdinstall jail /home/michael/src/jails/hw

# Start the jail with a name, IP address, and a shell
sudo jail -c name=hw host.hostname=hw.example.org \
    ip4.addr=192.168.1.190 \
    path=/home/michael/src/jails/hw \
    command=/bin/sh
{% endhighlight %}

You now have a running jail named `hw`, with a hostname and IP, running a shell isolated from the host system.

`192.168.1.190` is just a static address picked arbitrarily by me. For you, you'll want to pick an address that is 
reachable on your local network.

## Poking Around

With your jail up and running, that means you can start working with it. To enter the jail, you can use the following:

{% highlight shell %}
sudo jexec hw /bin/sh
{% endhighlight %}

`jexec` allows you to send any command that you need to into the jail to execute.

{% highlight shell %}
sudo jexec hw ls /
{% endhighlight %}

## Querying

You can list running jails with:

{% highlight bash %}
jls
{% endhighlight %}

You should see something like this:

{% highlight plain %}
JID  IP Address      Hostname                      Path
2    192.168.1.190   hw.example.org                /home/michael/src/jails/hw
{% endhighlight %}

You can also look at what's currently running in the jail:

{% highlight bash %}
ps -J hw
{% endhighlight %}

You should see the `/bin/sh` process:

{% highlight plain %}
PID TT  STAT    TIME COMMAND
2390  5  I+J  0:00.01 /bin/sh
{% endhighlight %}

## Finishing up

To terminate the jail:

{% highlight bash %}
sudo jail -r hw
{% endhighlight %}

This is a minimal setup with no automated networking, no jail management frameworks, and no persistent configuration. 
And that’s exactly the point: you can get a working jail in three commands and tear it down just as easily.

## When to Use Jails

Jails make sense when:

- You want process and network isolation on FreeBSD without the overhead of full VMs.
- You want to run multiple versions of a service (e.g., Postgres 13 and 15) on the same host.
- You want stronger guarantees than chroot provides for service containment.
- You’re building or testing FreeBSD-based systems and want a reproducible sandbox.

For more complex jail setups, FreeBSD offers tools like `ezjail`, `iocage`, and `bastille` that add automation and 
persistence. But it's worth knowing how the pieces fit together at the core.

## Conclusion

FreeBSD jails offer a uniquely minimal, powerful, and mature alternative to both VMs and containers. With just a few 
commands, you can create a secure, isolated environment for experimentation, testing, or even production workloads.

This post only scratched the surface, but hopefully it’s enough to get you curious. If you're already on FreeBSD, jails 
are just sitting there, waiting to be used—no extra software required.

