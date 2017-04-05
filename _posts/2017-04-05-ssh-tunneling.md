---
layout: post
title: SSH tunneling
date: 2017-04-05
comments: false
categories: [ "ssh", "tunnel" ]
---

[SSH Tunneling](https://en.wikipedia.org/wiki/Tunneling_protocol) is a technique that allows you to provide access to a network or service, through another access point. This is particularly useful when you aren't afforded immediate access to the network or service that you're trying to reach. Marshalling this traffic through a network protocol that you can immediately access (and allowing that secondary point to on-forward your network requests) you can achieve the access that you require.

From the [wikipedia article](https://en.wikipedia.org/wiki/Tunneling_protocol):

> In computer networks, a tunneling protocol allows a network user to access or provide a network service that the underlying network does not support or provide directly. 

In today's article, I'll demonstrate a basic setup for tunneling to different services.

The basic format that the command takes that you'll use, will look like this:

{% highlight text %}
ssh -f remote-user@remote-host -L local-port:remote-host:remote-port -N
{% endhighlight %}

`-f` tells the `ssh` command to drop into the background after its invocation. `-L` maps a `local-port` through the `remote-host` onto the `remote-port`. `-N` tells OpenSSH to not execute a command, remotely.

Examples of the `local-port:remote-host:remote-port` might look as follows.

A firewall that you're implicitly connected through (as a result of being at Starbucks, or at a hotel) isn't allowing you to connect to your home email server (on port 25). You setup a tunnel using `5000:my-email-host-at-home.com:25` and connect your email client to `localhost` on port `5000`. Data is now encrypted through the tunnel and is marshalled over the requested port.

Your company doesn't allow [IRC](https://en.wikipedia.org/wiki/Internet_Relay_Chat) traffic through it's firewall. You use `9000:irc.server.com:6667` to get around these restrictions; sending your chat data encrypted through the firewall.


