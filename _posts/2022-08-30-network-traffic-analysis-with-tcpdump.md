---
layout: post
title: Network Traffic Analysis with tcpdump
date: 2022-08-30
comments: false
categories: [ "network", "tcpdump" ]
---

### Introduction

Sometimes it can be of value to be able to isolate and analyse specific network
traffic that is flowing through your network interface. [tcpdump](https://www.tcpdump.org/)
offers you this capability in a command line application.

There are many tutorials already that take you through tcpdump comprehensively, 
so this article will just be constrained to usages that have benefited me.

### Reading the output

In order for this tool to be of any use, it pays to know how to read the output. In
this example I'm capturing all of the port 80 traffic flowing through my network
interface.

{% highlight shell %}
tcpdump -nnSX port 80
{% endhighlight %}

The stream of output that you see after this (once you have some port 80 traffic going)
is the output that you'll use for analysis. Here's an excerpt after hitting 
[the first page on the internet](http://info.cern.ch/hypertext/WWW/TheProject.html).

{% highlight plain %}
21:49:54.056071 IP 192.168.20.35.60548 > 188.184.21.108.80: Flags [P.], seq 3043086668:3043087150, ack 3119373143, win 502, options [nop,nop,TS val 3532078292 ecr 2432081556], length 482: HTTP: GET /hypertext/WWW/TheProject.html HTTP/1.1
        0x0000:  4500 0216 a4d4 4000 4006 ed1d c0a8 1423  E.....@.@......#
        0x0010:  bcb8 156c ec84 0050 b561 d14c b9ed db57  ...l...P.a.L...W
        0x0020:  8018 01f6 62a6 0000 0101 080a d287 3cd4  ....b.........<.
        0x0030:  90f6 9e94 4745 5420 2f68 7970 6572 7465  ....GET./hyperte
        0x0040:  7874 2f57 5757 2f54 6865 5072 6f6a 6563  xt/WWW/TheProjec
        0x0050:  742e 6874 6d6c 2048 5454 502f 312e 310d  t.html.HTTP/1.1.
{% endhighlight %}

There's lots here.

We're given the time of the packet being observed `21:49:54.056071`.

We're given the network layer protocol `IP`, source address (my machine) `192.168.20.35` and port `60584`; along 
with the destination `188.184.21.108` (on port `80`).

The next field `Flags [P.]` is an encoded representation of the TCP flags. The following table gives a
breakdown of these flag values.

| Value | Flag | Description |
|-------|------|-------------|
| S     | SYN  | Connection start |
| F     | FIN  | Connection finish |
| P     | PUSH | Data push |
| R     | RST  | Connection reset |
| .     | ACK  | Acknowledgement |

The combination of values tells you the flags that are up. In this case `P.` tells us this is a `PUSH-ACK` packet.

The sequence number `seq 3043086668:3043087150` tells us the run of bytes contained within this sample. The `ack` 
value `ack 3119373143` is the next byte expected. The `win` value tells us the number of bytes available in the 
buffer followed by the TCP options.

The packet length is given at the end of the line.

The data frame is now split into a hexadecimal representation in the middle (given by `-X`); and the ASCII representation to the 
right.

With the basic output view out of the way, we get move onto some useful invocations.

### Invocations

#### Filter by Port

As per the above example, we can filter traffic by any port that we give to `port` switch. Here
we can see any SMTP traffic.

{% highlight shell %}
# just the traffic on port 25
tcpdump port 25

# traffic on ports ranging from 25 to 30
tcpdump portrange 25-30
{% endhighlight %}

#### Everything

Sometimes it can be useful to just receive everything flowing through a network interface.

{% highlight shell %}
tcpdump -i wlp4s0
{% endhighlight %}

#### Filter by Host

You can use the `host` keyword to see traffic going to or coming from an IP address. You can constrain this
even further using `src` (coming from) or `dest` (going to).

{% highlight shell %}
tcpdump host 192.168.20.1

# packets going to 20.1
tcpdump dst 192.168.20.1

# packets coming from 20.1
tcpdump src 192.168.20.1
{% endhighlight %}

#### Filter by Network

Using broader strokes, you can use `net` to specify a full network to filter packets on. This will allow you
to filter a whole network or subnet.

{% highlight shell %}
tcpdump net 192.168.20.0/24
{% endhighlight %}

#### Filter by Protocol

Just seeing ping (ICMP) traffic can be filtered like so:

{% highlight shell %}
tcpdump icmp
{% endhighlight %}


### Conclusion

tcpdump is a very useful network analysis tool do perform discoveries on what's actually happening. There's
a lot more power that can be unlocked by combining some of these basic filters together using logical 
concatenators. 
