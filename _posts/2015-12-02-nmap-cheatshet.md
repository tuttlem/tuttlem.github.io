---
layout: post
title: nmap Cheatsheet
date: 2015-12-02
comments: false
categories: [ "nmap", "security", "admin" ]
---

The following post is a quick guide to getting around the [nmap](https://nmap.org/) network administration and security tool. 

### General scanning

Scanning with nmap gives you the insight into what is available to a server (from an external user's perspective). Information about the techniques that nmap will use can be found [here](https://nmap.org/nmap_doc.html).

{% highlight bash %}
# scan a host by ip/name
nmap 192.168.0.1
nmap host.example.com

# scan multiple hosts
nmap 192.168.0.1 192.168.0.2 192.168.0.3
nmap 192.168.0.1,2,3

# range scanning
nmap 192.168.0.1-3
nmap 192.168.0.*

# subnet scanning
nmap 192.168.0.0/24
{% endhighlight %}

### Utilities

| Command                           | Description                                 |
|-----------------------------------|---------------------------------------------|
| `nmap -v -A 192.168.0.1`          | Turn on OS and version detection |
| `nmap -sA 192.168.0.1`            | Check for a firewall |
| `nmap -PN 192.168.0.1`            | Scan a firewall protected host |
| `nmap -6 ::1`                     | Scan IPv6 address |
| `nmap -sP 192.168.0.1/24`         | Check for alive hosts |
| `nmap --reason 192.168.0.1`       | Document the reason for a service discovery |
| `nmap --open 192.168.0.1`         | Show open ports |
| `nmap --packet-trace 192.168.0.1` | Show packet trace (sent/received) |
| `nmap --iflist`                   | Show host interface and routes |
| `nmap -O 192.168.0.1`             | Detect remote operating system |
| `nmap -sV 192.168.0.1`            | Detect remote service/daemon version |
| `nmap -sO 192.168.0.1`            | Scan for IP protocol |


### Port scans

| Command                           | Description                                 |
|-----------------------------------|---------------------------------------------|
| `nmap -p 80 192.168.0.1`          | Scan http                                   |
| `nmap -p T:80 192.168.0.1`        | Scan tcp/http                               |
| `nmap -p U:53 192.168.0.1`        | Scan udp/dns                                |

### Firewalls

The following commands scan firewalls for weaknesses

{% highlight bash %}
# tcp null scan
nmap -sN 192.168.0.1

# tcp fin scan
nmap -sF 192.168.0.1

# tcp xmas scan
nmap -sX 192.168.0.1

# scan a firewall for packet fragments
nmap -f 192.168.0.1
{% endhighlight %}

### Spoof

{% highlight bash %}
# cloak a scan with decoys
nmap -n -Ddecoy1.example.com,decoy2.example.com 192.168.0.1

# scan with a spoofed mac address
nmap --spoof-mac MAC-ADDRESS-HERE 192.168.0.1

# scan with a random mac address
nmap -v -sT -PN --spoof-mac 0 192.168.0.1
{% endhighlight %}
