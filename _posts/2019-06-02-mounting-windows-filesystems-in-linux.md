---
layout: post
title: Mounting windows filesystems in Linux
date: 2019-06-02
comments: false
categories: [ "cifs", "samba", "linux" ]
---

A quick note on mounting windows filesystems on linux.

Your platform will require `cifs-utils`.

{% highlight text %}
sudo apt install cifs-utils
{% endhighlight %}

From here, you can connect and mount a remote file system.

{% highlight text %}
sudo mount -t cifs //ip.address.of.windows/location/to/mount /mnt -o user=john,password=mypassword,domain=mydomain
{% endhighlight %}

Done. You can now access the remote filesystem.


