---
layout: post
title: unqualified host name; sleeping for retry
date: 2014-07-19
comments: false
categories: ["FreeBSD", "Boot", "sm-mta", "sm-msp-queue"]
---

After a fresh installation of FreeBSD today, I'd noticed that my boot up time was suffering due to this message which consistently appears:

{% highlight text %}
Jul 19 16:58:38 freebsd sm-mta[1097]: My unqualified host name (freebsd) unknown; sleeping for retry
Jul 19 16:59:38 freebsd sm-mta[1097]: unable to qualify my own domain name (freebsd) -- using short name
Jul 19 16:59:38 freebsd sm-msp-queue[1100]: My unqualified host name (freebsd) unknown; sleeping for retry
Jul 19 17:00:38 freebsd sm-msp-queue[1100]: unable to qualify my own domain name (freebsd) -- using short 
{% endhighlight %}

As you can see, this machine that I've created does have the <em>very original</em> hostname of <strong>freebsd</strong>.

What the error message is telling us is that I need to fully qualify my hostname. Editing <em>/etc/rc.conf</em> you can change the `hostname` value to include this information. The top line of my <em>rc.conf</em> now reads as follows:

{% highlight text %}
hostname="freebsd.home"
{% endhighlight %}

No more slow boot times; because of this problem, at least.
