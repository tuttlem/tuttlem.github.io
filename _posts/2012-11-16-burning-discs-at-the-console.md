---
layout: post
title: Burning Discs at the Console
date: 2012-11-16
comments: false
---

A <em>very short </em>guide to burning CD and DVD media at the console with <strong>growisofs</strong>.

### Control

Ejecting the drive

{% highlight bash %}
eject
{% endhighlight %}  

Retracting the drive 

{% highlight bash %}
eject -T  
{% endhighlight %}

### Burning

Burn the contents of a directory  

{% highlight bash %}
growisofs -dvd-compat -Z /dev/dvd -R -J -pad "/path/to/data"
{% endhighlight %}

Burn the contents of multiple directories  

{% highlight bash %}
growisofs -dvd-compat -Z /dev/dvd -R -J -pad -graft-points "/foo=/path/to/foo" "/chu=/path/to/chu"
{% endhighlight %}

Burn an ISO to a disc

{% highlight bash %}
growisofs -dvd-compat -Z /dev/dvd=/path/to/image.iso
{% endhighlight %}

### RW Burning

Formatting a RW disc for burning  

{% highlight bash %}
dvd+rw-format -force /dev/dvd  
{% endhighlight %}

Appending data to a RW disc  

{% highlight bash %}
growisofs -dvd-compat -M /dev/dvd -R -J -pad -graft-points "/foo=/path/to/additional/data"
{% endhighlight %}
