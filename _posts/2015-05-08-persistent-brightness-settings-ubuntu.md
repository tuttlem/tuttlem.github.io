---
layout: post
title: Persistent brightness settings in Ubuntu
date: 2015-05-08
comments: false
categories: [ "ubuntu", "video" ]
---

A really quick tip for persistently setting your video brightness level in Ubuntu, originally picked up from [here](http://askubuntu.com/questions/327040/how-to-permanently-set-screen-brightness-level-in-ubuntu-12-04-on-dell-vostro-24).

Set the brightness level as you would normally with your control keys, then open a terminal to grab the current brightness setting:

{% highlight bash %}
cat /sys/class/backlight/acpi_video0/brightness
{% endhighlight %}

The value that you're given as the output here can be used in your `/etc/rc.local` startup script. As the last item <em>prior to the</em> `exit 0` statement, just add this:

{% highlight text %}
echo your_value_here > /sys/class/backlight/acpi_video0/brightness
{% endhighlight %}

