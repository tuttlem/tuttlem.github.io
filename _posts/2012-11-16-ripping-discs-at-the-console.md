---
layout: post
title: Ripping Discs at the Console
date: 2012-11-16
comments: false
categories: [ "Ripping", "Linux", "HandBrakeCLI" ]
---

A <em>very brief</em> guide to ripping DVD media at the console with <strong>handbrake</strong>.

{% highlight bash %}
# Rip a disc with the normal preset
HandBrakeCLI -i /dev/dvd -o output.mp4 --preset=Normal --main-feature  

# Scan a disc for chapter data
HandBrakeCLI -i /dev/dvd -t <chapter> --scan

# Rip a specific chapter
HandBrakeCLI -i /dev/dvd -o output.mp4 --preset=Normal -t <chapter>  
{% endhighlight %}