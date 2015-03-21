---
layout: post
title: sysstat utilities
date: 2015-03-21
comments: false
categories: [ "sysstat", "linux", "monitoring" ]
---

[sysstat](http://sebastien.godard.pagesperso-orange.fr/) is a collection of utilities for Linux that provide performance and activity usage monitoring. In today's post, I'll go through a brief explanation of these utilities.

### iostat

> iostat(1) reports CPU statistics and input/output statistics for devices, partitions and network filesystems.

{% highlight text %}
Linux 3.13.0-46-generic (thor) 	21/03/15 	_x86_64_	(8 CPU)

avg-cpu:  %user   %nice %system %iowait  %steal   %idle
           1.53    0.01    0.46    0.09    0.00   97.92

Device:            tps    kB_read/s    kB_wrtn/s    kB_read    kB_wrtn
sda              15.87       216.84       169.79     755449     591528
{% endhighlight %} 

`iostat` provides a top level cpu report in its first line with a breakdown percentages for the amount of time the cpu is spent:

* In user space (%user)
* In user space with nice priority (%nice)
* In kernel space (%system)
* Waiting on I/O (%iowait)
* Forced to wait from the hypervisor (%steal)
* Doing nothing (%idle)

Secondly, a breakdown by device is given of disk activity. In this chart, it shows the disk devices':

* Transfer per second (tps)
* Amount of data read per second (kB_read/s)
* Amount of data written per second (kB_wrtn/s)
* Total read (kB_read)
* Total write (kB_wrtn)

### mpstat

> mpstat(1) reports individual or combined processor related statistics.

{% highlight text %}
Linux 3.13.0-46-generic (thor) 	21/03/15 	_x86_64_	(8 CPU)

15:23:58     CPU    %usr   %nice    %sys %iowait    %irq   %soft  %steal  %guest  %gnice   %idle
15:23:58     all    1.36    0.01    0.41    0.08    0.00    0.00    0.00    0.00    0.00   98.14
{% endhighlight %}

`mpstat` goes a little deeper into how the cpu time is divided up among its responsibilities. By specifying `-P ALL` on the command line to it, you can get a report per cpu:

{% highlight text %}
Linux 3.13.0-46-generic (thor) 	21/03/15 	_x86_64_	(8 CPU)

16:05:10     CPU    %usr   %nice    %sys %iowait    %irq   %soft  %steal  %guest  %gnice   %idle
16:05:10     all    1.58    0.00    0.42    0.06    0.00    0.00    0.00    0.00    0.00   97.94
16:05:10       0    2.05    0.00    0.74    0.04    0.00    0.00    0.00    0.00    0.00   97.17
16:05:10       1    2.62    0.02    0.58    0.03    0.00    0.00    0.00    0.00    0.00   96.76
16:05:10       2    2.84    0.00    0.59    0.03    0.00    0.00    0.00    0.00    0.00   96.54
16:05:10       3    2.31    0.00    0.67    0.02    0.00    0.00    0.00    0.00    0.00   97.00
16:05:10       4    0.83    0.00    0.18    0.10    0.00    0.00    0.00    0.00    0.00   98.89
16:05:10       5    0.65    0.02    0.20    0.10    0.00    0.00    0.00    0.00    0.00   99.03
16:05:10       6    0.71    0.00    0.20    0.08    0.00    0.00    0.00    0.00    0.00   99.01
16:05:10       7    0.64    0.00    0.17    0.06    0.00    0.00    0.00    0.00    0.00   99.13
{% endhighlight %}

### pidstat

> pidstat(1) reports statistics for Linux tasks (processes) : I/O, CPU, memory, etc.

{% highlight text %}
Linux 3.13.0-46-generic (thor) 	21/03/15 	_x86_64_	(8 CPU)

16:06:19      UID       PID    %usr %system  %guest    %CPU   CPU  Command
16:06:19        0         1    0.00    0.01    0.00    0.02     0  init
16:06:19        0         7    0.00    0.02    0.00    0.02     3  rcu_sched

. . .
. . .
{% endhighlight %}

`pidstat` will give you the utilisation breakdown by process that's running on your system. 

### sar

> sar(1) collects, reports and saves system activity information (CPU, memory, disks, interrupts, network interfaces, TTY, kernel tables,etc.)

`sar` requires that data collection is on to be used. The settings defined in `/etc/default/sysstat` will control this collection process. As `sar` is the collection mechanism, other applications use this data:

> sadc(8) is the system activity data collector, used as a backend for sar.

> sa1(8) collects and stores binary data in the system activity daily data file. It is a front end to sadc designed to be run from cron. 

> sa2(8) writes a summarized daily activity report. It is a front end to sar designed to be run from cron. 

> sadf(1) displays data collected by sar in multiple formats (CSV, XML, etc.) This is useful to load performance data into a database, or import them in a spreadsheet to make graphs.

### nfs and cifs

[NFS](http://en.wikipedia.org/wiki/Network_File_System) and [CIFS](https://technet.microsoft.com/en-us/library/cc939973.aspx) also have monitoring utilities.

> nfsiostat-sysstat(1) reports input/output statistics for network filesystems (NFS).

> cifsiostat(1) reports CIFS statistics.

These certainly come in handy when you've got remote shares running from your machine.