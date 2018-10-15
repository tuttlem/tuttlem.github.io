---
layout: post
title: FAT in Linux
date: 2018-10-16
comments: false
categories: [ "fat", "fs", "dos" ]
---

It has to be said that the most popular transfer format (when it comes to file systems) is either FAT32 or NTFS. In today's article I'll walk you through creating one of these lowest-common-denominator devices.

First of all, we need to find the device that you want to format. After you've attached your pendrive/device, use the `lsblk` command to determine what your device's name is.

{% highlight text %}
➜  ~ lsblk
NAME        MAJ:MIN RM   SIZE RO TYPE MOUNTPOINT
sda           8:0    1  29.8G  0 disk
{% endhighlight %}

In my case here, it's called `sda`. 

First of all, we'll partition the drive using `fdisk`.

### Partitioning

{% highlight text %}
➜  ~ sudo fdisk /dev/sda

Command (m for help): p
Disk /dev/sda: 29.8 GiB, 32015679488 bytes, 62530624 sectors
Units: sectors of 1 * 512 = 512 bytes
Sector size (logical/physical): 512 bytes / 512 bytes
I/O size (minimum/optimal): 512 bytes / 512 bytes
Disklabel type: dos
Disk identifier: 0xcfaecd67
{% endhighlight %}

We'll create a single partition for the device.

{% highlight text %}
Command (m for help): n
Partition type
   p   primary (0 primary, 0 extended, 4 free)
   e   extended (container for logical partitions)
Select (default p):

Using default response p.
Partition number (1-4, default 1):
First sector (2048-62530623, default 2048):
Last sector, +sectors or +size{K,M,G,T,P} (2048-62530623, default 62530623):

Created a new partition 1 of type 'Linux' and of size 29.8 GiB.
{% endhighlight %}

We can take a look at how the partition table now looks with `p`.

{% highlight text %}
Command (m for help): p
Disk /dev/sda: 29.8 GiB, 32015679488 bytes, 62530624 sectors
Units: sectors of 1 * 512 = 512 bytes
Sector size (logical/physical): 512 bytes / 512 bytes
I/O size (minimum/optimal): 512 bytes / 512 bytes
Disklabel type: dos
Disk identifier: 0xcfaecd67

Device     Boot Start      End  Sectors  Size Id Type
/dev/sda1        2048 62530623 62528576 29.8G 83 Linux
{% endhighlight %}

We still need to change the type from `Linux` to `W95 FAT32`, which has a code of `b`.

{% highlight text %}
Command (m for help): t
Selected partition 1
Hex code (type L to list all codes): b
Changed type of partition 'Linux' to 'W95 FAT32'.
{% endhighlight %}

We now finish partitioning and move onto formatting. We write the partition table with `w`.

{% highlight text %}
Command (m for help): w
The partition table has been altered.
Calling ioctl() to re-read partition table.
Syncing disks.
{% endhighlight %}

### Formatting

Finally, we use `mkfs` to create a `vfat` filesystem on our device's partition.

{% highlight text %}
➜  ~ sudo mkfs -t vfat /dev/sda1
mkfs.fat 4.1 (2017-01-24)
{% endhighlight %}

Remove the USB and then plug it back in. After it mounts automatically, we can verify with `df`.

{% highlight text %}
Filesystem     Type      Size  Used Avail Use% Mounted on
. . .
. . .
/dev/sda1      vfat       30G   16K   30G   1% /run/media/user/58E6-54A3
{% endhighlight %}

Ready to go.
