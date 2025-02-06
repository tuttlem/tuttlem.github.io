---
title: Formatting to FAT32 on FreeBSD
date: 2025-02-06
tags: [FreeBSD, Filesystems, USB, FAT32]
---

Formatting drives on any operating system can be a handful of instructions specific to that operating environment. In today's post we'll walk through the process of formatting a USB drive to FAT32, explaining each command along the way.

## Identifying the Device

Before making any changes, you need to determine which device corresponds to your USB drive. The best way to do this is:

{% highlight shell %}
dmesg | grep da
{% endhighlight %}

or, for a more detailed view:

{% highlight shell %}
geom disk list
{% endhighlight %}

On FreeBSD, USB mass storage devices are typically named `/dev/daX` (where `X` is a number). If you only have one USB drive plugged in, it is likely `/dev/da0`.

Device naming in FreeBSD is quite uniform:

* **USB Drives**: `/dev/daX`
* **SATA/SAS/IDE Drives**: `/dev/adaX`
* **NVMe Drives**: `/dev/nvmeX`
* **RAID Volumes**: `/dev/mfidX`, `/dev/raidX`

## Partitioning the Drive

Now that we know the device name, we need to set up a partition table and create a FAT32 partition.

### Destroying Existing Partitions

If the drive has existing partitions, remove them:

{% highlight shell %}
gpart destroy -F /dev/da0
{% endhighlight %}

This ensures a clean slate.

### Creating a Partition Table

We create a Master Boot Record (MBR) partition table using:

{% highlight shell %}
gpart create -s mbr /dev/da0
{% endhighlight %}

- `-s mbr`: Specifies an MBR (Master Boot Record) partition scheme.  
- Other options include `gpt` (GUID Partition Table), which is more modern but may not be supported by all systems.

### Adding a FAT32 Partition

Now, we create a FAT32 partition:

{% highlight shell %}
gpart add -t fat32 /dev/da0
{% endhighlight %}

- `-t fat32`: Specifies the FAT32 partition type.  
- Other valid types include `freebsd-ufs` (FreeBSD UFS), `freebsd-swap` (swap partition), `freebsd-zfs` (ZFS), and `linux-data` (Linux filesystem).  

After running this command, the new partition should be created as `/dev/da0s1`.

## Formatting the Partition as FAT32

To format the partition, we use `newfs_msdos`:

{% highlight shell %}
newfs_msdos -L DISKNAME -F 32 /dev/da0s1
{% endhighlight %}

- `-L DISKNAME`: Assigns a label to the volume.  
- `-F 32`: Specifies FAT32.  
- `/dev/da0s1`: The newly created partition.

> **Why `/dev/da0s1` instead of `/dev/da0`?**  
> When using MBR, partitions are numbered starting from `s1` (`slice 1`), meaning that the first partition on `da0` becomes `da0s1`. Using `/dev/da0` would format the entire disk, not just a partition.

## Wrapping Up

At this point, your USB drive is formatted as FAT32 and ready to use. You can mount it manually if needed:

{% highlight shell %}
mount -t msdosfs /dev/da0s1 /mnt
{% endhighlight %}

To safely remove the drive:

{% highlight shell %}
umount /mnt
{% endhighlight %}

