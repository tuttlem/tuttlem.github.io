---
layout: post
title: RAID array stability
date: 2024-05-04
comments: false
categories: [ "raid", "admin", "usb" ]
---

### Introduction

Recently, I've purchased a [Terramaster D2-310](https://www.terra-master.com/global/d2-310.html) RAID array.

There were some important configurations that I needed to put together in order to get this unit to perform correctly.

### Problems

After getting everything setup, I had two [4TB BarraCuda drives](https://www.seagate.com/au/en/products/hard-drives/barracuda-hard-drive/) plugged in ready to go.
I have this system running as a [RAID 1](https://en.wikipedia.org/wiki/Standard_RAID_levels) so that I have a mirror of my data.

After starting to transfer data onto the device, I noticed that copy jobs would grind to a halt; as well as the error log being full of the following:

{% highlight text %}
[sda] tag#7 uas_zap_pending 0 uas-tag 13 inflight: CMD 
[sda] tag#7 CDB: Write(16) 8a 00 00 00 00 00 17 4f 50 10 00 00 04 00 00 00
[sda] tag#8 uas_zap_pending 0 uas-tag 14 inflight: CMD 
[sda] tag#8 CDB: Write(16) 8a 00 00 00 00 00 17 4f 50 00 00 00 00 10 00 00
[sda] tag#9 uas_zap_pending 0 uas-tag 15 inflight: CMD 
[sda] tag#9 CDB: Write(16) 8a 00 00 00 00 00 17 4f 4c 00 00 00 04 00 00 00
[sda] tag#10 uas_zap_pending 0 uas-tag 16 inflight: CMD 
[sda] tag#10 CDB: Write(16) 8a 00 00 00 00 00 17 4f 48 00 00 00 04 00 00 00
[sda] tag#11 uas_zap_pending 0 uas-tag 17 inflight: CMD 
[sda] tag#11 CDB: Write(16) 8a 00 00 00 00 00 17 4d 44 00 00 00 04 00 00 00
[sda] tag#13 uas_zap_pending 0 uas-tag 18 inflight: CMD 
[sda] tag#13 CDB: Synchronize Cache(10) 35 00 00 00 00 00 00 00 00 00
{% endhighlight %}

There's also signs of crashes from the disk as well:

{% highlight text %}
task:jbd2/sda1-8     state:D stack:0     pid:7414  tgi>
Call Trace:
 <TASK>
 __schedule+0x27c/0x6b0
 schedule+0x33/0x110
 io_schedule+0x46/0x80
 bit_wait_io+0x11/0x90
 __wait_on_bit+0x42/0x110
 ? __pfx_bit_wait_io+0x10/0x10
 out_of_line_wait_on_bit+0x8c/0xb0
 ? __pfx_wake_bit_function+0x10/0x10
 __wait_on_buffer+0x30/0x50
 jbd2_journal_commit_transaction+0x16da/0x1af0
 ? lock_timer_base+0x3b/0xe0
 kjournald2+0xab/0x280
 ? __pfx_autoremove_wake_function+0x10/0x10
 ? __pfx_kjournald2+0x10/0x10
 kthread+0xef/0x120
 ? __pfx_kthread+0x10/0x10
 ret_from_fork+0x44/0x70
 ? __pfx_kthread+0x10/0x10
 ret_from_fork_asm+0x1b/0x30
 </TASK>
{% endhighlight %}

Ugh ...

### Disable USB Auto Suspend

I thought there might have been a suspend issue with the USB device. In order to achieve this, I needed to send the kernel some parameters from GRUB.

Opening `/etc/default/grub`, we can start adding items to `GRUB_CMDLINE_LINUX_DEFAULT`:

{% highlight text %}
GRUB_CMDLINE_LINUX_DEFAULT="usbcore.autosuspend=-1"
{% endhighlight %}

### Failing back to usb_storage

Looking at [some discussions](https://discussion.fedoraproject.org/t/i-o-errors-with-dd-rsync-but-not-with-nautilus-or-deja-dup/24897) I can see that a lot of people were having success with blacklisting the uas driver that the vendor provides, failing back to the standard driver from linux.

So, I did that too.

To do that, you need to pass the IDs as quirks into the blacklist request. To get those IDs, you need to look at the output from `lsusb`:

{% highlight text %}
$ lsusb
Bus 001 Device 001: ID 1d6b:0002 Linux Foundation 2.0 root hub
Bus 002 Device 001: ID 1d6b:0003 Linux Foundation 3.0 root hub
Bus 003 Device 001: ID 1d6b:0002 Linux Foundation 2.0 root hub
Bus 003 Device 002: ID 174c:2074 ASMedia Technology Inc. ASM1074 High-Speed hub
Bus 003 Device 003: ID 046d:c548 Logitech, Inc. Logi Bolt Receiver
Bus 003 Device 004: ID 8087:0033 Intel Corp. AX211 Bluetooth
Bus 004 Device 001: ID 1d6b:0003 Linux Foundation 3.0 root hub
Bus 004 Device 002: ID 174c:3074 ASMedia Technology Inc. ASM1074 SuperSpeed hub
Bus 004 Device 003: ID 0480:a006 Toshiba America Inc UAS Controller
{% endhighlight %}

The Toshiba, and ASMedia items were of interest to me.

Create a file `/etc/modprobe.d/blacklist-uas.conf`

I added this text:

{% highlight text %}
options usb-storage quirks=174c:2074:u,174c:3074:u,0480:a006:u
{% endhighlight %}

Remember to specify the `u` at the end of the ID strings. It's literal, and need to be there.

### Conclusion

This thing works - and works really well now.