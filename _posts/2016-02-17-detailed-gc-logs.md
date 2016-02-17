---
layout: post
title: Detailed GC logs
date: 2016-02-17
comments: false
categories: [ "java", "gc" ]
---

From time to time, it makes sense to perform some [GC](http://www.oracle.com/webfolder/technetwork/tutorials/obe/java/gc01/index.html) tuning on your [Java Virtual Machines](https://en.wikipedia.org/wiki/Java_virtual_machine). Whilst there are a lot of tools that can visually help your debugging process, in today's post I'll talk you through the GC log that you can optionally turn on in your virtual machine arguments.

### Enabling the log

To boost up the logging of your application, you'll need to tune the execution runtime using command line parameters. The following parameters will get the JVM to log out information that it's holding on garbage collection events.

{% highlight text %}
-verbose:gc -XX:+PrintGCDetails -XX:+PrintGCTimeStamps -Xloggc:/tmp/gc.log
{% endhighlight %}

To explain these a little:

`-verbose:gc` will ramp the logging level of GC events up to a verbose level, `-XX:+PrintGCDetails` and `-XX:+PrintGCTimeStamps` define some features of the log that's written. Finally `-Xloggc:/tmp/gc.log` defines the file endpoint on disk that the GC log will be written to.

### Reading the log

After you've run your program with these parameters engaged, you should find the `/tmp/gc.log` file sitting on your hard drive waiting to be read. I won't dump the full log for the test program that I've run here; rather I'll go through it piece by piece.

The header of the file defines what your software versions, memory statistics and virtual machine arguments are. 

{% highlight text %}
OpenJDK 64-Bit Server VM (25.66-b01) for linux-amd64 JRE (1.8.0_66-internal-b01), built on Aug  5 2015 09:09:16 by "pbuilder" with gcc 4.9.2
Memory: 4k page, physical 8055396k(6008468k free), swap 8267772k(8267772k free)
CommandLine flags: -XX:InitialHeapSize=1073741824 -XX:MaxHeapSize=1073741824 -XX:+PrintGC -XX:+PrintGCDetails -XX:+PrintGCTimeStamps -XX:+UseCompressedClassPointers -XX:+UseCompressedOops -XX:+UseParallelGC 
{% endhighlight %}

After these initial lines, you'll start to see some of the memory allocation events appear along with the timestamps *(remember, we asked for timestamps above)*.

{% highlight text %}
0.320: [GC (Allocation Failure) [PSYoungGen: 262144K->43488K(305664K)] 262144K->142896K(1005056K), 0.1552445 secs] [Times: user=0.40 sys=0.20, real=0.16 secs] 
0.649: [GC (Allocation Failure) [PSYoungGen: 305632K->43504K(305664K)] 405040K->275088K(1005056K), 0.2105517 secs] [Times: user=0.58 sys=0.26, real=0.21 secs] 
0.986: [GC (System.gc()) [PSYoungGen: 219445K->43520K(305664K)] 451029K->369480K(1005056K), 0.1570988 secs] [Times: user=0.47 sys=0.14, real=0.15 secs] 
1.143: [Full GC (System.gc()) [PSYoungGen: 43520K->0K(305664K)] [ParOldGen: 325960K->368132K(699392K)] 369480K->368132K(1005056K), [Metaspace: 2530K->2530K(1056768K)], 2.5983336 secs] [Times: user=9.55 sys=0.03, real=2.59 secs] 
3.984: [GC (Allocation Failure) [PSYoungGen: 262144K->32K(305664K)] 630276K->368164K(1005056K), 0.0049817 secs] [Times: user=0.01 sys=0.00, real=0.00 secs] 
4.070: [GC (System.gc()) [PSYoungGen: 108791K->32K(305664K)] 476924K->368164K(1005056K), 0.0041558 secs] [Times: user=0.02 sys=0.00, real=0.00 secs] 
4.074: [Full GC (System.gc()) [PSYoungGen: 32K->0K(305664K)] [ParOldGen: 368132K->133835K(699392K)] 368164K->133835K(1005056K), [Metaspace: 2539K->2539K(1056768K)], 0.4427402 secs] [Times: user=1.59 sys=0.01, real=0.45 secs] 
{% endhighlight %}

Pulling one of these lines apart:

{% highlight text %}
0.320: [GC (Allocation Failure) [PSYoungGen: 262144K->43488K(305664K)] 262144K->142896K(1005056K), 0.1552445 secs] [Times: user=0.40 sys=0.20, real=0.16 secs] 
{% endhighlight %}

This event was generated `0.320` seconds into the program. This item is a `GC (Allocation Failure)` event and it's being reported on the `PSYoungGen` collection. Prior to the event, the space allocated before was `262144K` and after was `43488K`. The capacity value is in braces `305664K`.

The `Full GC` events will give you statistics for all of the memory collections:

{% highlight text %}
1.143: [Full GC (System.gc()) [PSYoungGen: 43520K->0K(305664K)] [ParOldGen: 325960K->368132K(699392K)] 369480K->368132K(1005056K), [Metaspace: 2530K->2530K(1056768K)], 2.5983336 secs] [Times: user=9.55 sys=0.03, real=2.59 secs] 
{% endhighlight %}

Each of the collections is displayed as `[CollectionName: SpaceBefore->SpaceAfter(Capacity)]`. 

Finally, we have a heap analysis of the program as it breaks down amongst the different memory classes: Young Gen, Old Gen and *(new for 1.8)* Metaspace. Metaspace would have previously been Perm Gen.

{% highlight text %}
Heap
 PSYoungGen      total 305664K, used 5243K [0x00000000eab00000, 0x0000000100000000, 0x0000000100000000)
  eden space 262144K, 2% used [0x00000000eab00000,0x00000000eb01ecf8,0x00000000fab00000)
  from space 43520K, 0% used [0x00000000fab00000,0x00000000fab00000,0x00000000fd580000)
  to   space 43520K, 0% used [0x00000000fd580000,0x00000000fd580000,0x0000000100000000)
 ParOldGen       total 699392K, used 133835K [0x00000000c0000000, 0x00000000eab00000, 0x00000000eab00000)
  object space 699392K, 19% used [0x00000000c0000000,0x00000000c82b2c88,0x00000000eab00000)
 Metaspace       used 2546K, capacity 4486K, committed 4864K, reserved 1056768K
  class space    used 268K, capacity 386K, committed 512K, reserved 1048576K
{% endhighlight %}

