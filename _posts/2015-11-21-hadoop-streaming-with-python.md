---
layout: post
title: Hadoop Streaming with Python
date: 2015-11-21
comments: false
categories: [ "hadoop", "streaming", "mapreduce", "python" ]
---

[Hadoop]() provides a very rich API interface for developing and running [MapReduce](https://en.wikipedia.org/wiki/MapReduce) jobs in [Java](https://www.java.com/en/download/), however this is not always everybody's preference. [Hadoop Streaming](https://hadoop.apache.org/docs/r1.2.1/streaming.html) makes it possible to run MapReduce jobs with any language that can access the [standard streams](https://en.wikipedia.org/wiki/Standard_streams) `STDIN` and `STDOUT`. 

Hadoop Streaming creates the plumbing required to build a full map reduce job out to your cluster so that all you need to do is supply a [mapper](https://en.wikipedia.org/wiki/MapReduce#Map_function) and [reducer](https://en.wikipedia.org/wiki/MapReduce#Reduce_function) that uses STDIN for their input and STDOUT for their output.  

In today's example, we'll re-implement the [word count](https://wiki.apache.org/hadoop/WordCount) example with python using streaming.

### The mapper

In this case, the mapper's job is to take a line of text (input) a break it into words. We'll then write the word along with the number `1` to denote that we've *counted* it.

{% highlight python %}
#!/usr/bin/env python

import sys

def read_input(file):
  '''Splits the lines given to it into words and
     produces a generator'''

  for line in file:
      yield line.split()

def main():
  '''Produces (word,1) pairs for every word 
     encountered on the input'''

  data = read_input(sys.stdin)

  for words in data:
      for word in words:
          print '%s,%d' % (word, 1)

if __name__ == "__main__":
  main()
{% endhighlight %}

### The reducer

The reducers' job is to come through and process the output of the map function, perform some aggregative operation over the set and produce an output set on this information. In this example, it'll take the word and each of the 1's, accumulating them to form a word count.

{% highlight python %}
#!/usr/bin/env python

from itertools import groupby
from operator import itemgetter
import sys

def parse_output(file):
  '''Parses a single line of output produced 
     by the mapper function'''

  for line in file:
      yield line.rstrip().split(',', 1)

def main():
  data = parse_output(sys.stdin)

  # produce grouped pairs to count
  for current_word, group in groupby(data, itemgetter(0)):
    try:

      # produce the total count      
      total_count = sum(int(count) for current_word, count in group)
    
      # send it out to the output
      print "%s,%d" % (current_word, total_count)
    except ValueError:
      # ignore casting errors
      pass

if __name__ == "__main__":
  main()
{% endhighlight %}

### input | map | sort | reduce

Before we full scale with this job, we can simulate the work that the Hadoop cluster would do for us by using our shell and pipe indirection to test it out. This is not a **scale** solution, so make sure you're only giving it a small set of data. We can really treat this process as:

[The Zen and the Art of the Internet](http://textfiles.com/etext/MODERN/zen10.txt) should do, just fine.

{% highlight bash %}
$ cat zen10.txt | ./mapper.py | sort -k1,1 | ./reducer.py
{% endhighlight %}

We can now submit this job to the hadoop cluster like so. Remember, we need access to our source data, mapper and reducer from the namenode where we'll submit this job from.

### Submitting your job on the cluster

First, we need to get our input data in an accessible spot on the cluster.

{% highlight bash %}
$ bin/hadoop fs -mkdir /user/hadoop
$ bin/hadoop fs -put /srv/zen10.txt /user/hadoop
{% endhighlight %}

Make sure it's there:

{% highlight bash %}
$ bin/hadoop fs -ls /user/hadoop
{% endhighlight %}

{% highlight text %}
Found 1 items
-rw-r--r--   1 root supergroup     176012 2015-11-20 23:03 /user/hadoop/zen10.txt
{% endhighlight %}

Now, we can run the job.

{% highlight bash %}
$ bin/hadoop jar share/hadoop/tools/lib/hadoop-streaming-2.7.0.jar \
             -mapper /src/mapper.py \
             -reducer /src/reducer.py 
             -input /user/hadoop/zen10.txt \
             -output /user/hadoop/zen10-output \
{% endhighlight %}

The `-mapper` and `-reducer` switches are referring to files on the actual linux node whereas `-input` and `-output` are referring to HDFS locations.

### Results

The results are now available for you in `/user/hadoop/zen10-output`.

{% highlight bash %}
$ bin/hadoop fs -cat \
             /user/hadoop/zen10-output/part-00000
{% endhighlight %}

You should see the results start spraying down the page.

{% highlight text %}
. . .
. . .

vernacular,1
version,10
versions,9
very,13
via,20
vic-20,2
vice,1

. . .
. . .
{% endhighlight %}

### Limitation

So far, the only limitation that I've come across with this method of creating map reduce jobs is that the mapper will only work line-by-line. You can't treat a single record as information spanning across multiple lines. Having information span across multiple lines in your data file should be a rare use case though.

