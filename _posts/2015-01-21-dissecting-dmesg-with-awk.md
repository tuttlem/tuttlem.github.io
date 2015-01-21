---
layout: post
title: Dissecting dmesg with awk
date: 2015-01-21
comments: false
categories: [ "awk", "dmesg" ]
---

[AWK](http://www.gnu.org/software/gawk/manual/gawk.html) is a programming language that deals with processing text in a sequence of pattern matching rules. It's really handy for reducing massive amounts of text into just the information that you care about. The full user guide for AWK can be found [here](http://www.gnu.org/software/gawk/manual/gawk.html). 

Rather than take you on a tour through the user guide, I thought today's post might be better as a practical example. I'm going to present some useful functions with AWK using the Linux Kernel's [dmesg](http://en.wikipedia.org/wiki/Dmesg) output as source data.

As a final note, a lot if not all of the information that I'll present below can be transformed into a "one liner". There's [quite](http://www.pement.org/awk/awk1line.txt) [a](http://tuxgraphics.org/~guido/scripts/awk-one-liner.html) [few](http://www.staff.science.uu.nl/~oostr102/docs/nawk/nawk_41.html) [instances](http://www.catonmat.net/blog/awk-one-liners-explained-part-one/) of crafty AWK hackers putting these together. I just want to present some of the language.

### Source data

The dmesg data is in an easy-enough format to work with. Taking the first few lines as an example:

{% highlight text %}
[    0.000000] Initializing cgroup subsys cpuset
[    0.000000] Initializing cgroup subsys cpu
[    0.000000] Initializing cgroup subsys cpuacct
{% endhighlight %}

We see that there is an elapsed time figure surrounded with square brackets, the rest of the line is the log text. Further on through the text, we start to see the log lines prefixed with a driver name also:

{% highlight text %}
[    4.871693] vboxdrv: Found 8 processor cores.
[    4.872033] vboxdrv: fAsync=0 offMin=0x19e offMax=0xcb6
{% endhighlight %}

### Basic usage

For the purposes of today's post, the following usage is going to be most useful to us

{% highlight bash %}
dmesg | awk -f our-awk-script.awk
{% endhighlight %}

This supplies the dmesg output to our AWK script.

### Print any line with the word "failed" in it

To accomplish this task, we're going to use a regular expression to pick out each line with "fail" in it.

{% highlight awk %}
/ failed / {
	print $0
}
{% endhighlight %}

Immediately, you can see that AWK statements take the shape of:

{% highlight text %}
condition { actions }
{% endhighlight %}

The action here `print $0` prints the whole, captured line to the console. Other variables are available to be printed such as `$1`, `$2`, and so on. These numbered variables take chunks of the captured string, split by a space character as its delimiter.

### Exploring the variables

Just to take a look at those variables a little closer, we can augment our initial rule slightly to see what's contained in those variables:

{% highlight awk %}
/ failed / {
	print "$0: ", $0
	print "$1: ", $1
	print "$2: ", $2
	print "$3: ", $3
	print "$4: ", $4
}
{% endhighlight %}

Run for one line of text matching the "failed" rule:

{% highlight text %}
$0:  [    1.804314] iwlwifi 0000:03:00.0: Direct firmware load failed with error -2
$1:  [
$2:  1.804314]
$3:  iwlwifi
$4:  0000:03:00.0:
{% endhighlight %}

### Listing out which drivers mentioned the word "failed"

AWK has a very flexible associative array type as well. We can basically reference any variable with any index we choose. For the next progression of this script, we'll build an array of driver names with an instance count so we can just give the user a report of the which drivers were mentioned how many times.

{% highlight awk %}
/ failed / { 
	drivers[$3] = drivers[$3] + 1
}

END {
	for (driver in drivers) {
		print driver ":", drivers[driver]
	}	
}
{% endhighlight %}

`$3` is giving us the driver name, so we just increment a value in the array for that driver. `END` is something new. It's executed, at the end. We enumerate the array that we've built, printing the name of the driver and the count.

Running this, I get the following result:

{% highlight text %}
nouveau: 1
nouveau:: 1
iwlwifi: 2
{% endhighlight %}

That's annoying. `nouveau` appears in the report twice because it's mentioned with and without a colon `:` character in the source text.

{% highlight text %}
[    1.687503] nouveau E[     DRM] failed to create 0x80000080, -22
[    1.687631] nouveau: probe of 0000:01:00.0 failed with error -22
{% endhighlight %}

Adding a call to `gsub` to perform a simple string replacement does the trick. `gsub` is a part of AWK's [string functions](https://www.gnu.org/software/gawk/manual/html_node/String-Functions.html).

{% highlight awk %}
/ failed / { 
	gsub(/\:/, "", $3)
	drivers[$3] = drivers[$3] + 1
}
{% endhighlight %}

With an output like this

{% highlight text %}
nouveau: 2
iwlwifi: 2
{% endhighlight %}

Much better.

Just as we have an 'END' section above, we are also given the ability to write code in a 'BEGIN' section that will kick off before any of our pattern rules are executed.

### Using boolean logic in conditions

AWK conditions aren't just regular expressions, they can incorporate boolean logic from the file also. You can test any variable like a normal boolean condition. In the following example, I don't want to count failures that come out of the `iwlwifi` driver.

{% highlight awk %}
/ failed / && $3 != "iwlwifi" { 
	gsub(/\:/, "", $3)
	drivers[$3] = drivers[$3] + 1
}
{% endhighlight %}

### Other functions to check out

If at any time, your rule wants to bug out of the script entirely - wire up the `exit` call. If you just want to stop processing <em>this</em> line of text and move on to the next, you can use `next`.

