---
layout: post
title: Text search with grep
date: 2019-04-24
comments: false
categories: [ "linux", "gnu", "grep", "text", "search" ]
---

[GNU Grep](https://www.gnu.org/software/grep/) is an amazingly powerful tool that will allow you to search input files containing specified patterns. 

The following command performs a recursive search for a string, inside a directory of files:

{% highlight bash %}
grep -rnw '/path/to/search' -e 'pattern-to-find'
{% endhighlight %}

Breaking this command down:

* `-r` makes the execution recursive
* `-n` will give you the line number
* `-w` matches the whole word

You can use the `--exclude` switch to remove file patterns from the set of files included in your search. Removing `js` and `html` files from your search might look like this:

{% highlight bash %}
--exclude *.{js,html}
{% endhighlight %}

The opposite of this will be the `--include` switch.

For further details on this really useful tool, check out the [man page](http://linuxcommand.org/lc3_man_pages/grep1.html).
