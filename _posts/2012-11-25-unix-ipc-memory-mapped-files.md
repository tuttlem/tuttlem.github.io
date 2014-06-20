---
layout: post
title: Unix IPC&#58; Memory Mapped Files
date: 2012-11-25
comments: false
categories: [ "Unix", "Linux", "POSIX", "IPC", "memory mapped" ]
---

This snippet will show you how to open a file and map it into memory.

{% highlight c %}
int fd, pagesize;
char *data;

/* open the file */
fd = open("somefile", O_RDONLY);

/* get the current page size */
pagesize = getpagesize();

/* map the 2nd page into memory */
data = mmap((caddr_t)0, pagesize, PROT_READ, MAP_SHARED, fd, pagesize);

/* start using the data pointer */
{% endhighlight %}

### Further reading
[The mmap system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?mmap)
