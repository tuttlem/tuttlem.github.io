---
layout: post
title: Unix IPC: Pipes and FIFOs
date: 2012-11-25
comments: false
categories: [ "Unix", "Linux", "POSIX", "IPC", "pipes", "FIFO" ]
---

This snippet will show you two processes communicating between each other using a pipe.

{% highlight c %}
int pfds[2];
char buf[30];

/* open the pipe */
pipe(pfds);

if (!fork()) {
	/* the child will write to the pipe */
	write(pfds[1], "test", 5);
	exit(0);
} else {
	/* the parent will read from the pipe */
	read(pfds[0], buf, 5);
	wait(NULL);
}
{% endhighlight %}

FIFOs are just pipes that have a specific name. In this snippet the name is a constant defined elsewhere called FIFO_NAME.

{% highlight c %}
/* first program writes to the named pipe */
char buffer[20];
int num, fd;

/* create the named pipe */
mknod(FIFO_NAME, S_IFIFO | 0666, 0);
/* open for writing */
fd = open(FIFO_NAME, O_WRONLY);
/* write to the pipe */
write(fd, buffer, strlen(buffer));


/* second program reads from the named pipe */
char buffer[20];
int num, fd;
/* create the named pipe */
mknod(FIFO_NAME, S_IFIFO | 0666, 0);
/* open for reading */
fd = open(FIFO_NAME, O_RDONLY);
/* read from the pipe */
read(fd, s, 20);
{% endhighlight %}

### Further reading
[The mknod system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?mknod)
