---
layout: post
title: Unix IPC&#58; Shared Memory
date: 2012-11-25
comments: false
categories: [ "Unix", "Linux", "POSIX", "IPC", "shared memory" ]
---

Shared memory allows multiple processes to view, modify and control shared segments of memory. This snippet will show you how to obtain a pointer to some shared memory and then release the pointer.

{% highlight c %}
key_t key;
int shmid;

/* get an ipc key */
key = ftok("filename", 'R');

/* connect to the segment */
shmid = shmget(key, SHM_SIZE, 0644 | IPC_CREAT);

/* attach to the segment */
data = shmat(shmid, (void *)0, 0);

/* perform writes and reads on the "data" pointer */

/* detach from the segment */
shmdt(data);
{% endhighlight %}

### Further reading
* [The shmget system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?shmget)
* [The shmat system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?shmat)
* [The shmdt system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?shmdt)
