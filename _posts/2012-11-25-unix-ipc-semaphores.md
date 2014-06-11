---
layout: post
title: Unix IPC: Semaphores
date: 2012-11-25
comments: false
---

Semaphores can simplify access to shared resources. The following snippet will show you how to create a semaphore set and destroy it.

{% highlight c %}
key_t key;
int semid;

/* get an IPC key */
key = ftok("filename", 'E');
/* create the new semaphore set */
semid = semget(key, 10, 0666 | IPC_CREAT);

/* destroy the semaphore */
semctl(semid, 0, IPC_RMID);
{% endhighlight %}

### Further reading
* [The semget system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?semget)
* [The semctl system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?semctl)

