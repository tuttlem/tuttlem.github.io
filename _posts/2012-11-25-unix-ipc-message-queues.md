---
layout: post
title: Unix IPC&#58; Message Queues
date: 2012-11-25
comments: false
categories: [ "Unix", "Linux", "POSIX", "IPC", "message queue" ]
---

Message queues are pretty common structures for inter-process communication. A common queue is created by one of the processes, from there it can be connected to by any other process and have messages submitted to it.

This snippet shows the creation of a queue and sending a message. Another block below this will show receiving a message from the queue.

{% highlight c %}
char buf[20];
int msqid;
key_t key;

/* --- message sender --- */

/* make a key */
key = ftok("first", 'B');
/* create the message queue */
msqid = msgget(key, 0644 | IPC_CREAT));

/* put a message onto the queue */
msgsnd(msqid, &buf, 20, 0);

/* destroy (remove) the message queue */
msgctl(msqid, IPC_RMID, NULL);


/* --- message receiver --- */

/* make a key */
key = ftok("second", 'B');
/* connect to the message queue */
msqid = msgget(key, 0644);
/* receive the message off the queue */
msgrcv(msqid, &buf, 20, 0, 0);
{% endhighlight %}

### Further reading
* [The ftok system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?ftok)
* [The msgget system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?msgget)
* [The msgsnd system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?msgsnd)
* [The msgctl system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?msgctl)
* [The msgrcv system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?msgrcv)
