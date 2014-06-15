---
layout: post
title: Unix IPC: Forking and Waiting
date: 2012-11-25
comments: false
categories: [ "Unix", "Linux", "POSIX", "IPC", "fork", "wait" ]
---

In order to create processes within the Unix environment, you must fork. Forking a process establishes the parent / child relationship which is where the waiting comes into it. All good parents wait for their children to die before terminating.

It's just good manners, you know?

So, the snippet for this will be a fork and wait set:

{% highlight c %}
/* fork execution here */
pid_t pid = fork();
 
int child_exit_code;
 
if (pid < 0) {
 /* -1 indicates that fork failed */
 exit(1);
} else if (pid == 0) {
 /* 0 indicates that this is the child process */
 exit(0);
} else {
 /* the pid being returned indicates it's the parent */
 /* wait for the child to finish and
    capture its exit code */
 wait(&child_exit_code);
}
{% endhighlight %}

### Further reading
* [The fork system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?fork+2)
* [The wait system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?wait+2)
