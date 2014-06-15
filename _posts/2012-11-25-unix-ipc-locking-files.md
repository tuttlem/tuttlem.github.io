---
layout: post
title: Unix IPC: Locking Files
date: 2012-11-25
comments: false
categories: [ "Unix", "Linux", "POSIX", "IPC", "locking", "files" ]
---

Locking a file helps assure your program that no other processes can tamper with it or a region of it.

This snippet will show you how to lock and unlock a file.struct flock fl;
int fd;

{% highlight c %}
/* fill out the lock structure */   
fl.l_type   = F_WRLCK; 
fl.l_whence = SEEK_SET;
fl.l_start  = 0;       
fl.l_len    = 0;       
fl.l_pid    = getpid();
 
/* open the file */
fd = open("filename", O_WRONLY);
 
/* lock the file */
fcntl(fd, F_SETLKW, &fl);
 
/* --- complete any work here with the file locked --- */
 
/* unlock the file now */
fl.l_type   = F_UNLCK; 
fcntl(fd, F_SETLK, &fl);
{% endhighlight %}

### Further reading
[The fcntl system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?fcntl)