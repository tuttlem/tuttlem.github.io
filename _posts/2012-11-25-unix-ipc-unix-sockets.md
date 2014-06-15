---
layout: post
title: Unix IPC: Unix Sockets
date: 2012-11-25
comments: false
categories: [ "Unix", "Linux", "POSIX", "IPC", "sockets" ]
---

Unix sockets talk locally on the machine. Their operation is very similar to standard socket operations (as they are the same in every way). This snippet will show you a server and client.

{% highlight c %}
int s, s2, t, len;
struct sockaddr_un local, remote;
char str[100];

/* --- as a server --- */

/* create the socket to serve on */
s = socket(AF_UNIX, SOCK_STREAM, 0);
/* fill out the address struct to listen on */
local.sun_family = AF_UNIX;
strcpy(local.sun_path, SOCK_PATH);
unlink(local.sun_path);
len = strlen(local.sun_path) + sizeof(local.sun_family);

/* bind the socket to this name */
bind(s, (struct sockaddr *)&local, len);
/* listen on this socket */
listen(s, 5);
/* accept any incoming connection */
s2 = accept(s, (struct sockaddr *)&remote, &t));
/* receive some data */
n = recv(s2, str, 100, 0);
/* close the client */
close(s2);


/* --- as a client --- */

/* create a socket to connect on */
s = socket(AF_UNIX, SOCK_STREAM, 0);

/* fill out the address struct to connect to */
remote.sun_family = AF_UNIX;
strcpy(remote.sun_path, SOCK_PATH);
len = strlen(remote.sun_path) + sizeof(remote.sun_family);

/* connect on this socket */
connect(s, (struct sockaddr *)&remote, len);

/* send some data */
send(s, str, strlen(str), 0);
/* close the socket */
close(s);
{% endhighlight %}

### Further reading
* [The socket system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?socket)
* [The bind system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?bind+2)
* [The listen system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?listen+2)
* [The accept system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?accept+2)
* [The connect system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?connect+2)
* [The send system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?send+2)
* [The recv system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?recv+2)