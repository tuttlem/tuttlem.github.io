---
layout: post
title: Unix Networking: Client
date: 2012-11-26
comments: false
---

The following snippet outlines the skeleton of a client socket application. It's the bare bones of what is needed to establish the client connection.

{% highlight c %}
int sockfd, numbytes;
char buf[MAXDATASIZE];
struct addrinfo hints, *servinfo, *p;
int rv;
char s[INET6_ADDRSTRLEN];

/* fill out the address info */
memset(&hints, 0, sizeof hints);
hints.ai_family = AF_UNSPEC;
hints.ai_socktype = SOCK_STREAM;

/* create the actual server address */
getaddrinfo("www.remoteplace.com", PORT, &hints, &servinfo);

/* create the socket file descriptor */
sockfd = socket(p->ai_family, p->ai_socktype, p->ai_protocol);

/* perform the network connect */
connect(sockfd, p->ai_addr, p->ai_addrlen);

/* free up the addres structure */
freeaddrinfo(servinfo); 

/* receive some data on the socket */
recv(sockfd, buf, MAXDATASIZE-1, 0));

/* close the socket off */
close(sockfd);
{% endhighlight %}

###Further reading
[The getaddrinfo system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?getaddrinfo+3)
[The socket system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?socket)
[The connect system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?connect)
[The recv system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?recv)
[The send system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?send)
