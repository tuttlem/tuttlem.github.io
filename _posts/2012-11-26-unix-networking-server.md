---
layout: post
title: Unix Networking: Server
date: 2012-11-26
comments: false
---

The following snippet is a bare-bones server.

{% highlight c %}
int sockfd, new_fd; 
struct addrinfo hints, *servinfo, *p;
struct sockaddr_storage their_addr;
socklen_t sin_size;
struct sigaction sa;
int yes=1;
char s[INET6_ADDRSTRLEN];
int rv;
 
/* fill out the address structure */
memset(&hints, 0, sizeof hints);
hints.ai_family = AF_UNSPEC;
hints.ai_socktype = SOCK_STREAM;
hints.ai_flags = AI_PASSIVE;
 
/* get the address information */
getaddrinfo(NULL, PORT, &hints, &servinfo);
 
/* open the socket */
sockfd = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
 
/* allow the socket to be re-used */
setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int));
 
/* bind the socket to the address */
bind(sockfd, p->ai_addr, p->ai_addrlen);
 
/* free up the address structure */
freeaddrinfo(servinfo);
 
/* start the socket listening */
listen(sockfd, BACKLOG);
 
/* accept the first connect */
new_fd = accept(sockfd, (struct sockaddr *)&their_addr, &sin_size);
 
/* send the client some data */
send(new_fd, "Hello, world!", 13, 0);
  
/* finished with the client & server */
close(new_fd);
close(sockfd);
{% endhighlight %}

Further reading
[The getaddrinfo system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?getaddrinfo+3)
[The socket system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?socket)
[The bind system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?bind+2)
[The listen system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?listen+2)
[The accept system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?accept+2)
[The recv system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?recv)
[The send system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?send)
