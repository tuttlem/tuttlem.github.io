---
layout: post
title: Unix IPC: Working with Signals
date: 2012-11-25
comments: false
categories: [ "Unix", "Linux", "POSIX", "IPC", "signals" ]
---

Incoming messages to your process can come in the form of a signal. Signals are standard message packets that the operating system will use to tell your process something about the environment.

This snippet will show you show to setup a signal handler in your program.

{% highlight c %}
/** The signal handler */
void sigint_handler(int sig) {
	/* do something interesting with the sigint here */
}

int main(void) {
	struct sigaction sa;
	
	/* fill out the signal structure */
	sa.sa_handler = sigint_handler;
	sa.sa_flags   = 0;
	sigemptyset(&sa.sa_mask);

	/* assign it to the appropriate signal */
	sigaction(SIGINT, &sa, NULL);

	/* do other stuff here, sigint will be handled */
}
{% endhighlight %}

### Further reading
* [The sigaction system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?sigaction+2)
* [The signal system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?signal+2)
* [The raise system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?raise)
* [The kill system call](http://unixhelp.ed.ac.uk/CGI/man-cgi?kill+2)
