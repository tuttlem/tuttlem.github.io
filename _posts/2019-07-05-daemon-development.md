---
layout: post
title: Daemon development
date: 2019-07-05
comments: false
categories: [ "daemon", "server", "c" ]
---

In today's article, I'll take you through some basic daemon development for [POSIX](https://en.wikipedia.org/wiki/POSIX) based systems. This article should give you enough information to get started on your own project to add in _the bits that your program does_.

### Getting started

The largest amout of effort into writing any daemon is validation. Validation of incoming data is one thing; but we're not even there yet. We're just talking about validating the current system state, environment, and any configuration values.

### fork

First job is to use the [fork](http://man7.org/linux/man-pages/man2/fork.2.html) system call, to duplicate the current process.

> fork() creates a new process by duplicating the calling process.  The new process is referred to as the child process.  The calling process is referred to as the parent process.

Directly after forking from the parent process, the parent is terminated; leaving the child to own the running of the daemon. This allows the daemon to now be independent of any other process running. Removing this dependency means that no one (or nothing) can kill your daemon by killing another (parent) process.

A secondary fork is then invoked; usually after a call to [setsid](http://man7.org/linux/man-pages/man2/setsid.2.html). This starts a new session with no terminal.

> setsid() creates a new session if the calling process is not a process group leader.  The calling process is the leader of the new session (i.e., its session ID is made the same as its process ID). The calling process also becomes the process group leader of a new process group in the session (i.e., its process group ID is made the same as its process ID).

The side-effect that we're looking for here.

> Initially, the new session has no controlling terminal.

The child is then exited, removing any reliance on the terminal. 

The grandchild is now the daemon.

{% highlight c %}
pid_t pid;

/* the initial fork */
pid = fork();

/* terminate the parent */
if (pid == -1) {
  die("Failed to fork (errno=%d)", errno);
} else if (pid != 0) {
  exit(EXIT_SUCCESS);
}

/* make the child to session-leader */
setsid();

/* SIGHUP now gets ignore, and we re-fork */
signal(SIGHUP, SIG_IGN);
pid = fork();

/* terminate the child */
if (pid == -1) {
  die("Failed to fork (errno=%d)", errno);
else if (pid != 0) {
  exit(EXIT_SUCCESS);
}

/* we are now the grandparent
   this is the daemon */
{% endhighlight %}

### Working directory

Now that we have a *safe* running process, it's time to change to a location that we know is safe. We know that the root directory exists. We'll use it for our example.

{% highlight c %}
if (chdir("/") == -1) {
  die("Unable to change working directory (errno=%d)", errno);
}
{% endhighlight %}

This puts us in a *sane* place, where we know where we are. Doesn't need to be the root directory; but at least we know where we are.

### File mask

Next up, we'll set the [umask](https://en.wikipedia.org/wiki/Umask) for the process to `0`. It it standard practice for a daemon to operate with a umask of `0`. At the operating system level, this forces new file objects created to have permission of `0666` (world-writable); and directories as `0777`.

> umask() sets the calling process's file mode creation mask (umask) to mask & 0777 (i.e., only the file permission bits of mask are used), and returns the previous value of the mask.

In short, umask of `0` means objects are created with privileges initially revoked.

{% highlight c %}
umask(0);
{% endhighlight %}

### Standard File Descriptors

Daemons now close and re-open the standard file handles. `STDIN`, `STDOUT`, and `STDERR` all get closed. It's assumed that `/dev/null` is provided on any POSIX system, and as a result it's used to re-open these handles.

{% highlight c %}
close(STDIN_FILENO);
close(STDOUT_FILENO);
close(STDERR_FILENO);

if (open("/dev/null", O_RDONLY) == -1) {
  die("Unable to re-open STDIN (errno=%d)", errno);
}

if (open("/dev/null", O_WRONLY) == -1) {
  die("Unalb to re-open STDOUT (errno=%d)", errno);
}

if (open("/dev/null", O_RDWR) == -1) {
  die("Unable to re-open STDERR (errno=%d)", errno);
}
{% endhighlight %}

### All together

Your daemonize process might now look something like this:

{% highlight c %}
#include <errno.h>
#include <signal.h>
#include <fcntl.h>
#include <unistd.h>

void daemonise() {
  /* 1. Initial fork */
  pid_t pid = fork();
  if (pid == -1) {
    die("Unable to fork (errno=%d)", errno);
  } else if (pid != 0) {
    _exit(0);
  }

  /* 2. Start a new session */
  if (setsid()==-1) {
    die("Unable to become a session leader (errno=%d)", errno);
  }

  /* 3. Fork again */
  signal(SIGHUP, SIG_IGN);
  pid = fork();

  if (pid == -1) {
    die("Unable to fork (errno=%d)",errno);
  } else if (pid != 0) {
    _exit(0);
  }

  /* 4. Set the working directory */
  if (chdir("/") == -1) {
    die("Unable to change the working directory (errno=%d)",errno);
  }

  /* 5. Set the umaask */
  umask(0);

  /* 6. Deal with the file descriptors */
  close(STDIN_FILENO);
  close(STDOUT_FILENO);
  close(STDERR_FILENO);

  if (open("/dev/null", O_RDONLY) == -1) {
    die("Unable to re-open stdin (errno=%d)", errno);
  }
  if (open("/dev/null", O_WRONLY) == -1) {
    die("Unable to re-open stdout (errno=%d)",errno);
  }
  if (open("/dev/null", O_RDWR) == -1) {
    die("Unable to re-open stderr (errno=%d)",errno);
  }
}
{% endhighlight %}


### Closing up

You're now daemonised, and ready to start actually writing your daemon process.

Some points to note might be installing some signal handlers?

{% highlight c %}
signal(SIGCHLD, SIG_IGN);
signal(SIGTSTP, SIG_IGN);
signal(SIGTTOU, SIG_IGN);
signal(SIGTTIN, SIG_IGN);
signal(SIGHUP, daemon_signalhandler);
signal(SIGTERM, daemon_signalhandler);
{% endhighlight %}

