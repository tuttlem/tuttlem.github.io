---
layout: post
title: Simple logging in C
date: 2012-12-08
comments: false
---

Logging is probably one of the most important services you can offer your application. Your programs have information that needs to be expressed to the user, in fact in parts it's important for your program to be almost paranoid that it hasn't said anything.

All things should be controllable though. Putting your application into production with a paranoid level of logging doesn't make for very happy sys-admins, so this is left as an exercise to the reader. Here, I want to present you a configurable but most importantly, a usable logging framework that you can drop into your C projects without much effort at all. Let's go through the header!

{% highlight c %}
#ifndef __libced_log_h_

#define __libced_log_h_

#include <unistd.h>
#include <time.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>

/** Logs a line of text */
void _ced_log(FILE *file, const char *fmt, ...);

#define ced_logf(f, ...) _ced_log(f   , __VA_ARGS__)
#define ced_log(...)     _ced_log(NULL, __VA_ARGS__)

#endif /* __libced_log_h_ */
{% endhighlight %}

This is very simple stuff. Only one function `_ced_log` is defined that's used by this header. The user is encouraged to access the logging functionality by proxy of the macros defined (`ced_log` and `ced_logf`). `ced_log` takes in the same parameter structure as a printf call allowing the very natural format strings to be used for your logging. `ced_logf` takes the structure of fprintf where you can direct your log information to a specific file handle. Again, with some small modifications you can direct all of your logs into a file by default. I just like to throw all mine out to the console. I hate surprises (in development)!! The implementation for this library piece is also very simple with only one function needing to be filled out:

{% highlight c %}
#include "../include/log.h"

/** */
void _ced_log(FILE *file, const char *fmt, ...) {
  va_list ap;
  time_t t;
  char datestr[51];

  /* determine if we just go to std error */
  file = (file == NULL) ? stderr : file;

  /* datetime & pid formatting */
  t = time(NULL);
  tzset();
  strftime(datestr, sizeof(datestr) - 1, "%a %b %d %T %Z %Y", localtime(&t));
  fprintf(file, "%s [%d]: ", datestr, getpid());

  /* draw out the vararg format */
  va_start(ap, fmt);
  vfprintf(file, fmt, ap);
  va_end(ap);

  /* bump to the next line */
  fprintf(file, "\n");
}
{% endhighlight %}

Here is where you can really make this module your own. The default format that I have going in here has the current process ID and date/time stamp. I find these very useful during the fault-finding process once a unit of software is running. The most important thing to draw from this is you can change the default makeup of a log line by changing this code. I use this code just about everywhere. I use it so much that I have included it in my library that it available from github [here](https://github.com/tuttlem/libced).
