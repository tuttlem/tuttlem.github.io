---
layout: post
title: Console control with readline
date: 2015-08-19
comments: false
categories: [ "console", "readline", "linux" ]
---

The [GNU Readline Library](https://cnswww.cns.cwru.edu/php/chet/readline/rltop.html) is a really useful library for creating single-line input style programs at the console. From the introduction:

> The GNU Readline library provides a set of functions for use by applications that allow users to edit command lines as they are typed in.

There's some really nice stuff added on top though. Being able to use emacs or vi editing modes are just some of them. This allows you to provide your users with consistent key bindings to what they use in their text editor.

### Modules

Taking a look on your file system, you can find readline's header files under `/usr/include/readline/`. 

{% highlight text %}
chardefs.h  keymaps.h   rlconf.h  rltypedefs.h
history.h   readline.h  rlstdc.h  tilde.h
{% endhighlight %}

Taking a really brief tour of these headers, `history.h` provides the library with the ability to recount previously entered text blocks. This sort of behavior can be observed when using bash; pressing the up and down buttons allow you to scroll through previous commands. `keymaps.h` provides the library with the mapping abstraction to make your readline-based application behave like vi or emacs or any custom implemented map you wish.

### Reading lines

The library's purpose is to read lines of text. The function that you'll use to do this is called `readline`. A simple example of this would be:

{% highlight c %}
#include <stdlib.h>
#include <stdio.h>
#include <readline/readline.h>

int main() {
  char *name;

  name = readline("What is your name? ");

  printf("Hello %s!\n", name);
  free(name);

  return 0;
}
{% endhighlight %}

<b>Note!</b> The string that you're given back from `readline` is allocated using `malloc`, so it's your responsibility to ensure that the memory is freed using `free`.

Resulting with the following execution:

{% highlight text %}
$ ./hello
What is your name? Joe
Hello Joe!
{% endhighlight %}

### Historical and auto complete

You can quickly give your readline program a historical account of previously entered lines with the following sort of structure:

{% highlight c %}
while (running) {
  filename = readline("Enter a filename: ");

  // add the data to the input history
  add_history(filename);

  // perform some work on "filename"

  free(filename);
}
{% endhighlight %}

The `add_history` method is available from the `readline/history.h` header.

Allowing your line edit to include file path auto completion is pretty easy also. You can bind this behavior to any key that you'd like; we're all pretty used to the tab character doing this work for us though:

{% highlight c %}
rl_bind_key('\t', rl_complete);
{% endhighlight %}

