---
layout: post
title: bash_profile vs bashrc
date: 2012-12-20
comments: false
---

### Introduction

Whenever I need to make shell configuration changes in unix land (weather that be on a linux, bsd or osx box), I always find myself scratching around trying to find the "correct place" to put the changes that I want to make. This post is all about the black and white between `.bash_profile` and `.bashrc.`

### The documented difference

If you were to read the [bash man page](http://linux.die.net/man/1/bash) you'll get two pretty clear sentences in there.

> When bash is invoked as an interactive login shell, or as a non-interactive shell with the --login option, it first reads and executes commands from the file /etc/profile, if that file exists. After reading that file, it looks for ~/.bash_profile, ~/.bash_login, and ~/.profile, in that order, and reads and executes commands from the first one that exists and is readable.
> When an interactive shell that is not a login shell is started, bash reads and executes commands from ~/.bashrc, if that file exists.

From this, you can see the deciding factor is:

* If it's a login shell, use `.bash_profile`
* If it's <strong>not</strong> a login shell, use `.bashrc`

### What's a login shell?

A login shell is when you're logging into the machine at the console or over SSH i.e. you haven't yet logged in. A non-login shell is, for example, when you've already logged into your window manager and you fire up your terminal (that is of course, if your terminal is configured to run as a non-login shell!). >Caveats and configurations aside, this is the coarse difference between terms.

### Ok, which one do I bloody use then?

Well, to save yourself some administration headaches why not just use one? You then `source` that configuration file from the other. I know that doesn't read very well, so here's the example. I hope this makes it clearer.

So, fill `.bashrc` with all of your configuration goodies and put the following into `.bash_profile`

{% highlight bash %}
if [ -f ~/.bashrc ]; then
   source ~/.bashrc
fi
{% endhighlight %}

So now it doesn't matter if you use a login or non-login shell, you get the same, consistent configuration experience!