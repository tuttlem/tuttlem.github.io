---
layout: post
title: Custom Installs with Checkinstall
date: 2014-01-11
comments: false
---

### Introduction

Sometimes it's necessary to install a piece of software from source. This is normally an awkward process when you've had a package manager taking care of all your software needs when you're faced with the proposition of installing something that the package manager is unaware of. Another concern is that some software developers don't do the simple things well - some makefiles won't even offer you the ability to uninstall a piece of software leaving you to try to remove the files that have been peppered into your system directories.

In today's post, I'll walk through a sample usage of the application [CheckInstall](https://wiki.debian.org/CheckInstall) for Debian based Linux distributions.

### What is it?

From the [CheckInstall](https://wiki.debian.org/CheckInstall) page on the Debian Wiki:

> CheckInstall keeps track of all the files created or modified by your  installation script ("make" "make install" "make install_modules",  "setup", etc), builds a standard binary package and installs it in your  system giving you the ability to uninstall it with your distribution's  standard package management utilities. 

The thing I love about the Debian distribution is the stability of the packages in its repositories. Sometimes, it's also what I'm not to fond of as software vendors are bringing out new versions of their software and they don't make it into the stable repositories until they're deemed stable (which takes ages!) or they may never make it into the repositories.

### Using CheckInstall

I've used CheckInstall for quite a few packages in the past. Just recently, I've used it to manage the installation of SDL2 onto my system.

{% highlight bash %}
# extract your source package
$ tar -zxvf SDL2-2.0.1.tar.gz

# configure and build as usual
$ cd SDL2-2.0.1
$ ./configure
$ make

# use checkinstall on the installation step
$ sudo checkinstall make install
{% endhighlight %}

After this process had finished, a deb file was created for me which represented the files that had been installed on the system and the deb itself had been applied to the system.

The idea here is that it simplifies re-installation and removal by proxy of the generated deb package.