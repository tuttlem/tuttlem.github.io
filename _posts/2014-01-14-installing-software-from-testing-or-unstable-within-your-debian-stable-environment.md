---
layout: post
title: Installing Software from Testing or Unstable within your Debian Stable Environment
date: 2014-01-14
comments: false
---

### Introduction

The great thing about using the current stable version of [Debian](www.debian.org) is that you're assured that a lot of testing has gone in to ensure that all of the packages you're looking at are in fact stable - sometimes this works against us as it takes so long for packages to become stable, making the Debian stable repository quite stale with its versions.

In today's post, I'll show you how you can install a package from a different repository (other than stable) within your stable Debian environment.

At the time of this writing, I'm currently using "Wheezy" (codename for Stable). This makes "Jessie" the codename for Testing and "Sid" the codename for Unstable.

### Adding Software Sources

In order to install software from another repository, you need to tell "apt" where to get the software from. Before making any changes, my /etc/apt/sources.list looks like this:

{% highlight text %}
deb http://ftp.au.debian.org/debian/ wheezy main
deb-src http://ftp.au.debian.org/debian/ wheezy main

deb http://security.debian.org/ wheezy/updates main
deb-src http://security.debian.org/ wheezy/updates main

deb http://ftp.au.debian.org/debian/ wheezy-updates main
deb-src http://ftp.au.debian.org/debian/ wheezy-updates main
{% endhighlight %}

The man page for "sources.list" will fill you in on the structure of these lines in your sources.list. For the purposes of this post, just take note that each line mentions "wheezy" at the end.

Without modification, if we were to use "apt-cache policy" we can find out what versions of a particular package are available to us. For the purposes of this post, I'll use "haskell-platform". Taking a look at the cache policy for this package:

{% highlight text %}
$ apt-cache policy haskell-platform
haskell-platform:
  Installed: (none)
  Candidate: 2012.2.0.0
  Version table:
     2012.2.0.0 0
        500 http://ftp.au.debian.org/debian/ wheezy/main amd64 Packages
{% endhighlight %}

We've got version "2012.2.0.0" available to us in the stable repository. With "2013.2.0.0" as the current version, we can see that stable is a little behind. Let's try and fix that.

We're going to add some software from the testing repository, so we're going to link up with the binary source pointed to "jessie". To do this, we'll add one extra line to /etc/apt/sources.list, like so:

{% highlight text %}
deb http://ftp.au.debian.org/debian/ wheezy main
deb-src http://ftp.au.debian.org/debian/ wheezy main
deb http://ftp.au.debian.org/debian/ jessie main

deb http://security.debian.org/ wheezy/updates main
deb-src http://security.debian.org/ wheezy/updates main

deb http://ftp.au.debian.org/debian/ wheezy-updates main
deb-src http://ftp.au.debian.org/debian/ wheezy-updates main
{% endhighlight %}

Note the third line (new) that mentions "jessie".

### Setting Priorities

Now that we've confused apt, by mixing software sources - we need to set some priorities where the stable repository will take precedence over the testing repository.

To do this, we open/create the file `/etc/apt/preferences`. In this file, we can list out all of the repositories that we'd like to use and assign a priority to them. Here's the sample putting a higher priority on stable:

{% highlight text %}
Package: *
Pin: release a=stable
Pin-Priority: 700

Package: *
Pin: release a=testing
Pin-Priority: 600
{% endhighlight %}

The instructions here are defining what packages these rules apply to, which release they apply to and what priority is to be applied. Now that we've put these priorities in place, we'll update our local software cache:

{% highlight bash %}
$ sudo apt-get update
{% endhighlight %}

Then, we can take a look at the policy:

{% highlight text %}
haskell-platform:
  Installed: (none)
  Candidate: 2012.2.0.0
  Version table:
     2013.2.0.0.debian3 0
        600 http://ftp.au.debian.org/debian/ jessie/main amd64 Packages
     2012.2.0.0 0
        700 http://ftp.au.debian.org/debian/ wheezy/main amd64 Packages
{% endhighlight %}


We now have the ability to install the later package!

### Installing

With all of these rules in place now, installing software from a particular repository is as simple as:

{% highlight bash %}
$ sudo apt-get -t testing install haskell-platform
{% endhighlight %}

We've passed the repository the `-t` option. This will take haskell-platform and associated dependencies from the testing repository.
