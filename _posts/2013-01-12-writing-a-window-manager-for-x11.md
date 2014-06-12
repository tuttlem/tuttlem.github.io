---
layout: post
title: Writing a Window Manager for X11
date: 2013-01-12
comments: false
---

As a bit of a bookmark to myself, I just wanted to post about writing a window manager for X windows. A lot of the material that I've seen around the place in articles and posts themselves have all pointed me towards downloading the out-of-print [O'Reilly Open Book](http://oreilly.com/openbook/) site.

Some of the books of interest to a window manager programmer are

* [Xlib Reference Manual](http://www.archive.org/details/xlibretmanver1102nyemiss)
* [Xlib Reference Manual Volume 2](http://www.archive.org/details/xlibrefmanv115ed02nyemiss)
* [X Toolkit Intrinsics Programming Manual](http://www.archive.org/details/xtoolkitintrinsi04nyemiss)
* [X Toolkit Intrinsics Reference Manual](http://www.archive.org/details/xtoolkitintrirefman05oreimiss)

I'm sure that there is plenty of other reference material around, but a lot of people were suggesting to put a copy of these in your claw - so I have. Digging around a little bit more, I came across [TinyWM](http://incise.org/tinywm.html). TinyWM looks like a suitable candidate to be the bootstrap to a window manager project. It's functional, it doesn't do much but it is around 50 lines of C code. I'll be using this as my guide for when that rainy day comes and I start work on my own WM.