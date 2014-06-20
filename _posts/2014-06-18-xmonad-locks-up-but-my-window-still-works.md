---
layout: post
title: XMonad locks up by my window still works
date: 2014-06-18
comments: false
categories: ["XMonad", "X11", "Haskell", "Tip"]
---

I've just recently installed [XMonad](http://xmonad.org/) on my laptop and am just ironing out some issues that have popped up from time to time. Today, I'd noticed that XMonad had stopped responding to my `mod` key requests, however the focused application (in my case firefox) was still responding.

After doing some searching around the web, I've come across [this article](http://www.haskell.org/haskellwiki/Xmonad/Frequently_asked_questions#XMonad_is_frozen.21) which has set me straight. It seems that a pipe that XMonad writes to is full - it just needs to be cleared.

First off, you'll need to get yourelf to a terminal. If you're unable to do that (like I was), I went to another virtual terminal all together by using `alt`+`ctrl`+`f1`.

You need to find XMonad's currently running pid.

{% highlight bash %}
$ ps -ef | grep xmonad
michael  1406  1395  0 10:48 ?      00:00:09 /home/michael/.xmonad/xmonad-x86_64-linux
{% endhighlight %}

In my case, it's `1406`. You need to take a look at this pid's file descriptors:

{% highlight bash %}
$ ls -l /proc/1406/fd
total 0
lr-x------ 1 michael michael 64 Jun 18 14:10 0 -> /dev/null
lrwx------ 1 michael michael 64 Jun 18 14:10 1 -> socket:[17946]
lrwx------ 1 michael michael 64 Jun 18 14:10 2 -> socket:[17946]
l-wx------ 1 michael michael 64 Jun 18 14:10 3 -> /var/log/slim.log
lrwx------ 1 michael michael 64 Jun 18 14:10 4 -> socket:[18899]
l-wx------ 1 michael michael 64 Jun 18 14:10 5 -> pipe:[18898]
{% endhighlight %}

So, in my case here `#5` which is the pipe - needs to be cleared. You can do so really quickly just by `cat`ting it to screen.

{% highlight bash %}
$ cat /proc/1406/fd
{% endhighlight %}

XMonad should now be back in the land of the living.

Finally, this should never be a problem just as long as your `xmonad.hs` is configured with a `logHook` that will pipe the contents of this stream out.

{% highlight haskell %}
xmproc <- spawnPipe "xmobar"

. . .
. . .

logHook = dynamicLogWithPP $ xmobarPP
         { ppOutput = hPutStrLn xmproc
         , ppTitle = xmobarColor "green" "" . shorten 50
         }
{% endhighlight %}



