---
layout: post
title: cabal sandbox ghci
date: 2014-03-16
comments: false
---

A very quick post that is based on the information from [this stack overflow article](http://stackoverflow.com/questions/17014270/how-can-i-use-ghci-with-the-new-cabal-1-17-sandboxes) on how to get a GHCi session up and running with the libraries referenced that you've installed using your cabal sandbox.

{% highlight bash %}
cd $YOUR_PACKAGE_DIR
 
# For GHC >= 7.6
ghci -no-user-package-db -package-db .cabal-sandbox/i386-linux-ghc-7.6.1-packages.conf.d
 
# For GHC < 7.6
ghci -no-user-package-conf -package-conf .cabal-sandbox/i386-linux-ghc-7.4.2-packages.conf.d
{% endhighlight %}