---
layout: post
title: Hello, Haskell (from the web)!
date: 2012-12-25
comments: false
categories: [ "Hello world", "Haskell", "Web" ]
---

### Introduction

The best part about technology is the ability to use anything you want. I've run across many frameworks all touting their own awesomeness. Sometimes though, you just need to smash a bit of output back to the client. Using the CGI and XHTML libraries available for Haskell, web programming is so simple. Just about every web server known to mankind can run a CGI program and if yours can't, it may need a trip to the gym.

### Show me some code

Excellent. This is where it gets very interesting. I realise at first, when you're reading this code you're going to be saying "well, duh - I can knock out a HTML page to be statically served much easier than this B.S." If this is your train of thought, you may have missed the point. The example I provider here is only to demonstrate to the reader how we can get Haskell closer to the client generation.

{% highlight haskell %}
module Main where

import Network.CGI
import Text.XHtml

-- Generates the page content
page :: Html 
page = body << h1 << "Served fresh from Haskell!"

-- Renders our html document to string
cgiMain :: CGI CGIResult
cgiMain = output $ renderHtml page
 
-- Handles the CGI action and basic errors also
main :: IO ()
main = runCGI $ handleErrors cgiMain
{% endhighlight %}

This should be fairly easy to follow. There's nothing out of the ordinary and it's very simple. After reading [this document](http://www.haskell.org/haskellwiki/Practical_web_programming_in_Haskell) I was impressed that this could be condensed down into the following:

{% highlight haskell %}
import Network.CGI
import Text.XHtml

main = runCGI . output . renderHtml $ body << h1 << "Served fresh from Haskell!"
{% endhighlight %}

Damn! Not bad. And, here's the output that you get from running these CGI programs:

{% highlight html %}
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
  <body>
    <h1>Served fresh from Haskell!</h1>  
  </body>
</html>                
{% endhighlight %}

At the moment, I can see how this looks like a long way around and it certainly is a feathers over steam-train approach to delivering a static page but we've now got the power of Haskell behind our CGI program.