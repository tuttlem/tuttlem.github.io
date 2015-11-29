---
layout: post
title: Encoding information in prime numbers
date: 2015-11-29
comments: false
categories: [ "math", "theory", "prime" ]
---

An interesting part of encryption theory is the ability to encode a message using prime numbers. It's not the most efficient way to represent a message, but it does exhibit some interesting properties.

### Hello

Take the message "HELLO" for instance. Here it is along with the [ASCII](https://en.wikipedia.org/wiki/ASCII) values for each character.

{% highlight text %}
H  E  L  L  O
72 69 76 76 79 
{% endhighlight %}

If we assign each character of our message a prime (as they ascend in sequence):

{% highlight text %}
2  3  5  7  11
H  E  L  L  O
72 69 76 76 79 
{% endhighlight %}

We can encode this message using these prime numbers like so:

{% highlight text %}
(2^72) * (3^69) * (5^76) * (7^76) * (11^79) =

1639531486723067852359816964623169016543137549
4122401687192804219102815235735638642399170444
5066082282398711507312101674742952521828622795
1778467808618104090241918575825850806280956250
0000000000000000000000000000000000000000000000
0000000000000000000000000 
{% endhighlight %}

That massive number is our encoded message. 

### Adjusting the message

You can add a letter to the message, just by multiplying in another value:

{% highlight text %}
 H        E        L        L        O         O
(2^72) * (3^69) * (5^76) * (7^76) * (11^79) * (13^79) 
{% endhighlight %}

Commutatively, we can remove a character from our message just by dividing the encoded message. To remove the `E` from our message, we'd divide the encoded message by `3^69`.

### The guessing game

As there's no encryption involved with this process, it's purely encoding; all someone needs to do is factor out your message. From there they can gain the ASCII codes and positions to be able to read your message.

