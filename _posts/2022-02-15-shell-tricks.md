---
layout: post
title: Shell Tricks
date: 2022-02-15
comments: false
categories: [ "shell", "bash" ]
---

Sometimes you can be just as productive using your shell as you are in any
programming environment, you just need to know a couple of tricks. In this
article, I'll walk through some basic tips that I've come across.

### Reading input

You can make your scripts immediately interactive by using the `read` 
instruction. 

{% highlight shell %}
#/bin/bash

echo -n "What is your name? "
read NAME

echo "Hi there ${NAME}!"
{% endhighlight %}

### String length

You can get the length of any string that you've stored in a variable by 
prefixing it with `#`. 

{% highlight shell %}
#/bin/bash

echo -n "What is your name? "
read NAME

echo "Your name has ${#NAME} characters in it"
{% endhighlight %}

### Quick arithmetic

You can perform some basic arithmetic within your scripts as well. The value
emitted with the `#` character is an integral value that we can perform tests
against.

{% highlight shell %}
#/bin/bash

echo -n "What is your name? "
read NAME

if (( ${#NAME} > 10 )) 
then
  echo "You have a very long name, ${NAME}"
fi
{% endhighlight %}

### Substrings

String enumeration will also allow you to take a substring directly. The 
format takes the form of `${VAR:offset:length}`.

Passing positive integers for `offset` and `length` will make substring 
operate from the leftmost side of the string. Negative numbers provide a 
reverse index, from the right.

{% highlight shell %}
STR="Scripting for the win"

echo ${STR:10:3}
# for

echo ${STR: -3}
# win

echo ${STR: -7: 3}
# the
{% endhighlight %}

### Replacement

It's common place to be able to use regular expressions to make substitutions
where needed, and they're available to you at the shell as well.

{% highlight shell %}
#!/bin/bash

STR="Scripting for the win"

echo ${STR/win/WIN}
# Scripting for the WIN
{% endhighlight %}

### Finishing up

There's lots more that you can do just from the shell, without needing to 
reach for other tools. This is only a few tips and tricks.

