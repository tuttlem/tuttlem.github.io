---
layout: post
title: Conditionally turning on spell checking in vim
date: 2014-06-18
comments: false
categories: ["vim", "spell"]
---

Turning on spell check in vim is easy, you just enter the following command at the prompt:

{% highlight text %}
:set spell spelllang=en_au
{% endhighlight %}

<em>Of course</em> you substitute in the language code that suits you. So, when you're tired of all of the highlighting, you can easily turn it off like so:

{% highlight text %}
:set nospell
{% endhighlight %}

I use vim for both general text editing (say, like the Markdown document for instance) as well as for code editing. I really don't want spell checking on for code editing, so I've added the following block to my `.vimrc` to turn it on for Markdown documents only:

{% highlight text %}
autocmd FileType mkd set spell spelllang=en_au
{% endhighlight %}

Finally, if you've got a word that you've spelt incorrectly and you're stuck on fixing it - put your cursor over the word and hit `z=`. You'll get a list of suggestions to help you!


