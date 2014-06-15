---
layout: post
title: Shebang, Ruby and RVM
date: 2013-01-09
comments: false
categories: [ "Ruby", "rvm", "shebang" ]
---

I needed to distribute one of my ruby programs to be setup as a job, so I needed to add a [shebang](http://en.wikipedia.org/wiki/Shebang_(Unix)) to the top of the file. I run my development environment using RVM so I had no idea how to address ruby in the shebang. Then it hit me ..

{% highlight bash %}
#!/usr/bin/env ruby
{% endhighlight %}

Interestingly, things are a little different when trying to execute a ruby with one of your RVM installed rubies from a cron job. The rvm website has a whole [section](https://rvm.io/integration/cron/) regarding the topic of cron integration. The command that I passed to successfully execute these jobs needed to be addressed absolutely:

{% highlight text %}
bash -c "/home/user/.rvm/bin/ruby /path/to/script.rb"
{% endhighlight %}

Easy.