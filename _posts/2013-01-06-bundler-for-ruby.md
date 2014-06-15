---
layout: post
title: Bundler for Ruby
date: 2013-01-06
comments: false
categories: [ "Ruby", "bundler", "installation", "tutorial" ]
---

### Introduction

A really easy way to take the headache out of keeping your gems at the right version (especially when source controlling your projects) is to use a package manager. Today's post is about [Bundler](http://gembundler.com/) which helps maintain your environment for Ruby development.

### Let's go!

Install bundler.

{% highlight bash %}
$ gem install bundler
{% endhighlight %}

You're now installed and ready to start bundling. The first job that you need to do is to write a metafile containing all of the gems that your application requires and where bundler should fetch those gems from. This metafile is called your `Gemfile`. A `Gemfile` will take the following format.

{% highlight ruby %}
source "http://rubygems.org"
gem "nokogiri"              
gem "premailer"             
gem "tlsmail"
{% endhighlight %}

The `source` tells bundler what site to download the gems from. The most common ones that I've seen are as follows. 

{% highlight ruby %}
source :rubygems
source "http://rubygems.org"
source :rubyforge
source "http://gems.rubyforge.org"
source :gemcutter
source "http://gemcutter.org"
{% endhighlight %}

You can see each of the sources here with a symbolic shortcut that you can use also. The `gem` tells bundler that you have a dependency. You can also constrain the version of your dependencies on these lines by using version information after the gem. Now that you've created your `Gemfile`, you can install all of the gems and their dependencies simple by changing directories to where your `Gemfile` resides and typing the following at the prompt.

{% highlight bash %}
$ bundle install
{% endhighlight %}

Now you have all of your dependencies installed for your application. Doing this will generate a `Gemfile.lock` file in your directory also. Make sure that you source control both your `Gemfile` and `Gemfile.lock`.

That's it.