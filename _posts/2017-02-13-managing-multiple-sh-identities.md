---
layout: post
title: Managing multiple SSH identities
date: 2017-02-13
comments: false
categories: [ "ssh" ]
---

Sometimes, it makes sense to have multiple SSH identites. This can certainly be the case if you're doing work with your own personal accounts, vs. doing work for your job. You're not going to want to use your work account for your personal stuff.

In today's post, I'm going to run through the few steps that you need to take in order to manage multiple SSH identities.

## Different identities

First up, we generate two different identities:

{% highlight bash %}
ssh-keygen -t rsa -C "user@work.com"
{% endhighlight %}

When asked, make sure you give the file a unique name:

{% highlight plain %}
Enter file in which to save the key (/home/michael/.ssh/id_rsa): ~/.ssh/id_rsa_work
{% endhighlight %}

Now, we create the identity for home.

{% highlight bash %}
ssh-keygen -t rsa -C "user@home.com"
{% endhighlight %}

Again, set the file name so they don't collide:

{% highlight plain %}
Enter file in which to save the key (/home/michael/.ssh/id_rsa): ~/.ssh/id_rsa_home
{% endhighlight %}

Now, we should have the following:

{% highlight plain %}
id_rsa_home
id_rsa_home.pub
id_rsa_work
id_rsa_work.pub
{% endhighlight %}

## Configuration

Now we create a configuration file that ties all of the identities up. Start editing `~/.ssh/config`:

{% highlight plain %}
# Home account
Host home-server.com
  HostName home-server.com
  PreferredAuthentications publickey
  IdentityFile ~/.ssh/id_rsa_home

# Company account
Host work-server.com
  HostName work-server.com
  PreferredAuthentications publickey
  IdentityFile ~/.ssh/id_rsa_work
{% endhighlight %}

Delete all of the cached keys:

{% highlight bash %}
ssh-add -D
{% endhighlight %}

If you see the error message `Could not open a connection to your authentication agent.` you'll need to run the following:

{% highlight bash %}
ssh-agent -s
{% endhighlight %}

## Add your keys

You can now list your keys with:

{% highlight bash %}
ssh-add -l
{% endhighlight %}

You can add your keys back in with the following:

{% highlight bash %}
ssh-add ~/.ssh/id_rsa_work
ssh-add ~/.ssh/id_rsa_home
{% endhighlight %}

That's it!