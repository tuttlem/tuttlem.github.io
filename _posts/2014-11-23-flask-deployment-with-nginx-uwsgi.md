---
layout: post
title: Flask deployment with nginx and uwsgi
date: 2014-11-23
comments: false
categories: ["python", "flask", "nginx", "uwsgi"]
---

Taking your applications from the development web server into a full application server environment is quite painless with [nginx](http://nginx.org/en/), [uwsgi](https://uwsgi-docs.readthedocs.org/en/latest/) and [virtualenv](http://virtualenv.readthedocs.org/en/latest/). This post will take you through the steps required to get an application deployed.

### Server Setup

First of all, you'll need to get your server in a state where it's capable of serving HTTP content as well as housing your applications. If you've already got a server that will do this, you can skip this.

{% highlight bash %}
$ sudo apt-get install nginx uwsgi uwsgi-plugin-python python-dev python-setuptools build-essential
$ sudo easy_install pip
$ sudo pip install virtualenv
{% endhighlight %}

This will put all of the software required onto the server to house these applications. 

### Application setup with uWSGI

Each uWSGI application's configuration is represented on the filesystem as an `ini` file, typically found in `/etc/uwsgi/apps-available`. Symlinks are established between files in this directory into `/etc/uwsgi/apps-enabled` to tell the `uwsgi` daemon that an application needs to be running.

The following is an example uWSGI configuration file that you can use as a template:

{% highlight ini %}
[uwsgi]
vhost = true
chmod-socket = 666
socket = /tmp/app.sock
plugins = python
venv = /path/to/proj/env
chdir = /path/to/proj
module = modulename
callable = app
{% endhighlight %}

This will get our application housed by uWSGI. You can now enable this application:

{% highlight bash %}
$ sudo ln -s /etc/uwsgi/apps-available/app.ini /etc/uwsgi/apps-enabled/app.ini
$ sudo service uwsgi restart
{% endhighlight %}

### Web server setup

Finally, we'll get nginx to provide web access to our application. You may have specific web site files that you need to modify to do this, but this example assumes that you're in control of the `default` application.

Add the following section to `/etc/nginx/sites-available/default`:

{% highlight text %}
location /app {
        include uwsgi_params;
        uwsgi_param SCRIPT_NAME /app;
        uwsgi_modifier1 30;
        uwsgi_pass unix:/tmp/app.sock;
}
{% endhighlight %}

Reload your web server config, and you're ready to go:

{% highlight bash %}
$ sudo service nginx restart
{% endhighlight %}

