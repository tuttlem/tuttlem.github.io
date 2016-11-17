---
layout: post
title: Using nginx as a proxy
date: 2016-11-17
comments: false
categories: [ "nginx", "proxy", "docker" ]
---

When running applications in [docker](https://www.docker.com/) containers, it can make sense to put a [proxy server](https://en.wikipedia.org/wiki/Proxy_server) in front. It's relatively simple to setup an [nginx](https://nginx.org/en/) server to sit in front of any application which I'll demonstrate in this article.

## Configuration

In order to get started, we'll use the [nginx image](https://hub.docker.com/_/nginx/) hosted up on [dockerhub](https://hub.docker.com/). This particular image allows us to specify a configuration file to a web server relatively simply.

To setup the scenario, we have a [node.js](https://nodejs.org/en/) application running on port `3000` of the host machine that we'd look to proxy through the nginx proxy. Here's how the configuration would look, over port `80`:

{% highlight plain %}
server {
  listen 80;
  index index.html;

  server_name localhost;

  error_log /var/log/nginx/error.log;
  access_log /var/log/nginx/access.log;
  root /var/www/public;

  location ~* /my-api {
    rewrite /my-api(.*) /$1 break;
    proxy_pass https://172.17.0.1:4010;
    proxy_http_version 1.1;
    proxy_set_header Upgrade $http_upgrade;
    proxy_set_header Connection 'upgrade';
    proxy_set_header Host $host;
    proxy_cache_bypass $http_upgrade;
  }

}
{% endhighlight %}

There's even a `rewrite` here that takes the `my-api` part of the original request URI out of the forwarded request, so that the node.js application can be treated directly off the root.

## Start me up!

To now get this started, we need to sub-in this configuration file as if it were part of the running container.

{% highlight plain %}
docker run -ti --rm -v $(pwd)/default.conf:/etc/nginx/conf.d/default.conf -p 80:80 nginx
{% endhighlight %}

## Security

Yep. Now we need to use [SSL](https://en.wikipedia.org/wiki/Transport_Layer_Security) and put the application over `443`! First up, let's create a self-signed certificate using [OpenSSL](https://www.openssl.org/).

{% highlight text %}
openssl req -x509 -nodes -days 3652 -newkey rsa:2048 -keyout nginx.key -out nginx.crt
{% endhighlight %}

Now that we've got our certificate `nginx.crt` and key `nginx.key`, we can change the configuration now to proxy our application securely:

{% highlight plain %}
server {
  listen 80;
  listen 443 ssl;
  index index.html;

  server_name localhost;
  ssl_certificate /etc/nginx/ssl/nginx.crt;
  ssl_certificate_key /etc/nginx/ssl/nginx.key;

  error_log /var/log/nginx/error.log;
  access_log /var/log/nginx/access.log;
  root /var/www/public;

  location ~* /my-api {
    rewrite /my-api(.*) /$1 break;
    proxy_pass https://172.17.0.1:4010;
    proxy_http_version 1.1;
    proxy_set_header Upgrade $http_upgrade;
    proxy_set_header Connection 'upgrade';
    proxy_set_header Host $host;
    proxy_cache_bypass $http_upgrade;
  }

}
{% endhighlight %}

Now when we start up the container, we not only need to expose `443` for SSL, but we'll also volume-in our certificate and key:

{% highlight text %}
docker run    \
       -ti    \
       --rm   \
       -v $(pwd)/default.conf:/etc/nginx/conf.d/default.conf \
       -v $(pwd)/ssl:/etc/nginx/ssl \
       -p 443:443 \
       nginx
{% endhighlight %}

Now you can proxy your other dockerized web-applications through nginx without much hassle at all.

