---
layout: post
title: Getting NGINX to Do Things
date: 2025-06-26
comments: false
categories: [ nginx, devops, lua, systems ]
---

# Introduction

NGINX is famous for serving static content and handling reverse proxies — but it can do a lot more. In this post, 
we’re going to explore three “power moves” with NGINX:

- Running a **reverse proxy** to local services  
- Embedding **Lua scripts** using OpenResty  
- Talking to **local services over Unix domain sockets**

By the end, you’ll be able to glue together web services like a pro — no Node.js middleman required.

# Setup

Before we begin, we'll setup your local development environment so that you can experiment with a few of these 
configurations. We'll use `docker` and the [OpenResty](https://hub.docker.com/r/openresty/openresty) image to simplify 
our setup.

I've created a directory structure that looks like this:

{% highlight text %}
.
├── conf.d/
├── lua/
└── nginx.conf
{% endhighlight %}

The `nginx.conf` file has some boilerplate in it:

{% highlight conf %}
worker_processes 1;

events {
    worker_connections 1024;
}

http {
    lua_package_path "/usr/local/openresty/nginx/lua/?.lua;;";

    include       mime.types;
    default_type  application/octet-stream;

    sendfile        on;
    keepalive_timeout  65;

    include /etc/nginx/conf.d/*.conf;
}
{% endhighlight %}

We include the `lua_package_path` directive to tell OpenResty where to find any lua modules that we'll add in.

We'll build this directory structure out as we go. We'll start our proxy server at the root of that directory structure 
with the following command:

{% highlight bash %}
docker run --rm -it \              
    -p 8080:8080 \
    -v "$PWD/nginx.conf:/usr/local/openresty/nginx/conf/nginx.conf:ro" \
    -v "$PWD/conf.d/basic.conf:/etc/nginx/conf.d/default.conf:ro" \
    openresty/openresty:alpine
{% endhighlight %}

As we need, we'll mount in more configurations and modules.

Now that've got a testbed to work with, we can checkout a few of these different configurations.

# Reverse Proxy Basics

Let’s say you’ve got a service running locally on port 5000, and you want to expose it at `https://example.com/api/`.

Here’s a minimal NGINX config:

{% highlight text %}
server {
    listen 8080;

    location /api/ {
        proxy_pass http://localhost:5000/;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
    }
}
{% endhighlight %}

A couple of points to note here:

- That trailing slash on `proxy_pass` matters.
- Use `proxy_set_header` to preserve client info.

This setup allows you to do things like offload your SSL/TLS onto NGINX rather than needing to deal with it inside of 
your application. You're actually controlling the flow of traffic using this reverse proxy setup, so it will make your 
overall system design a lot more flexible should you need to pivot in future.

I put this configuration into a file called `./conf.d/basic.conf`. I run this with the following:

{% highlight bash %}
docker run --rm -it \              
    -p 8080:8080 \
    -v "$PWD/nginx.conf:/usr/local/openresty/nginx/conf/nginx.conf:ro" \
    -v "$PWD/conf.d/basic.conf:/etc/nginx/conf.d/default.conf:ro" \
    openresty/openresty:alpine
{% endhighlight %}

Sending a `curl` request should fail (if you're like me and you don't have a service running on port `5000`):

{% highlight text %}
curl http://localhost:8080/api/test

<html>
<head><title>502 Bad Gateway</title></head>
<body>
<center><h1>502 Bad Gateway</h1></center>
<hr><center>openresty/1.27.1.2</center>
</body>
</html>
{% endhighlight %}

This is all well and good for basic reverse proxying. What if you need a little more functionality at your proxy? You 
may want some arbitrary logic. In order to do this, you need some more help.

# Lua + OpenResty: Custom Logic at the Edge

Want conditional logic? Inline transformations? Run Lua scripts inside NGINX using [OpenResty](https://openresty.org/).

Here’s an example that rewrites responses:

{% highlight text %}
location /hello {
    content_by_lua_block {
        ngx.say("Hello from Lua!")
    }
}
{% endhighlight %}

Save this into `./conf.d/lua.conf` and then ee can get this running with the following:

{% highlight bash %}
docker run --rm -it \              
  -p 8080:8080 \
  -v "$PWD/nginx.conf:/usr/local/openresty/nginx/conf/nginx.conf:ro" \
  -v "$PWD/conf.d/lua.conf:/etc/nginx/conf.d/default.conf:ro" \
  openresty/openresty:alpine
{% endhighlight %}

A simple request to the `/hello` endpoint:

{% highlight text %}
curl http://localhost:8080/hello
Hello from Lua!
{% endhighlight %}

This demonstrates the usage of `content_by_lua_block` to provide content to the response.

Or maybe you want to inspect a header before proxying:

{% highlight text %}
location /auth {
    access_by_lua_block {
        local token = ngx.var.http_authorization
        if token ~= "Bearer secrettoken" then
            ngx.status = 401
            ngx.say("Unauthorized")
            return ngx.exit(401)
        end
    }

    proxy_pass http://localhost:7000/;
}
{% endhighlight %}

We can now test this out with the following:

{% highlight bash %}
curl -H "Authorization: Bearer secrettoken" http://localhost:8080/auth
{% endhighlight %}

Which sends our super secret token through.

Lua runs **inside NGINX's event loop**, it’s lightweight, fast, and perfect for request-time filtering, routing, 
or even metrics.

# Proxying to a Unix Domain Socket

Sometimes your app listens on a Unix domain socket, not a TCP port. NGINX can talk to those too:

First, we'll start a simple server running locally with `socat`.

{% highlight bash %}
socat -v UNIX-LISTEN:/tmp/mysock,reuseaddr,fork SYSTEM:'while read line; do echo "You said: $line"; done'
{% endhighlight %}

The socket is created by the host user, but inside the container, NGINX runs as a different user (typically `nobody`).
To avoid a “permission denied” error, I made the socket world-accessible:

{% highlight bash %}
chmod 777 /tmp/mysock
{% endhighlight %}

Then configure NGINX:

{% highlight text %}
server {
    listen 8080;

    location /socket/ {
	proxy_pass http://unix:/tmp/mysock:;
	proxy_set_header Host $host;
	proxy_set_header X-Real-IP $remote_addr;
    }
}
{% endhighlight %}

Now we can run this with the following:

{% highlight bash %}
docker run --rm -it \
    -p 8080:8080 \
    -v "$PWD/nginx.conf:/usr/local/openresty/nginx/conf/nginx.conf:ro" \
    -v "$PWD/conf.d/unix.conf:/etc/nginx/conf.d/default.conf:ro" \
    -v "/tmp/mysock:/tmp/mysock" \
    openresty/openresty:alpine
{% endhighlight %}

You can see that we've needed to mount in our socket.

## Wrap-up

NGINX isn’t just a dumb proxy — with OpenResty and some careful configuration, it becomes a programmable router and 
request gatekeeper. If you’re building internal tools, APIs, or secure microservices — this setup is gold.