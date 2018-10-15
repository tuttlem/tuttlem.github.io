---
layout: post
title: Trusting a self-signed certificate
date: 2018-10-12
comments: false
categories: [ "ssl", "trust", "certificate", "linux" ]
---

When working in development and sandboxes, it can make sense to trust the [self-signed certificates](https://en.wikipedia.org/wiki/Self-signed_certificate) that you might be using. This can lower the amount of workflow noise that you might endure.

In today's article, I'll take you through generating a certificate; using the certificate (its use-case is terribly simple), and finally trusting the certificate.

### Generation

In a previous post titled ["Working with OpenSSL"]({% post_url 2015-02-07-working-with-openssl %}), I took you through a few different utilities available to you within the [OpenSSL](https://www.openssl.org/) suite. One of the sections was on generating your own self-signed certificate.

{% highlight bash %}
openssl req -x509 -nodes -days 365 -subj '/C=AU/ST=Queensland/L=Brisbane/CN=localhost' -newkey rsa:4096 -keyout server.key -out server.crt
{% endhighlight %}

You should receive output which looks like the following:

{% highlight text %}
Generating a RSA private key
.......................................................................................................++++
...............................................................................................................................++++
writing new private key to 'server.key'
-----
{% endhighlight %}

On the filesystem now you should have a `server.key` and `server.cer` files waiting for you.

### Using the certificate

Now we're going to stand up a web server that uses this key/certificate pair. Using the [nginx docker image](https://hub.docker.com/_/nginx/), we can quickly get this moving with the following `nginx.conf`.

{% highlight text %}
user  nginx;
worker_processes  1;

error_log  /var/log/nginx/error.log warn;
pid        /var/run/nginx.pid;


events {
    worker_connections  10000;
}


http {
    include       /etc/nginx/mime.types;
    default_type  application/octet-stream;

    log_format  main  '$remote_addr - $remote_user [$time_local] "$request" '
                      '$status $body_bytes_sent "$http_referer" '
                      '"$http_user_agent" "$http_x_forwarded_for"';

    access_log  /var/log/nginx/access.log  main;

    sendfile        on;
    #tcp_nopush     on;

    keepalive_timeout  65;

    #gzip  on;

  server {
    listen 443;
    index index.html;

    server_name localhost;

    ssl_certificate /opt/server.crt;
    ssl_certificate_key /opt/server.key;

    ssl on;
    root /var/www/public;

    location / {
      try_files $uri $uri/;
    }
  }
}
{% endhighlight %}

Starting the server requires the cerificate, key and configuration file to be mounted in. I've also exposed 443 here.

{% highlight bash %}
docker run --rm \ 
           -ti \
           -v $(pwd)/nginx.conf:/etc/nginx/nginx.conf:ro \
           -v $(pwd)/server.key:/opt/server.key \
           -v $(pwd)/server.crt:/opt/server.crt \
           -p 443:443 \
           nginx
{% endhighlight %}

Right now, when we use the `curl` command without the `--insecure` switch, we receive the following:

{% highlight text %}
curl: (60) SSL certificate problem: self signed certificate
More details here: https://curl.haxx.se/docs/sslcerts.html

curl failed to verify the legitimacy of the server and therefore could not
establish a secure connection to it. To learn more about this situation and
how to fix it, please visit the web page mentioned above.
{% endhighlight %}

### Trusting the certificate

We can now use [cerutil](https://developer.mozilla.org/en-US/docs/Mozilla/Projects/NSS/tools/NSS_Tools_certutil) to work with the NSS database to add this certificate.

If you're on a brand new system, you may need to create your NSS database. This can be done with the following instructions. Please note, that I'm not using a password to secure the database here.

{% highlight bash %}
mkdir -p %HOME/.pki/nssdb
certutil -N -d $HOME/.pki/nssdb --empty-password
{% endhighlight %}

With a database created, you can now add the actual certificate itself. You can acquire the certificate with the following script (that uses OpenSSL):

{% highlight bash %}
#!/bin/sh
#
# usage:  import-cert.sh remote.host.name [port]
#
REMHOST=$1
REMPORT=${2:-443}
exec 6>&1
exec > $REMHOST
echo | openssl s_client -connect ${REMHOST}:${REMPORT} 2>&1 |sed -ne '/-BEGIN CERTIFICATE-/,/-END CERTIFICATE-/p'
certutil -d sql:$HOME/.pki/nssdb -A -t "P,," -n "$REMHOST" -i $REMHOST 
exec 1>&6 6>&-
{% endhighlight %}

This script is doing a little bit; but most important to see that `openssl` acquires the certificate for us; then we issue a call to `certutil` to add the certificate into our store.

Chrome will look for the nss database in `$HOME/.pki/nssdb`. This is why this folder has been chosen. The `-t` switch allows you to specify `trustargs`. Lifted from the manpage:

{% highlight text %}
·   p - Valid peer
·   P - Trusted peer (implies p)
·   c - Valid CA
·   C - Trusted CA (implies c)
·   T - trusted CA for client authentication (ssl server only)
{% endhighlight %}

The trust settings are applied as a combination of these characters, in a series of three.

> There are three available trust categories for each certificate, expressed in the order SSL, email, object signing for each trust setting.

With the certificate added into the store, we can re-start chrome and hit our website. Chrome no longer complains about the certificate not being trusted.

