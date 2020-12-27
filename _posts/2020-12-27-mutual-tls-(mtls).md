---
layout: post
title: Mutual TLS (mTLS)
date: 2020-12-27
comments: false
categories: [ "" ]
---

### Introduction

[TLS](https://en.wikipedia.org/wiki/Transport_Layer_Security/) has forever played a very large part in securing internet communications. Secure Socket Layer (SSL) filled this space prior to TLS coming to the fore.

In today's article, I'm going to walk through an exercise of mTLS which is just an extension of TLS.

### CA

First of all, we need a certificate authority (CA) that both the client and the server will trust. We generate these using `openssl`.

{% highlight text %}
openssl req -new -x509 -nodes -days 365 -subj '/CN=my-ca' -keyout ca.key -out ca.crt
{% endhighlight %}

This now puts a private key in `ca.key` and a certificate in `ca.crt` on our filesystem. We can inspect these a little further with the following.

{% highlight text %}
openssl x509 --in ca.crt -text --noout
{% endhighlight %}

Looking at the output, we see some interesting things about our CA certificate. Most importantly the `X509v3 Basic Constraints` value is set `CA:TRUE`, telling us that this certificate can be used to sign other certificates (like CA certificates can).

### Server

The server now needs a key and certificate. Key generation is simple, as usual:

{% highlight text %}
openssl genrsa -out server.key 2048
{% endhighlight %}

We need to create a certificate that has been signed by our CA. This means we need to generate a certificate signing request, which is then used to produce the signed certificate.

{% highlight text %}
openssl req -new -key server.key -subj '/CN=localhost' -out server.csr
{% endhighlight %}

This gives us a signing request for the domain of `localhost` as mentioned in the `-subj` parameter. This signing request now gets used by the CA to generate the certificate.

{% highlight text %}
openssl x509 -req -in server.csr -CA ca.crt -CAkey ca.key -CAcreateserial -days 365 -out server.crt
{% endhighlight %}

Inspecting the server certificate, you can see that it's quite a bit simpler than the CA certificate. We're only able to use this certificate for the subject that we nominated; `localhost`.

### Client

The generation of the client certificates is very much the same as the server.

{% highlight text %}
# create a key
openssl genrsa -out client.key 2048

# generate a signing certificate
openssl req -new -key client.key -subj '/CN=my-client' -out client.csr

# create a certificate signed by the CA
openssl x509 -req -in client.csr -CA ca.crt -CAkey ca.key -CAcreateserial -days 365 -out client.crt
{% endhighlight %}

The subject in this case is `my-client`. 

The `-CAcreateserial` number also ensures that we have unique serial numbers between the server and client certificates. Again, this can be verified when you inspect the certificate.

{% highlight text %}
# Server serial number
        Serial Number:
            5c:2c:47:44:2c:13:3b:c9:56:56:99:37:3f:c9:1e:62:c4:c7:df:20

# Client serial number
        Serial Number:
            5c:2c:47:44:2c:13:3b:c9:56:56:99:37:3f:c9:1e:62:c4:c7:df:21
{% endhighlight %}

Only the last segment was incremented here. You get the idea though. Unique.

### Appliation

Now, we setup a basic node.js server that requires mTLS.

{% highlight javascript %}
const https = require('https');
const fs = require('fs');

const hostname = 'localhost';
const port = 3000;

const options = { 
    ca: fs.readFileSync('ca.crt'), 
    cert: fs.readFileSync('server.crt'), 
    key: fs.readFileSync('server.key'), 
    rejectUnauthorized: true,
    requestCert: true, 
}; 

const server = https.createServer(options, (req, res) => {
  res.statusCode = 200;
  res.setHeader('Content-Type', 'text/plain');
  res.end('Hello World');
});

server.listen(port, hostname, () => {
  console.log(`Server running at http://${hostname}:${port}/`);
});
{% endhighlight %}

Most important here is that the server's options specify `rejectUnauthorized` as well as `requestCert`. This will force the mTLS feedback look back to the client.

A curl request now verifies that the solution is secured by this system of certificates.

{% highlight text %}
curl --cacert ca.crt --key client.key --cert client.crt https://localhost:3000
{% endhighlight %}

The client's key, certificate, and the ca cert accompany a successful request. A request in any other format simply fails as the authentication requirements have not been met.




