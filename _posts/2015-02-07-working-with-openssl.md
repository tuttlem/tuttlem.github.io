---
layout: post
title: Working with openssl
date: 2015-02-07
comments: false
categories: [ "openssl", "encryption" ]
---

[OpenSSL](https://www.openssl.org/) is the open source project that provides the world with [SSL and TLS](http://en.wikipedia.org/wiki/Transport_Layer_Security). In today's post, I'll walk through some simple tasks to encrypt and decrypt your data.

### Features

OpenSSL is a <strong>very</strong> feature-rich library. It contains many pieces of functionality that you should study in more detail. The [man page](https://www.openssl.org/docs/apps/openssl.html) for it goes into all of these details in great depth.

### Encoding information

Perhaps a slightly edge-case piece of functionality, OpenSSL has the ability to [Base64](http://en.wikipedia.org/wiki/Base64) encode your information. It's no where near actually securing your information, but the facility is there.

You can Base64 encode a string with the following command:

{% highlight bash %}
$ echo "Hello, world!" | openssl enc -base64
SGVsbG8sIHdvcmxkIQo=
{% endhighlight %}

You can bring it back to plain text with the following:

{% highlight bash %} 
$ echo "SGVsbG8sIHdvcmxkIQo=" | openssl enc -base64 -d
Hello, world!
{% endhighlight %}

### Encrypt with a password

OpenSSL gives you the ability to encrypt a piece of information using a password. This is a simple way of securing your information without certificates, but isn't a very strong strategy for information security.

Take a look under the [Encoding and Cipher Commands](https://www.openssl.org/docs/apps/openssl.html#encoding_and_cipher_commands) for a full range of strategies here. Where we used the `base64` options above, no password was asked for. This is because it's just an encoding. If we were to use the `bf` option (which will use the [Blowfish Cipher](http://en.wikipedia.org/wiki/Blowfish_%28cipher%29)), we're prompted for a password.

{% highlight bash %}
$ echo "Hello, world" | openssl enc -bf > password_enc.dat
enter bf-cbc encryption password:
Verifying - enter bf-cbc encryption password:
{% endhighlight %}

`password_enc.dat` contains what would appear to be garbage, but it is our string; just encrypted. To get our plain text back:

{% highlight bash %}
$ openssl enc -bf -d -in password_enc.dat 
enter bf-cbc decryption password:
Hello, world!
{% endhighlight %}

You need to enter the <em>correct</em> password in order to get your plain text back. Pretty simple. This is the process for any of the ciphers mentioned above.

### Encrypt with a key pair

Stepping up the complexity, you can get OpenSSL to encrypt and decrypt your data using [public-key cryptographyy](http://en.wikipedia.org/wiki/Public-key_cryptography). 

First of all, we need to generate a public/private key pair. The following command will generate a private key. This will be an [RSA](http://en.wikipedia.org/wiki/RSA_(cryptosystem)) keypair with a 4096 bit private key.

{% highlight bash %}
$ openssl genrsa -out private_key.pem 4096
Generating RSA private key, 4096 bit long modulus
....++
......................................................................................................................................................................................................................++
e is 65537 (0x10001)
{% endhighlight %}

Now that the private key has been generated, we extract the public key from it:

{% highlight bash %}
$ openssl rsa -pubout -in private_key.pem -out public_key.pem
writing RSA key
{% endhighlight %}

You can view all of the details of your keypair details with the following command. It's a pretty verbose information dump, so brace yourself.

{% highlight bash %}
$ openssl rsa -text -in private_key.pem
{% endhighlight %}

We encrypt the source information with the public key and perform the decryption using the private key. 

To encrypt the information:

{% highlight bash %}
$ echo "Hello, world" > encrypt.txt
$ openssl rsautl -encrypt -inkey public_key.pem -pubin -in encrypt.txt -out encrypt.dat
{% endhighlight %}

To decrypt the information:

{% highlight bash %}
$ openssl rsautl -decrypt -inkey private_key.pem -in encrypt.dat -out decrypt.txt
$ cat decrypt.txt 
Hello, world
{% endhighlight %}

### Working with certificates

You can use OpenSSL to generate a [self-signed certificate](http://en.wikipedia.org/wiki/Self-signed_certificate).

Generating a self-signed certificate is a fairly simple process. The following will generate a certificate and private key (in the one file) that's valid for 1 year. This certificate's key won't be protected by a passphrase.

{% highlight bash %}
$ openssl req -x509 -nodes -days 365 -newkey rsa:1024 -keyout mycert.pem -out mycert.pem
{% endhighlight %}

You can shorted the key generation process (make it ask less questions) by specifying all of the subject details in the generation command:

{% highlight bash %}
$ openssl req -x509 -nodes -days 365 -subj '/C=AU/ST=Queensland/L=Brisbane/CN=localhost' -newkey rsa:4096 -keyout mycert2.pem -out mycert2.pem
{% endhighlight %}

### Other functions

You can use OpenSSL to generate some random data for you as well. This is useful in scenarios where your application requires [nonce data](http://en.wikipedia.org/wiki/Cryptographic_nonce). The `rand` switch does this easily:

{% highlight bash %}
$ openssl rand -base64 128
tfINhtHHe5LCek2mV0z6OlCcyGUaHD6xM0jQYAXPNVpy0tjoEB4gy7m6f/0Fb4/K
cKyDfZEmpvoc3aYdQuCnH1kfJk1EQR1Gbb3xyW22KOcfjuEot5I+feinilJcDfWY
aJKDyuNUOn9YuZ8aALhP1zhA0knAT5+tKtNxjjNar04=
{% endhighlight %}

Piping the contents of `/dev/urandom` through OpenSSL's base64 encoder will also perform the same task (with better entropy).

[Prime](http://en.wikipedia.org/wiki/Prime_number) testing is an important cryptographic step and can be achieved with the `prime` switch:

{% highlight bash %}
$ openssl prime 3
3 is prime
$ openssl prime 4
4 is not prime
$ openssl prime 5
5 is prime
$ openssl prime 6
6 is not prime
{% endhighlight %}

A really practical utility bundled inside of OpenSSL is the testing server that you can instantiate to test out your certificates that you generate.

{% highlight bash %}
$ openssl s_server -cert mycert.pem -www
{% endhighlight %}

This starts a HTTPS server on your machine. You can point your web browser to [https://server:4433/](https://server:4433/) to see how a browser responds to your certificate.

You can also use OpenSSL as a client to pull down remote certificates:

{% highlight bash %}
$ openssl s_client -connect server:443
{% endhighlight %}

