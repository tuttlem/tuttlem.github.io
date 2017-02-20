---
layout: post
title: Hashing with OpenSSL
date: 2017-02-20
comments: false
categories: [ "hash", "sha2", "sha256" ]
---

Today's post will be a quick tip on generating a hash using [OpenSSL](https://www.openssl.org/).

## Setup your makefile

We need to reference `libssl` and `libcrypto` in our Makefile:

{% highlight text %}
$(CC) $^ -o $(TARGET) -lssl -lcrypto
{% endhighlight %}

## The code

A simple `main` function that will hash a simple message:

{% highlight c %}
#include <stdio.h>
#include <string.h>
#include <openssl/sha.h>

int main(int argc, char* argv[]) {

   SHA256_CTX ctx;
   unsigned char digest[32];
   char *msg = "hello";

   SHA256_Init(&ctx);
   SHA256_Update(&ctx, msg, strlen(msg));
   SHA256_Final(digest, &ctx);

   int i = 0;

   for (i = 0; i < 32; i ++) {
      printf("%x", digest[i]);
   }

   return 0;
}
{% endhighlight %}

## Testing

Running this application just generates a hash of the word "hello":

{% highlight text %}
$ ./test
2cf24dba5fb0a3e26e83b2ac5b9e29e1b161e5c1fa7425e7343362938b9824%                 
{% endhighlight %}

We can verify our result using `sha256sum`:

{% highlight text %}
$ echo -n hello | sha256sum 
2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824  -
{% endhighlight %}

