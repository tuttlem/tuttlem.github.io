---
layout: post
title: Pretty Good Privacy
date: 2016-01-30
comments: false
categories: [ "pgp", "gpg", "gnupg" ]
---

[Pretty Good Privacy](https://en.wikipedia.org/wiki/Pretty_Good_Privacy) is a cryptographic computer program used to encrypt and authenticate messages. [GnuPG](https://www.gnupg.org/) is the free replacement to this program. It is an implementation of the [OpenPGP Message Format](http://www.ietf.org/rfc/rfc4880.txt).

Today's post will be about the creation of GPG keys, using them to encrypt and authenticate a message as well as the verification process.

### Creating your keys

First job is to create your credentials. These keys are what people are going to use that certify that something was from you. 

To generate your keys:

{% highlight bash %}
gpg --gen-key
{% endhighlight %}

You're then asked what *kind* of key that you want:

{% highlight text %}
gpg (GnuPG) 1.4.18; Copyright (C) 2014 Free Software Foundation, Inc.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.

Please select what kind of key you want:
   (1) RSA and RSA (default)
   (2) DSA and Elgamal
   (3) DSA (sign only)
   (4) RSA (sign only)
Your selection? 
{% endhighlight %}

The default of `RSA and RSA` is fine. This says that we'll use [RSA](https://en.wikipedia.org/wiki/RSA_%28cryptosystem%29) keys to sign and encrypt our messages.

Size and expiration are specified next. 

{% highlight text %}
RSA keys may be between 1024 and 4096 bits long.
What keysize do you want? (2048) 
Requested keysize is 2048 bits
Please specify how long the key should be valid.
         0 = key does not expire
      <n>  = key expires in n days
      <n>w = key expires in n weeks
      <n>m = key expires in n months
      <n>y = key expires in n years
Key is valid for? (0) 
Key does not expire at all
{% endhighlight %}

In my example here, I've chosen a key length of `2048` and a key won't expire. Now you're asked for your *Real Name*, *E-mail address* and *Comment*. It's a good idea to use **real** or **correct** values for *Real Name* and *E-mail address* as people will see these and use them as a visual indicator to authenticate you. In the *Comment* field, I normally put the key's purpose (email/personal/office/etc).

After some generation, the key generation process will present you with your fingerprint details. The 8 HEX chars that identify your public key are significant. This is the ID that you'll use to reference this key.

At any point you can take a look at this fingerprint information with the following:

{% highlight bash %}
gpg --fingerprint user@email.com
{% endhighlight %}

### Backup your keys

You always need a way that you can get hold of your keys. In the next steps we're going to export your private and public keys to file.

It's **important** to backup your private key in case you lose it. Your hard drive crashes, or house gets flooded. You need another copy of your secret put somewhere **else**. So, you can get a text copy of the key with the following.

{% highlight bash %}
gpg --export-secret-keys --armor user@email.com > user-email-com-priv.asc
{% endhighlight %}

You need to be able to freely distribute your public key to people as this certificate is what people will use to verify messages sent by you are actually send by you! It's also what's used in the encryption process for messages sent to you.

{% highlight bash %}
gpg --export --armor user@email.com > user-email-com-pub.asc
{% endhighlight %}

### Sending to a keyserver

So that other people will have the opportunity to look up your public key for authentication or decryption purposes, you'll need to send your public key to a key server.

{% highlight bash %}
gpg --send-key ABCD1234
{% endhighlight %}

By default, this sends the key out to `keys.gnupg.net`.

{% highlight text %}
gpg: sending key ABCD1234 to hkp server keys.gnupg.net
{% endhighlight %}

You can change this behavior and send your key to another server by specifying the keyserver using the `--keyserver` switch.

### Certificate revocation

It's also a *good idea* to generate and store away a [revocation certificate](https://en.wikipedia.org/wiki/Revocation_list) so that at any time, you can revoke your generated certificate.

{% highlight bash %}
gpg --output user-email-revoke.asc --gen-revoke ABCD1234
{% endhighlight %}

If you do need to execute on this, it's just about importing this revocation certificate into the store.

{% highlight bash %}
gpg --import user-email-revoke.asc
{% endhighlight %}

To let everyone know about it, you also need to push the certificate out to the key server.

{% highlight bash %}
gpg --keyserver subkeys.pgp.net --send ABCD1234
{% endhighlight %}

### Testing it out

There are [heaps of different programs](https://www.gnupg.org/related_software/swlist.html) that integrate with GnuPG, but for today's purposes we're just going to create a simple text message; sign and verify it.

So, let's create a message:

{% highlight bash %}
cat > message.txt
Hello, world!
^C
{% endhighlight %}

We can use a binary format to sign and verify:

{% highlight bash %}
# sign the message 
gpg --output message.sig --sign message.txt

# verify and extract the original message
gpg --output message.verf.txt --decrypt message.sig
{% endhighlight %}

This should be pretty straight forward. The more familiar text armored messages are done using the `--clearsign` switch:

{% highlight bash %}
# sign the message 
gpg --clearsign message.txt

# verify the message
gpg --verify message.txt.asc
{% endhighlight %}

