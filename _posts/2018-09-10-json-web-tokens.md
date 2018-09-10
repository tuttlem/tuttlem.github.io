---
layout: post
title: JSON Web Tokens
date: 2018-09-10
comments: false
categories: [ "json", "jwt", "token" ]
---

The [open standard](https://tools.ietf.org/html/rfc7519) of JSON Web Tokens allows parties to securely transfer claim information. Furthermore, signed tokens allow for verification of the token itself.

One of the most common use-cases of the JWT is for [authorization](https://en.wikipedia.org/wiki/Authorization). Once a user's secrets have been verified, they are issued a JWT containing claim information. This token is then attached to further requests that allows server applications to assert user's access to services, resources, etc.

JWTs can also be used for adhoc information transfer. A token's ability to be signed is an important characteristic providing information verifiability between parties. 

### Structure

A JWT is divided into three primary components:

* Header
* Payload
* Signature

The *header* contains information about the type of token; this article assumes that `JWT` is the type and the hashing algorithm. An example header might look something like this:

{% highlight text %}
{
  "alg": "RSA",
  "typ": "JWT"
}
{% endhighlight %}

The *payload* part is expected to have some standard attribute values. These values can fit into the following categories:

| Type | Description |
|------|-------------|
| Registered claims | "iss" Issuer, "sub" Subject, "aud" Audience, "exp" Expiration Time, "nbf" Not before, "iat" Issued at, "jti" JWT ID |
| Public claims | Freely definable names that should be made collision resistant. |
| Private claims | Private claims don't need to be collision resistant. Use these with caution. |

A simple payload for a user might look something like this:

{% highlight text %}
{
  "sub": "1",
  "email": "test@test.com",
  "name": "Tester"
}
{% endhighlight %}

Finally, the last piece of the JWT is the signature. The signature of the message depends on the hashing algorithm that you've selected in your header.

The calculation is going to look something like this:

{% highlight text %}
HMACSHA256(
  base64UrlEncode(header) + "." +
  base64UrlEncode(payload),
  "secret"
);
{% endhighlight %}

For instance, the following JWT:

{% highlight text %}
{
  "alg": "HS256",
  "typ": "JWT"
}

{
  "sub": "1",
  "email": "test@test.com",
  "name": "Tester"
}
{% endhighlight %}

Computes down to the following JWT:

{% highlight text %}
eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxIiwiZW1haWwiOiJ0ZXN0QHRlc3QuY29tIiwibmFtZSI6IlRlc3RlciJ9.mFv3TbmAMWui0w8ofwREb9xFqRRl0_Igahl8tbosHMw
{% endhighlight %}

You can see that the token itself is split into three encoded strings: `header.payload.signature`.

This token is now used in the `Authorization` header of your HTTP requests!

### References

* [jwt.io](https://jwt.io)
* [rfc-7519](https://tools.ietf.org/html/rfc7519)

