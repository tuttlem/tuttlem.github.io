---
layout: post
title: The Magic of Diffie Hellman
date: 2025-04-21
comments: false
categories: [ security secrets diffie ]
---

# Introduction

Imagine two people, Alice and Bob. They’re standing in a crowded room — everyone can hear them. Yet somehow, they want 
to agree on a secret password that only they know.

Sounds impossible, right?

That’s where *Diffie–Hellman key exchange* comes in. It’s a bit of mathematical magic that lets two people agree on a 
shared secret — even while everyone is listening.

Let’s walk through how it works — and then build a toy version in code to see it with your own eyes.

# Mixing Paint

Let’s forget numbers for a second. Imagine this:

1. Alice and Bob agree on a **public color** — let’s say **yellow paint**.
2. Alice secretly picks **red**, and Bob secretly picks **blue**.
3. They mix their secret color with the yellow:
   * Alice sends Bob the result of _red + yellow_.
   * Bob sends Alice the result of _blue + yellow_.
4. Now each of them adds their secret color again:
   * Alice adds red to Bob’s mix: **(yellow + blue) + red**
   * Bob adds blue to Alice’s mix: **(yellow + red) + blue**

Both end up with the **same final color**: yellow + red + blue!

But someone watching only saw:

* The public yellow
* The mixes: (yellow + red), (yellow + blue)

They can’t reverse it to figure out the red or blue. 

Mixing paint is easy, but un-mixing it is **really hard**.

# From Paint to Numbers

In the real world, computers don’t mix colors — they work with math.

Specifically, Diffie–Hellman uses something called *modular arithmetic*. Module arithmetic is just math where we 
“wrap around” at some number.

For example:

$$
7 \mod 5 = 2
$$

We’ll also use *exponentiation* — raising a number to a power.

And here’s the core of the trick: it’s easy to compute this:

$$
\text{result} = g^{\text{secret}} \mod p
$$

But it’s hard to go backward and find the `secret`, even if you know `result`, `g`, and `p`.

This is the secret sauce behind Diffie–Hellman.

# A Toy Implementation

Let's see this story in action.

{% highlight python %}
import random

# Publicly known numbers
p = 23      # A small prime number
g = 5       # A primitive root modulo p (more on this later)

print("Public values:  p =", p, ", g =", g)

# Alice picks a private number
a = random.randint(1, p-2)
A = pow(g, a, p)   # A = g^a mod p

# Bob picks a private number
b = random.randint(1, p-2)
B = pow(g, b, p)   # B = g^b mod p

print("Alice sends:", A)
print("Bob sends:  ", B)

# Each computes the shared secret
shared_secret_alice = pow(B, a, p)   # B^a mod p
shared_secret_bob = pow(A, b, p)     # A^b mod p

print("Alice computes shared secret:", shared_secret_alice)
print("Bob computes shared secret:  ", shared_secret_bob)
{% endhighlight %}

Running this (your results may vary due to random number selection), you'll see something like this:

{% highlight text %}
Public values:  p = 23 , g = 5
Alice sends: 10
Bob sends:   2
Alice computes shared secret: 8
Bob computes shared secret:   8
{% endhighlight %}

The important part here is that Alice and Bob both end up with **the same shared secret**.

Let's breakdown this code, line by line.

{% highlight python %}
p = 23
g = 5
{% endhighlight %}

These are **public constants**. Going back to the paint analogy, you can think of `p` as the size of the palette and `g` 
as our base "colour". We are ok with these being known to anybody.

{% highlight python %}
a = random.randint(1, p-2)
A = pow(g, a, p)
{% endhighlight %}

Alice chooses a secret nunber `a`, and then computes $$ A = g^a \mod p $$. This is her **public key** - the equivalent of 
"red + yellow".

Bob does the same with his secret `B`, producing `B`.

{% highlight python %}
shared_secret_alice = pow(B, a, p)
shared_secret_bob = pow(A, b, p)
{% endhighlight %}

They both raise the other’s public key to their secret power. And because of how exponentiation works, both arrive at 
the same final value:

$$
(g^b)^a \mod p = (g^a)^b \mod p
$$

This simplifies to:

$$
g^{ab} \mod p
$$

This is the *shared secret*.

# Try it yourself

Try running the toy code above multiple times. You’ll see that:

* Every time, Alice and Bob pick new private numbers.
* They still always agree on the same final shared secret.

And yet… if someone was eavesdropping, they’d only see `p`, `g`, `A`, and `B`. That’s not enough to figure out `a`, 
`b`, or the final shared secret (unless they can solve a very hard math problem called the **discrete logarithm problem** — 
something computers can’t do quickly, even today).

# It's not perfect

Diffie–Hellman is powerful, but there’s a catch: it doesn’t _authenticate_ the participants.

If a hacker, Mallory, can intercept the messages, she could do this:
* Pretend to be Bob when talking to Alice
* Pretend to be Alice when talking to Bob

Now she has two separate shared secrets — one with each person — and can **man-in-the-middle** the whole conversation.

So in practice, **Diffie–Hellman is used _with_ authentication** — like digital certificates or signed messages — to 
prevent this attack.

So, the sorts of applications you'll see this used in are:

* TLS / HTTPS (the “S” in secure websites)
* VPNs
* Secure messaging (like Signal)
* SSH key exchanges

It’s one of the fundamental building blocks of internet security.

# Conclusion

Diffie–Hellman feels like a magic trick: two people agree on a secret, in public, without ever saying the secret out 
loud.

It’s one of the most beautiful algorithms in cryptography — simple, powerful, and still rock-solid almost 50 years 
after it was invented.

And now, you’ve built one yourself.