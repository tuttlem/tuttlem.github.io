---
layout: post
title: Kerberos on Linux
date: 2025-09-13
comments: false
categories: [ kerberos, linux, ssh, sso, identity, security ]
---

# Introduction

Kerberos is one of those protocols that sounds mysterious until you see it in action. The moment you type `kinit`, run 
`klist`, and watch a ticket pop up, it clicks: this is Single Sign-On in its rawest form. In this post we’ll set up a 
tiny realm on a Debian test box (`koffing.local`), get a ticket-granting ticket (TGT), and then use it for SSH without 
typing a password.

## What is Kerberos?

Born at MIT’s Project Athena in the 1980s, Kerberos solved campus-wide single sign-on over untrusted networks. It 
matured through v4 to **Kerberos 5** (the standard you use today). It underpins enterprise SSO in Windows domains 
(Active Directory) and many UNIX shops.

Kerberos authenticates clients to services without sending reusable secrets. You authenticate once to the **KDC**, get 
a **TGT** (Ticket Granting Ticket), then use it to obtain per-service **tickets** from the **TGS** 
(Ticket Granting Service). 

Services trust the KDC, not your password.

## Core terms

- **Realm**: Admin boundary (e.g., `LOCAL`).
- **Principal**: Identity in the realm, like `michael@LOCAL` (user) or `host/koffing.local@LOCAL` (service).
- **KDC**: The authentication authority. Runs on `koffing.local` as `krb5kdc` and `kadmind`.
- **TGT**: Your “hall pass.” Lets you ask the KDC for service tickets.
- **Service ticket**: What you present to a service (e.g., SSHD on `koffing.local`) to prove identity.
- **Keytab**: File holding long-term service keys (like for sshd). Lets the service authenticate without storing a password.

Here's a visual representation of how the Kerberos flow operates:

<div class="mermaid">
sequenceDiagram
    participant U as User
    participant AS as KDC/AS
    participant TGS as KDC/TGS
    participant S as Service (e.g., SSHD)
    U->>AS: AS-REQ (I am michael)
    AS-->>U: AS-REP (TGT + session key)
    U->>TGS: TGS-REQ (I want ticket for host/koffing.local)
    TGS-->>U: TGS-REP (service ticket)
    U->>S: AP-REQ (here's my service ticket)
    S-->>U: AP-REP (optional) + access granted
</div>

Ok, with all of that out of the way we can get to setting up.

# Setup

There's a few packages to install and a little bit of configuration. All of these instructions are written for a 
Debian/Ubuntu flavour of Linux. I'm sure that the instructions aren't too far off for other distributions.

## Install the packages

We install the **Key Distribution Service** `krb5-kdc`, **Administration Server** `krb5-admin-server`, and some **Client 
Utilities** `krb5-user`.

{% highlight bash %}
sudo apt update
sudo apt install -y krb5-kdc krb5-admin-server krb5-user
{% endhighlight %}


## Configure your realm

The fully qualified name of my virtual machine that I'm testing all of this out on is called `koffing.local`. These 
values would change to suit your environment.

Edit `/etc/krb5.conf` and make sure it looks like this:

{% highlight plain %}
[libdefaults]
  default_realm = LOCAL
  rdns = false
  dns_lookup_kdc = false
  forwardable = true

[realms]
  LOCAL = {
    kdc = koffing.local
    admin_server = koffing.local
  }

[domain_realm]
  .local = LOCAL
  koffing.local = LOCAL
{% endhighlight %}

Make sure your host resolves correctly:

{% highlight bash %}
hostname -f        # should print: koffing.local (for me)

getent hosts koffing.local
# If needed, add to /etc/hosts:
# 127.0.1.1   koffing.local koffing
{% endhighlight %}

## Create the KDC database

Now we initialize the database that will hold all of your principals, policies, realms, etc.

{% highlight plain %}
sudo mkdir -p /var/lib/krb5kdc
sudo kdb5_util create -s -r LOCAL
# set the KDC master password when prompted
{% endhighlight %}

Start the daemons:

{% highlight plain %}
sudo systemctl enable --now krb5-kdc krb5-admin-server
sudo systemctl status krb5-kdc krb5-admin-server --no-pager
{% endhighlight %}

## Add principals

Create an admin and a user:

{% highlight plain %}
sudo kadmin.local -q "addprinc admin/admin"
sudo kadmin.local -q "addprinc michael"
{% endhighlight %}

## Hello, Kerberos!

Now it's time to give this a quick test. You can get a ticket with the following:

{% highlight plain %}
kdestroy
kinit michael
klist
{% endhighlight %}

You should see something similar to the following:

{% highlight plain %}
Ticket cache: FILE:/tmp/krb5cc_1000
Default principal: michael@LOCAL

Valid starting     Expires            Service principal
13/09/25 16:14:32  14/09/25 02:14:32  krbtgt/LOCAL@LOCAL
	renew until 14/09/25 16:14:28
{% endhighlight %}

That’s your TGT — Kerberos is alive.

# Troubleshooting

Kerberos is famously unforgiving about typos and hostname mismatches. Here are some quick checks if things go sideways:

Check hostnames / FQDNs

{% highlight bash %}
hostname -f # should print koffing.local
getent hosts koffing.local
{% endhighlight %}

{% include callout.html type="warning" title="Hostnames!" text="If these don’t line up, Kerberos tickets won’t match the service principal name." %}

Check if the KDC is running

{% highlight bash %}
sudo systemctl status krb5-kdc krb5-admin-server --no-pager
{% endhighlight %}

Look at logs (Debian uses journalctl instead of flat log files):

{% highlight bash %}
sudo journalctl -u krb5-kdc -u krb5-admin-server -b --no-pager
{% endhighlight %}

Verbose kinit to see exactly what’s happening:

{% highlight bash %}
KRB5_TRACE=/dev/stderr kinit -V michael
{% endhighlight %}

This will show you which hostnames it resolves, which tickets it requests, and where it fails.

List all principals in the KDC database:

{% highlight bash %}
sudo kadmin.local -q "listprincs"
{% endhighlight %}

Clear your credential cache if tickets get stale:
{% highlight bash %}
kdestroy
{% endhighlight %}

The two most common pitfalls are:

* Hostname mismatch
* Realm mismatch (default realm not set in /etc/krb5.conf).

# SSO

So, we've got the proof of concept going, but it would be good to see this in action. What we'll cover in this next 
section is getting the `sshd` service to trust our Kerberos tickets. This will allow for passwordless SSH for the 
user.

## Add the host service principal and keytab

In order to get KDC to vouch for services, those services need principal definitions. A principal is **any** Kerberos 
identity. Users get user principals (as we saw above), services also need principals.

{% highlight bash %}
sudo kadmin.local -q "addprinc -randkey host/koffing.local"
{% endhighlight %}

For SSH on my virtual machine `koffing.local`, the conventional name is:

{% highlight plain %}
host/koffing.local@LOCAL
{% endhighlight %}

* The `host/` prefix is the standard for SSH, rsh, and other “host-based” services.
* The FQDN (`koffing.local`) must match what the client thinks it is connecting to.
* `@LOCAL` is your realm.

When a client does `ssh michael@koffing.local`, the SSH server needs to prove _“I really am host/koffing.local, trusted by the KDC.”_

Now we need a **keytab**. 

{% highlight bash %}
sudo kadmin.local -q "ktadd -k /etc/krb5.keytab host/koffing.local"
{% endhighlight %}

A **keytab** is a file that stores one or more Kerberos keys (like passwords, but in cryptographic form). Unlike users 
(who can type passwords into `kinit`), services can’t type passwords interactively. So the KDC generates a random key 
for `host/koffing.local@LOCAL` (`-randkey`) and you export it into `/etc/krb5.keytab` with `ktadd`.

Now sshd can silently use that keytab to decrypt tickets clients send it.

## Enable GSSAPI in sshd

The global `/etc/ssh/sshd_config` needs a couple of flags flicked. The SSH daemon doesn't implement Kerberos directly, 
so it uses the GSSAPI library functions provided by MIT Kerberos (or Heimdal) to handle ticket validation. **GSSAPI** 
isn't a protocol itself; it's an API or abstraction layer.

Once we've flipped these switches we are telling `sshd` _“Accept authentication from any GSSAPI mechanism. In practice, this means Kerberos tickets.”_.

{% highlight plain %}
# GSSAPI options
GSSAPIAuthentication yes
GSSAPICleanupCredentials yes
{% endhighlight %}

This setup is obviously done on any server that you want to do this SSO style login with. It's a bit confusing in my 
example here, because everything is on the one machine.

## Configure your SSH client

Conversely, we have configuration to do on the client side. For clients that want to connect with this type of 
authentication, the following settings are required in their `~/.ssh/config`:

{% highlight plain %}
Host koffing.local
  GSSAPIAuthentication yes
  GSSAPIDelegateCredentials yes
{% endhighlight %}

## Testing

{% highlight plain %}
kdestroy
kinit michael
ssh michael@koffing.local
{% endhighlight %}

If everything lines up, ssh should not prompt for a password. Your Kerberos TGT has been used to authenticate silently.

# Where Kerberos Fits

Kerberos is ideal for LAN-based authentication: it provides fast, passwordless single sign-on for services like SSH, 
Postgres, and intranet HTTP apps. But it isn’t designed for cross-organization web or mobile use.

Modern protocols like **OIDC** (OpenID Connect) build on OAuth 2.0 to provide authentication and federation across the 
public internet. They use signed tokens, redirect flows, and JSON-based metadata — making them better suited for SaaS, 
cloud apps, and mobile clients.

In short: **Kerberos is the right tool inside the castle walls; OIDC is the right tool when your users are everywhere.**

# Wrap-up

We've stood up a Kerberos realm (`LOCAL`), issued a TGT for a user (`michael`), and used it for passwordless SSH into 
the same box. That’s enough to demystify Kerberos: no secrets flying across the network, just short-lived tickets 
granted by a trusted KDC.

There's plenty more that we can accomplish here as we could create service principals for HTTP, Postgres, or 
cross-realm trust. 
