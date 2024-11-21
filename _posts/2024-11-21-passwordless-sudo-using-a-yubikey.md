---
layout: post
title: Passwordless sudo using a YubiKey
date: 2024-11-21
comments: false
categories: ["YubiKey", "sudo"]
---

# Introduction

[YubiKeys](https://www.yubico.com/) are excellent multi-factor authentication (MFA) devices that can enhance your online security while simplifying your daily workflows on Linux. 

In this article, we’ll walk through the process of configuring a YubiKey for secure authentication, including setting up passwordless `sudo` or enabling two-factor authentication (2FA) for elevated privileges.

# Setup

## Prerequisites

First, ensure you have the `libpam-u2f` package (or its equivalent for your Linux distribution) installed. On Debian-based systems, use the following command:

{% highlight shell %}
sudo apt-get install libpam-u2f
{% endhighlight %}

[U2F](https://wiki.debian.org/Security/U2F) (Universal 2nd Factor) is an open standard for hardware MFA keys, and integration with Linux is made possible through [Yubico's pam-u2f module](https://github.com/Yubico/pam-u2f).

## Adding Your YubiKey

To link your YubiKey with your system, follow these steps:

1. **Connect your YubiKey**: Insert the device into your computer.

2. **Create the configuration directory**: If it doesn’t already exist, create the directory `~/.config/Yubico`:

   {% highlight shell %}
   mkdir -p ~/.config/Yubico
   {% endhighlight %}

3. **Register your YubiKey**: Add the key to the list of accepted devices by running:

   {% highlight shell %}
   pamu2fcfg > ~/.config/Yubico/u2f_keys
   {% endhighlight %}

   If you’ve set a PIN for your YubiKey, you may be prompted to enter it.

4. **Add additional keys (optional)**: If you have other YubiKeys, you can add them as follows:

   {% highlight shell %}
   pamu2fcfg -n >> ~/.config/Yubico/u2f_keys
   {% endhighlight %}

   Ensure there are **no extra newlines** between entries in the `~/.config/Yubico/u2f_keys` file.

## Configuring `sudo`

After setting up your key(s), you can configure `sudo` to use them for authentication.

### Enabling Passwordless `sudo`

To make `sudo` passwordless:

1. **Edit your `/etc/sudoers` file**: Add a line like this:

   {% highlight plain %}
   %wheel      ALL = (ALL) NOPASSWD: ALL
   {% endhighlight %}

   Ensure your user is part of the `wheel` group.

2. **Modify `/etc/pam.d/sudo`**: Add the following line **before** `@include common-auth`:

   {% highlight plain %}
   auth        sufficient      pam_u2f.so
   {% endhighlight %}

   This configuration makes YubiKey authentication sufficient for `sudo`, bypassing the need for a password.

### Enabling 2FA for `sudo`

To enable 2FA, where both your password and YubiKey are required:

1. **Edit `/etc/pam.d/sudo`**: Add the following line **after** `@include common-auth`:

   {% highlight plain %}
   auth        required        pam_u2f.so
   {% endhighlight %}

   This ensures the usual password authentication is followed by YubiKey verification.

# Troubleshooting

Before closing the terminal window where you’re editing `/etc/pam.d/sudo`, always confirm that your changes work as expected. 

### Enable Debugging

If something isn’t working, add `debug` to the `auth` line in `/etc/pam.d/sudo` to enable detailed logging during authentication:

{% highlight plain %}
auth        sufficient      pam_u2f.so debug
{% endhighlight %}

The additional logs can help identify configuration issues.

# Conclusion

Adding a YubiKey to your Linux authentication setup enhances security and can simplify your workflow by reducing the need to frequently enter passwords. Whether you choose passwordless authentication or 2FA, YubiKeys are a valuable tool for improving your overall security posture.
