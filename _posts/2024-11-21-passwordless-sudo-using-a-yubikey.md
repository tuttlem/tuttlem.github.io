---
layout: post
title: Passwordless sudo using a YubiKey
date: 2024-11-21
comments: false
categories: ["YubiKey", "sudo"]
---

# Introduction

[YubiKeys](https://www.yubico.com/) are excellent multi-factor authentication (MFA) devices that can enhance your online security while simplifying your daily workflows on Linux. 

In this article, we’ll walk through the process of configuring a YubiKey for secure authentication including: 

* Setting up passwordless `sudo` or enabling (2FA) for elevated privileges
* Setting up 2FA on your Desktop Environment's login
* Setting up 2FA on your system's TTY login
* Setting up passwordless graphical prompts for elevated privileges

# Setup

## Prerequisites

First, ensure you have the `libpam-u2f` package (or its equivalent for your Linux distribution) installed. On Debian-based systems, use the following command:

{% highlight shell %}
sudo apt-get install libpam-u2f
{% endhighlight %}

[U2F](https://wiki.debian.org/Security/U2F) (Universal 2nd Factor) is an open standard for hardware MFA keys, and integration with Linux is made possible through [Yubico's pam-u2f module](https://github.com/Yubico/pam-u2f).

## Adding Your YubiKey

To link your YubiKey with your system, follow these steps:

* **Connect your YubiKey**: Insert the device into your computer.

* **Create the configuration directory**: If it doesn’t already exist, create the directory `~/.config/Yubico`:

{% highlight shell %}
mkdir -p ~/.config/Yubico
{% endhighlight %}

* **Register your YubiKey**: Add the key to the list of accepted devices by running:

{% highlight shell %}
pamu2fcfg > ~/.config/Yubico/u2f_keys
{% endhighlight %}

If you’ve set a PIN for your YubiKey, you may be prompted to enter it.

* **Add additional keys (optional)**: If you have other YubiKeys, you can add them as follows:

{% highlight shell %}
pamu2fcfg -n >> ~/.config/Yubico/u2f_keys
{% endhighlight %}

Ensure there are **no extra newlines** between entries in the `~/.config/Yubico/u2f_keys` file.

## Before you start!

Before you start re-configuring things, it's worth opening another terminal that is running as `root`. This way if you 
do make any mistakes, you can still use that root terminal to back-out any changes that haven't gone to plan.

Open a new terminal, and issue the following:

{% highlight shell %}
sudo -i
{% endhighlight %}

Now leave that terminal running in the background.

## Configuring `sudo`

After setting up your key(s), you can configure `sudo` to use them for authentication.

### Enabling Passwordless `sudo`

To make `sudo` passwordless:

* **Edit your `/etc/sudoers` file**: Add a line like this:

{% highlight plain %}
%wheel      ALL = (ALL) NOPASSWD: ALL
{% endhighlight %}

Ensure your user is part of the `wheel` group.

* **Modify `/etc/pam.d/sudo`**: Add the following line **before** `@include common-auth`:

{% highlight plain %}
auth        sufficient      pam_u2f.so cue [cue_prompt=Tap your key]
{% endhighlight %}

This configuration makes YubiKey authentication sufficient for `sudo`, bypassing the need for a password.

### Enabling 2FA for `sudo`

To enable 2FA, where both your password and YubiKey are required:

* **Edit `/etc/pam.d/sudo`**: Add the following line **after** `@include common-auth`:

{% highlight plain %}
auth        required        pam_u2f.so cue [cue_prompt=Tap your key]
{% endhighlight %}

This ensures the usual password authentication is followed by YubiKey verification.

## Configuring 2FA for your Display Manager

I'm running [KDE](https://kde.org/) on this particular machine.

* **Edit `/etc/pam.d/kde`**: Add the `pam_u2f.so` reference:

{% highlight plain %}
#%PAM-1.0

auth       include                     system-local-login
auth       required                    pam_u2f.so cue [cue_prompt=Tap your key]

account    include                     system-local-login

password   include                     system-local-login

session    include                     system-local-login
{% endhighlight %}

You should be able to do the same with [GDM](https://en.wikipedia.org/wiki/GNOME_Display_Manager), etc.

## Configuring 2FA for TTY

When you change virtual TTY and go to login, we can also require a 2FA token at this point.

* **Edit `/etc/pam.d/login`**: Add the `pam_u2f.so` reference:

{% highight plain %}
#%PAM-1.0

auth       requisite    pam_nologin.so
auth       include      system-local-login
auth       required     pam_u2f.so cue [cue_prompt=Tap your key]
account    include      system-local-login
session    include      system-local-login
password   include      system-local-login
{% endhighlight %}

## Configuring Passwordless `polkit`

The graphical prompts that you see throughout your desktop environment session are controlled using [polkit](https://wiki.archlinux.org/title/Polkit).

Much like the passwordless configuration for `sudo` above, we can control `polkit` in the same way.

* **Edit `/etc/pam.d/polkit-1`**: Add the `pam_u2f.so` reference:

{% highlight plain %}
#%PAM-1.0

auth            sufficient      pam_u2f.so cue [cue_prompt=Tap your key]

auth            required        pam_env.so
auth            required        pam_deny.so

auth            include         system-auth
account         include         system-auth
password        include         system-auth
session         include         system-auth
{% endhighlight %}

# Troubleshooting

Always keep in mind that you have that terminal sat in the background. That terminal can get you out of all sorts of trouble
so that you can rewind any changes that you've made that might have broken authentication on your system.

## Enable Debugging

If something isn’t working, add `debug` to the `auth` line in `/etc/pam.d/sudo` to enable detailed logging during authentication:

{% highlight plain %}
auth        sufficient      pam_u2f.so debug
{% endhighlight %}

The additional logs can help identify configuration issues.

# Conclusion

Adding a YubiKey to your Linux authentication setup enhances security and can simplify your workflow by reducing the need to frequently enter passwords. Whether you choose passwordless authentication or 2FA, YubiKeys are a valuable tool for improving your overall security posture.
