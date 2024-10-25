---
layout: post
title: Setup Unattended Updates for Debian
date: 2024-10-20
comments: false
categories: [ "debian", "admin" ]
---

# Introduction

Keeping your Linux servers up to date with the latest security patches is critical. Fortunately, if you’re running a 
Debian-based distribution (like Debian or Ubuntu), you can easily automate this process using **unattended-upgrades**. In 
this guide, we’ll walk through setting up automatic patching with **unattended-upgrades**, configuring a schedule for 
automatic reboots after updates, and setting up **msmtp** to send email notifications from your local Unix mail account.

# Installation

The first step is to install **unattended-upgrades**, which will automatically install security (and optionally other) 
updates on your server. Here’s how to do it:

{% highlight shell %}
sudo apt-get update
sudo apt-get install unattended-upgrades apt-listchanges
{% endhighlight %}

After installation, you’ll want to enable **unattended-upgrades**:

{% highlight shell %}
sudo dpkg-reconfigure --priority=low unattended-upgrades
{% endhighlight %}

This will configure your server to automatically install security updates. However, you can customize the configuration 
to also include regular updates if you prefer.

# Configuration

By default, **unattended-upgrades** runs daily, but you can configure it further by adjusting the automatic reboot 
settings to ensure that your server reboots after installing updates when necessary.

## Automatic Updates

Edit the unattended-upgrades configuration file:

{% highlight shell %}
sudo vim /etc/apt/apt.conf.d/50unattended-upgrades
{% endhighlight %}

Make sure the file has the following settings to apply both security and regular updates:

{% highlight plain %}
Unattended-Upgrade::Allowed-Origins {
    "${distro_id}:${distro_codename}-security";
    "${distro_id}:${distro_codename}-updates";
};
{% endhighlight %}

## Automatic Reboots

You can also configure the server to automatically reboot after installing updates (useful when kernel updates require a 
reboot). To do this, add or modify the following lines in the same file:

{% highlight plain %}
Unattended-Upgrade::Automatic-Reboot "true";
Unattended-Upgrade::Automatic-Reboot-Time "02:00";
{% endhighlight %}

## Testing and Dry Runs

To give this a quick test, you can use the following:

{% highlight shell %}
# dry run the process
sudo unattended-upgrade --dry-run --verbose

# run the unattended upgrade immediately
sudo unattended-upgrade --verbose
{% endhighlight %}

# Email Notification

In the same file, you can simply add the email address that you'd like to notify:

{% highlight plain %}
Unattended-Upgrade::Mail "your-local-username@localhost";
{% endhighlight %}

You may need to configure your Debian machine to be able to send email.  For this, we’ll use **msmtp**, which can relay 
emails. I use gmail, but you can use any provider.

## Configuration

Open up the `/etc/msmtprc` file. For the password here, I needed to use an "App Password" from Google (specifically).

{% highlight plain %}
defaults
tls on
tls_trust_file /etc/ssl/certs/ca-certificates.crt
logfile /var/log/msmtp.log

account gmail
host smtp.gmail.com
port 587
auth on
user your-email@gmail.com
password your-password
from your-email@gmail.com

account default : gmail
{% endhighlight %}

## Default

You can set `msmtp` as your default by linking it as `sendmail`.

{% highlight shell %}
sudo ln -sf /usr/bin/msmtp /usr/sbin/sendmail
{% endhighlight %}

## Testing

Make sure your setup for email is working now by sending yourself a test message:

{% highlight shell %}
echo "Test email from msmtp" | msmtp your-local-username@localhost
{% endhighlight %}

# Conclusion

With **unattended-upgrades** and **msmtp** configured, your Debian-based servers will automatically stay up to date with 
security and software patches, and you'll receive email notifications whenever updates are applied. Automating patch 
management is crucial for maintaining the security and stability of your servers, and these simple tools make it easy to 
manage updates with minimal effort.

Happy patching!