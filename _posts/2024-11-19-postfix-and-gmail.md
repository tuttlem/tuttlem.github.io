---
layout: post
title: Setting Up Postfix to Send Email Through Gmail
date: 2024-11-19
comments: false
categories: [ "smtp", "postfix", "admin" ]
---

# Introduction

Setting up email notifications is a critical task in homelab management. Services like Proxmox and OPNsense benefit 
greatly from reliable email alerts for updates, backups, or critical events. Configuring Postfix to send emails through 
Gmail provides a straightforward and secure solution. This guide will walk you through the steps to set up Postfix on a 
Linux system to relay emails using Gmail's SMTP server.

# Prerequisites

Before you begin, ensure the following:

1. A Linux system with Postfix installed.
2. A Gmail account with an app password enabled (explained below).
3. Basic terminal access and permissions to edit configuration files.

## Why Use an App Password?

Google enforces stricter security measures for less secure apps. You'll need to generate an **app password** 
specifically for Postfix:

1. Log in to your Google account.
2. Go to **Manage Your Google Account** > **Security**.
3. Under **Signing in to Google**, enable **2-Step Verification** if not already enabled.
4. Once 2-Step Verification is active, return to the **Security** page and find **App Passwords**.
5. Create a new app password for "Mail" or "Other" and note it down for later.

## Step 1: Install Postfix

If Postfix is not already installed, install it using your distribution's package manager. For example:

{% highlight bash %}
sudo apt update
sudo apt install postfix -y
{% endhighlight %}

During installation, choose "Internet Site" when prompted, and set the system mail name (e.g., yourdomain.com).

## Step 2: Configure Postfix for Gmail SMTP

Edit the Postfix configuration file to use Gmail as the relay host. Open `/etc/postfix/main.cf` in your preferred text 
editor and fill out the following:

{% highlight plain %}
relayhost = [smtp.gmail.com]:587
smtp_use_tls = yes
smtp_sasl_auth_enable = yes
smtp_sasl_security_options = noanonymous
smtp_sasl_password_maps = hash:/etc/postfix/sasl_passwd
smtp_tls_CAfile = /etc/ssl/certs/ca-certificates.crt
{% endhighlight %}

### Create the Password File

Create a password file at `/etc/postfix/sasl_passwd` to store your Gmail credentials:

{% highlight plain %}
[smtp.gmail.com]:587 your-email@gmail.com:your-app-password
{% endhighlight %}

Replace `your-email@gmail.com` with your Gmail address and `your-app-password` with the app password generated earlier.

### Secure and Hash the Password File

Secure the file permissions, and hash the file using `postmap`:

{% highlight shell %}
sudo chmod 600 /etc/postfix/sasl_passwd
sudo postmap /etc/postfix/sasl_passwd
{% endhighlight %}

## Step 3: Restart Postfix

Restart the postfix service on your linux machine.

{% highlight shell %}
sudo systemctl restart postfix
{% endhighlight %}

## Step 4: Test the Setup

Test your configuration by sending a test email. Use the mail command or another mail client installed on your system:

{% highlight shell %}
echo "This is a test email." | mail -s "Test Email" recipient@example.com
{% endhighlight %}

Chcek the logs if the email fails to send:

{% highlight shell %}
sudo tail -f /var/log/mail.log
{% endhighlight %}

## Step 5: Troubleshooting Common Issues

* **Authentication errors**: Double-check the app password and ensure `sasl_passwd` is correctly hashed.
* **Firewall blocking SMTP**: Ensure ports `587` (for Gmail SMTP) are open.
* **Incorrect CA certificates**: Ensure `/etc/ssl/certs/ca-certificates.crt` exists or specify a valid CA file.

# Conclusion

Configuring Postfix to send emails through Gmail enables reliable email notifications for your homelab services.