---
layout: post
title: Making CIFS Shares available to Docker
date: 2024-10-17
comments: false
categories: ["cifs", "debian", "docker", "linux"]
---

# Introduction

Mounting CIFS (SMB) shares in Linux can be a convenient way to access network resources as part of the local filesystem. 
In this guide, I’ll walk you through the steps for properly configuring a CIFS share in `/etc/fstab` on a Linux system. 
I’ll also show you how to ensure that network mounts are available before services like Docker start up.

# Step 1: Modify `/etc/fstab`

To mount a CIFS share automatically at boot, we need to modify the `/etc/fstab` file. First, open it in a text editor:

{% highlight shell %}
sudo vim /etc/fstab
{% endhighlight %}

Now, add or modify the CIFS entry in the file. A typical CIFS entry looks like this:

{% highlight shell %}
# Example CIFS line in fstab
//server_address/share_name /local/mount/point cifs credentials=/path/to/credentials,file_mode=0755,dir_mode=0755,uid=1000,gid=1000,_netdev 0 0
{% endhighlight %}

## Explanation:
- `//server_address/share_name`: The remote server and share you want to mount (e.g., `//192.168.1.100/shared`).
- `/local/mount/point`: The local directory where the share will be mounted.
- `cifs`: The filesystem type for CIFS/SMB.
- `credentials=/path/to/credentials`: Points to a file containing your username and password (this is optional, but recommended for security).
- `file_mode=0755,dir_mode=0755`: Sets the file and directory permissions for the mounted share.
- `uid=1000,gid=1000`: Specifies the user and group IDs that should own the files (replace `1000` with your user/group IDs).
- `_netdev`: Ensures that the mount waits for network availability before mounting.
- `0 0`: The last two values are for dump and fsck; they can usually remain `0`.

# Step 2: Create a Credentials File

For better security, you can use a separate credentials file rather than hard-coding the username and password in `/etc/fstab`. To do this, create a file to store the username and password for the share:

{% highlight shell %}
sudo nano /path/to/credentials
{% endhighlight %}

Add the following lines to the file:

{% highlight shell %}
username=your_username
password=your_password
domain=your_domain   # (optional, if you're in a domain environment)
{% endhighlight %}

Make sure the credentials file is secure by setting appropriate permissions:

{% highlight shell %}
sudo chmod 600 /path/to/credentials
{% endhighlight %}

This ensures only the root user can read the file, which helps protect sensitive information.

# Step 3: Test the Mount

After adding the CIFS line to `/etc/fstab` and configuring the credentials file, it’s time to test the mount. You can do this by running:

{% highlight shell %}
sudo mount -a
{% endhighlight %}

If everything is configured correctly, the CIFS share should mount automatically. If you encounter any issues, check the system logs for errors. 
Use one of these commands to inspect the logs:

{% highlight shell %}
# On Ubuntu or Debian-based systems
sudo tail /var/log/syslog

# On CentOS or RHEL-based systems
sudo tail /var/log/messages
{% endhighlight %}

# Ensuring Mounts are Available Before Docker

If you're running Docker on the same system and need to ensure that your CIFS mounts are available before Docker starts, you'll want to modify 
Docker’s systemd service. Here’s how:

First, create a directory for Docker service overrides:

{% highlight shell %}
sudo mkdir -p /etc/systemd/system/docker.service.d
{% endhighlight %}

Next, create a custom override file:

{% highlight shell %}
sudo vim /etc/systemd/system/docker.service.d/override.conf
{% endhighlight %}

Add the following content:

{% highlight shell %}
[Unit]
After=remote-fs.target
Requires=remote-fs.target
{% endhighlight %}

This configuration ensures Docker waits until all remote filesystems (like CIFS) are mounted before starting.

Finally, reload the systemd configuration and restart Docker:

{% highlight shell %}
sudo systemctl daemon-reload
sudo systemctl enable docker
sudo systemctl restart docker
{% endhighlight %}

Now, Docker will wait for your CIFS mounts to be available before starting any containers that might rely on them.

By following these steps, you can ensure your CIFS shares are mounted reliably on boot and integrated seamlessly with other services like Docker. 
This is especially useful for network-based resources that are critical to your containers or other local services.

