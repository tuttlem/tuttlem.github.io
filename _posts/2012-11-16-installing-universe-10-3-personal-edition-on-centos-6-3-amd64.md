---
layout: post
title: Installing UniVerse 10.3 (Personal Edition) on CentOS 6.3 amd64
date: 2012-11-16
comments: false
---

A guide for installing the <strong>UniVerse</strong> database system on a <strong>RedHat</strong> flavored distribution.

### Installation Pre-requisites

As the database server is being installed to a 64bit system, some 32bit libraries are required for the installation to succeed.

{% highlight bash %}
yum install glibc.i686 libstdc++.so.6 pam.i686
{% endhighlight %}

### Running the installer

{% highlight bash %}
# Unpack the database installation into a temporary location.
unzip uvpe_rhlinux_10.3.3.zip

# Extract the uv.load script out of the archive
cpio -ivcBdum uv.load < ./STARTUP

# Set the date on the system to satisfy licensing requirements
date -s 1/1/2009

# Run the uv.load script
./uv.load

{% endhighlight %}  

### Completing the installation

Select the first option from the first menu:  

{% highlight text %}
1) Make 'root' the default owner and administrator of uniVerse.  
The current installation continues uninterrupted.  
{% endhighlight %} 

On the second menu, change the rewind and no-rewind tape names to the location of the installation files.  

{% highlight text %}
4) Rewind tape name          /tmp/uvpe  
5) No-rewind tape name        /tmp/uvpe  
{% endhighlight %} 

### Starting the RPC daemon

Before attempting to start the RPC daemon from within the uv environment, an entry for the service itself needs to be added to the <em>/etc/services</em> file   

{% highlight bash %}
echo "uvrpc 31438/tcp # uvNet rpc port" >> /etc/services  
{% endhighlight %} 

### References

Installing UniVerse on CentOS 6.2<br /><a href="http://u2tech.wordpress.com/2012/07/08/installing-universe-centos-6-2/">http://u2tech.wordpress.com/2012/07/08/installing-universe-centos-6-2/</a>