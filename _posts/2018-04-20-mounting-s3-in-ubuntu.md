---
layout: post
title: Mounting S3 in Ubuntu
date: 2018-04-19
comments: false
categories: [ "linux", "s3" ]
---

Getting the [s3](https://aws.amazon.com/s3/) storage mechanism can be easily integrated into your local linux file system using the [s3fs](https://github.com/s3fs-fuse/s3fs-fuse) project.

Install the [s3fs](https://packages.ubuntu.com/xenial/s3fs) package as usual:

{% highlight bash %}
sudo apt-get install s3fs
{% endhighlight %}

Configure authentication using a home-folder credential file called `.passwd-s3fs`. This file expects data in the format of `IDENTITY:CREDENTIAL`. You can easily create one of these with the following:

{% highlight bash %}
echo MYIDENTITY:MYCREDENTIAL >  ~/.passwd-s3fs
chmod 600  ~/.passwd-s3fs
{% endhighlight %}

Finally, mount your S3 bucket into the local file system:

{% highlight bash %}
s3fs your-bucket-name /your/local/folder -o passwd_file=/home/michael/.passwd-s3fs
{% endhighlight %}

That't it. You can now use S3 data, just as you would local data on your system.
