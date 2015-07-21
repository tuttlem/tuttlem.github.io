---
layout: post
title: PhoneGap Setup on Arch Linux
date: 2014-08-29
comments: false
categories: ["phonegap", "cordova", "arch", "linux"]
---

Here's a few notes to getting [PhoneGap](http://phonegap.com/) up and running on an [Arch Linix](https://www.archlinux.org/) installation.

### Dependencies

PhoneGap itself relies on some java tools, so you'll need a jdk and ant.

{% highlight bash %}
$ sudo pacman -S jdk7-openjdk
$ sudo pacman -S apache-ant
{% endhighlight %}

In order to run your applications in an android simulator, you'll need the android sdk installed. For the next steps, you'll need to ensure that `multilib` is enabled in your `/etc/pacman.conf` file.

You'll need the following packages installed from [AUR](https://aur.archlinux.org/):

* [android-sdk](https://aur.archlinux.org/packages/android-sdk/)
* [android-sdk-platform-tools](https://aur.archlinux.org/packages/android-sdk-platform-tools/)
* [android-sdk-build-tools](https://aur.archlinux.org/packages/android-sdk-build-tools/)
* [android-platform](https://aur.archlinux.org/packages/android-platform/)

After these have been successfully installed, using the suggested [installation procedure](https://wiki.archlinux.org/index.php/Arch_User_Repository#Build_the_package) guidance on the wiki, you'll need to put these tools on your path:

{% highlight bash %}
$ export PATH=$PATH:/opt/android-sdk/tools:/opt/android-sdk/platform-tools:/opt/android-sdk/build-tools
{% endhighlight %}

PhoneGap is installed using `npm` which is part of the NodeJS suite, so you'll need to have it installed as well:

{% highlight bash %}
$ sudo pacman -S nodejs
{% endhighlight %}

### Installation

From the [PhoneGap installation guide](http://phonegap.com/install/), installation should be just:

{% highlight bash %}
$ sudo npm install -g phonegap
{% endhighlight %}

### Device Setup

Before you can run any applications, you'll need to setup a device. No cpu images are installed by default, so you'll need to install these first using the `android` command.

After installing the appropriate images, you can create a device using `android` `avd`.


