---
layout: post
title: FreeBSD 9.0 installation on a T400
date: 2012-11-15
comments: false
categories: ["Installation", "FreeBSD", "T400", "Lenovo", "Guide"]
---

From time to time, FreeBSD has made its way onto my T400. Each time I do it, I never jot down what I did to get it to "work". Here's just a few thoughts and basic guide for installing FreeBSD RELEASE-9.0 on a Lenovo ThinkPad T400.

### BIOS
The T400's graphics mode needs to be switched from "Discreet" back to "Integrated", otherwise the BSD installation process just continually reboots. The laptop will now allow a standard installation to be performed.

### Post Installation
First things first. Stop the PC Speaker barking at you when you do something questionable (disable by adding this to <em>/etc/sysctl.conf</em>)

{% highlight text %}
hw.syscons.bell=0
{% endhighlight %}

Some switches required for connection resilience need to be applied to the WiFi configuration (fix by commenting these lines from <em>/etc/wpa_supplicant.conf</em>)  

{% highlight text %}
# eapol_version=2  
# ap_scan=1  
# fast_reauth=1  
{% endhighlight %}

WiFi network entry should look as follows  

{% highlight text %}
network={  
	ssid="tuttle"  
	key_mgmt=WPA-PSK  
 	psk="secret-in-here"
}  
{% endhighlight %}

Add the full hostname to <em>/etc/rc.conf</em>  

{% highlight text %}
hostname="thor.local"  
{% endhighlight %}

Get the proc filesystem mounted at boot by adding the following line to <em>/etc/fstab</em>  

{% highlight text %}
proc   /proc  procfs  rw  0   0  
{% endhighlight %}

### Organising the ports collection

In the interest of making the ports collection as "fresh" as possible, it's best to remove the installed version and pull down the latest version over the web.

Remove the existing ports collection

{% highlight bash %}
cd /usr/ports
rm -Rf *  
{% endhighlight %}

Refresh the ports collection from an australian mirror. You can find full instructions <a href="http://www.freebsd.org/doc/handbook/ports-using.html">here</a> on this.   <strong>Fetch</strong> the collection into /var/db/portsnap <strong>Extract</strong> the collection into /usr/ports    

{% highlight bash %}
portsnap fetch
portsnap extract
{% endhighlight %}

From now on, to refresh ports just use the following command 

{% highlight bash %}
portsnap update
{% endhighlight %}

### Installation

Any port needed for installation is done with the following commands  

{% highlight bash %}
cd /usr/ports/path/to/port  
make install clean  
{% endhighlight %}

<strong>bash</strong> (<em>/usr/ports/shells/bash</em>)  

Set bash as the default shell for users by running the following  

{% highlight bash %}
pw usermod user_name -s /path/to/bash  
{% endhighlight %}

<strong>sudo</strong> (<em>/usr/ports/security/sudo</em>)  

Setup user groups who can sudo by editing the config  

{% highlight bash %}
visudo -f /usr/local/etc/sudoers  
{% endhighlight %}

Uncomment the line allowing the wheel group  

{% highlight text %}
%wheel ALL=(ALL) ALL  
{% endhighlight %}

Add users who want to sudo to the wheel group  

{% highlight bash %}
pw usermod user_name -G wheel  
{% endhighlight %}

Other pieces of software to make your environment feel more like home

<strong>vim</strong> (<em>/usr/ports/editors/vim-lite</em>) <br />
<strong>git</strong> (<em>/usr/ports/devel/git</em>)  <br />
<strong>xorg</strong> (<em>/usr/ports/x11/xorg</em>)  <br />
<strong>xfce</strong> (<em>/usr/ports/wm/xfce4</em>)  <br />
<strong>gnome-keyring</strong> (<em>/usr/ports/security/gnome-keyring</em>)  <br />
<strong>gnome-power-manager</strong> (<em>/usr/ports/sysutils/gnome-power-manager</em>)  <br />
<strong>chrome</strong> (<em>/usr/ports/www/chromium</em>)  <br />
<strong>pidgin</strong> (<em>/usr/ports/net-im/pidgin</em>)  <br />
<strong>irssi</strong> (<em>/usr/ports/irc/irssi</em>)  <br />
<strong>evince</strong> (<em>/usr/ports/graphics/evince</em>) <br />

### Preferences

Set colourised output on ls by adding the following line to <em>~/.bash_profile</em> 

{% highlight bash %}
alias ls='ls -G'
{% endhighlight %}

Get bash_profile to execute bashrc if it exists by adding the following line towards the top to <em>bash_profile</em> 

{% highlight bash %}
test -f ~/.bashrc && . ~/.bashrc  
{% endhighlight %}

<strong>dbus</strong> and <strong>hald</strong> need to be started at boot time if a mouse/keyboard is going to be used within X. To get these to start, add the following lines to the /etc/rc.conf file  

{% highlight text %}
dbus_enable="YES"
hald_enable="YES" 
{% endhighlight %}

Both of the above config lines can be removed if using a login manager (like <strong>gdm</strong>). All the needs to be added in this scenario is  

{% highlight text %}
gnome_enable="YES"  
{% endhighlight %}

Automatically mount any samba shares at boot by adding lines like the following to <em>/etc/fstab</em> </span>

{% highlight text %}
>> //user@server/share  /mountpoint  smbfs  rw,-N,-Iserver  0  0  
{% endhighlight %}

With -N specified in the above fstab entry, any secrets needed to connect to the share need to be put into the <em>~/.nsmbrc</em> file and should look as follows  

{% highlight text %}
[SERVER:USER]  
password=secret  
{% endhighlight %}

<strong>Tread with caution here:</strong> I've just rolled back authentication for the samba shares as FreeBSD's mount_smbfs doesn't contain a _netdev option which will delay mounting the remote share until the network is available. Not being able to do this makes the BSD boot fail with authentication errors.  

### Shutdown  

{% highlight bash %}
sudo shutdown -h now  
{% endhighlight %}