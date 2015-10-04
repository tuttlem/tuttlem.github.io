---
layout: post
title: SSH key setup for remote login
date: 2015-10-05
comments: false
categories: [ "ssh", "login" ]
---

Setting up passphrase-less login to your SSH servers is a convenient way of logging into your servers without being annoyed for a passphrase. In today's post, I'll take you through generating a key, distributing your identity and logging on.

### Generating your key

If you haven't done so already, you'll need to generate some authentication keys for yourself. You can do this with [ssh-keygen](http://linux.die.net/man/1/ssh-keygen).

{% highlight bash %}
ssh-keygen -t rsa
{% endhighlight %}

The output of which will look like this:

{% highlight text %}
root@64ed9b1beed9:~# ssh-keygen -t rsa
Generating public/private rsa key pair.
Enter file in which to save the key (/root/.ssh/id_rsa):
Created directory '/root/.ssh'.
Enter passphrase (empty for no passphrase):
Enter same passphrase again:
Your identification has been saved in /root/.ssh/id_rsa.
Your public key has been saved in /root/.ssh/id_rsa.pub.
The key fingerprint is:
b6:ff:5f:1b:88:12:7e:3d:5f:28:c9:66:fb:a7:21:6a root@64ed9b1beed9
The key's randomart image is:
+--[ RSA 2048]----+
|                 |
|                 |
|                 |
|                 |
|        S.       |
|       .....o... |
|        .o oB+o.o|
|         .E+ +oo=|
|         .o.oo+= |
+-----------------+
{% endhighlight %}

Now that this process has completed, you're given a public key and private secret key in your `.ssh/` folder.

### Distribute your identity

To deploy your public key to other servers so that you can authenticate using your private key, you can use [ssh-copy-id](http://linux.die.net/man/1/ssh-copy-id).

{% highlight bash %}
$ ssh-copy-id remote-user@remote-host
{% endhighlight %}

You'll want to swap out `remote-user` for the user that you're associating your key to and `remote-host` with the machine that you want to connect to.

Another way that you can establish your key into the remote machine's authorized set is as follows:

{% highlight bash %}
cat ~/.ssh/id_rsa.pub | ssh example@123.123.123.123 "mkdir -p ~/.ssh && cat >>  ~/.ssh/authorized_keys"
{% endhighlight %}

You'll then be taken through the verification process, which is just supplying your remote password:

{% highlight text %}
The authenticity of host '123.123.123.123 (123.123.123.123)' can't be established.
ECDSA key fingerprint is ae:2d:33:79:e9:d8:03:16:6c:17:d3:f2:7e:c4:05:60.
Are you sure you want to continue connecting (yes/no)? yes
/usr/bin/ssh-copy-id: INFO: attempting to log in with the new key(s), to filter out any that are already installed
/usr/bin/ssh-copy-id: INFO: 1 key(s) remain to be installed -- if you are prompted now it is to install the new keys
example@123.123.123.123's password:

Number of key(s) added: 1

Now try logging into the machine, with:   "ssh 'example@123.123.123.123'"
and check to make sure that only the key(s) you wanted were added.

{% endhighlight %}

### Conclusion

You're free to login. Of course, if you don't set a pass phrase for your keys you won't be hassled all the time to unlock them. If you do set a pass phrase, your overall solution will be just a bit more secure.

