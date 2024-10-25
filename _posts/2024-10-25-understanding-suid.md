---
layout: post
title: Understanding SUID
date: 2024-10-25
comments: false
categories: [ "security" ]
---

# Introduction

In the world of Linux security, SUID (Set User ID) is a powerful but potentially dangerous feature that controls 
privilege escalation. This article will walk through how SUID works, illustrate its effects with a C program example, 
and explore how improper handling of SUID binaries can lead to privilege escalation.

# What is SUID?

SUID, or Set User ID, is a special permission flag in Unix-like operating systems that allows a user to execute a file 
with the permissions of the file’s owner, rather than their own. This is particularly useful when certain tasks require 
elevated privileges. For example, the `passwd` command uses SUID to allow any user to change their password, though the 
actual file manipulations need root access.

SUID permission can be set using the `chmod` command with the octal value `4` in front of the file permissions. A SUID 
binary might look something like this in a directory listing:

{% highlight plain %}
-rwsr-xr-x 1 root    root      16000 Oct 25 21:37 suid_binary
{% endhighlight %}

The `s` in the permission string indicates that the SUID bit is set.

# Finding SUID Binaries

SUID binaries can be located with the `find` command. This is useful both for security auditing and for understanding 
which executables can perform actions with elevated privileges.

{% highlight shell %}
find / -perm -u=s -type f 2>/dev/null
{% endhighlight %}

This command searches the entire filesystem for files that have the SUID bit set. Be cautious with these binaries, as 
any misconfiguration can expose the system to privilege escalation.

# Building a Program to Understand SUID

Let’s construct a simple C program to see the effects of SUID in action and learn how real and effective user IDs (UIDs) 
behave.

Here’s our initial program, which will print both the Real UID (RUID) and the Effective UID (EUID). These IDs help 
determine the permissions available during program execution:

{% highlight c %}
#include <stdio.h>
#include <unistd.h>

int main() {
    printf("Real UID     : %d\n", getuid());
    printf("Effective UID: %d\n", geteuid());

    return 0;
}
{% endhighlight %}

To compile the program, use:

{% highlight shell %}
gcc suid_example.c -o suid_example
{% endhighlight %}

On the first run of this program, it'll pick up the same id (the current executing user) for both the real and 
effective UID:

{% highlight plain %}
$ ./suid_example

Real UID     : 1000
Effective UID: 1000
{% endhighlight %}

We can escalate these privileges here through the use of `sudo`:

{% highlight plain %}
sudo ./suid_example
Real UID     : 0
Effective UID: 0 
{% endhighlight %}

Using `sudo` is cheating though. We want to demonstrate SUID.

## Adding SUID

We'll set the program's SUID bit so it can be run with elevated privileges:

{% highlight shell %}
sudo chown root:root suid_example
sudo chmod 4755 suid_example
{% endhighlight %}

If we re-run this program now, our real and effective UIDs are different:

{% highlight plain %}
$ ./suid_example

Real UID     : 1000
Effective UID: 0
{% endhighlight %}

Now, our Effective UID (EUID) is `0`, meaning we have root privileges, while the Real UID (RUID) remains our original 
user ID.

## Adding setuid

Calling `setuid(0)` explicitly sets the Real UID and Effective UID to `0`, making the user a superuser. This step is 
often necessary to maintain root access throughout the program execution.

{% highlight c %}
#include <stdio.h>
#include <unistd.h>

int main() {
    printf(" ---- before ---- \n");
    printf("Real UID     : %d\n", getuid());
    printf("Effective UID: %d\n", geteuid());

    setuid(0);

    printf(" ---- after ---- \n");
    printf("Real UID     : %d\n", getuid());
    printf("Effective UID: %d\n", geteuid());

    return 0;
}
{% endhighlight %}

Now that we have `setuid` in place, executing this program as our standard (`1000`) user gives us this result:

{% highlight plain %}
$ ./suid_example

 ---- before ---- 
Real UID     : 1000
Effective UID: 0
 ---- after ---- 
Real UID     : 0
Effective UID: 0
{% endhighlight %}

With this call, both the Real and Effective UID will be set to `0`, ensuring root-level privileges throughout 
the execution.

# Security Implications of SUID

SUID binaries, when not managed carefully, can introduce security vulnerabilities. Attackers can exploit misconfigured 
SUID programs to gain unauthorized root access. A few best practices include:

1. **Minimizing SUID Binaries**: Only use SUID where absolutely necessary, and regularly audit the system for SUID binaries.
2. **Code Review**: Ensure that all SUID programs are thoroughly reviewed for security vulnerabilities, particularly around system calls like system(), which could potentially be hijacked.

# Conclusion

In this post, we explored how SUID works, implemented a program to observe its effects on Real and Effective UIDs, and 
demonstrated the power of privilege escalation. While SUID is a useful tool for certain applications, it must be 
carefully managed to avoid security risks. By understanding SUID, Linux administrators and developers can better protect 
their systems against privilege escalation attacks.
