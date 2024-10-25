---
layout: post
title: Privilege Escalation Techniques
date: 2024-10-25
comments: false
categories: [ "security" ]
---

# Introduction

Privilege escalation is a critical concept in cybersecurity, allowing an attacker to gain higher-level access to systems 
by exploiting specific weaknesses. This process often enables adversaries to move from limited user roles to more 
powerful administrative or root-level access. In this article, we'll dive into several common privilege escalation 
techniques on both Linux and Windows systems, covering methods such as exploiting SUID binaries, weak permissions, 
and kernel vulnerabilities.

# Privilege Escalation

Privilege escalation attacks typically fall into two categories:

1. **Vertical Privilege Escalation:** This occurs when a user with lower privileges (e.g., a standard user) gains access to higher privileges (e.g., an admin or root level).
2. **Horizontal Privilege Escalation:** In this case, an attacker remains at the same privilege level but accesses resources or areas they typically shouldn’t have access to.

This article focuses on vertical privilege escalation techniques on Linux and Windows systems.

## Linux

### Exploiting SUID Binaries

In Linux, binaries with the **SUID** (Set User ID) bit set run with the privileges of the file owner rather than the 
user executing them. A misconfigured SUID binary owned by `root` can be exploited to execute code with root privileges.

To locate SUID binaries, use:

{% highlight bash %}
find / -perm -u=s -type f 2>/dev/null
{% endhighlight %}

Once located, inspect the binary for potential exploitation. Some known binaries like `find`, `vim`, or `perl` can often 
be exploited with SUID if configured incorrectly. For instance:

{% highlight bash %}
# Exploiting a SUID binary with `find`
find . -exec /bin/sh -p \; -quit
{% endhighlight %}

### Weak File Permissions

Misconfigured permissions can lead to privilege escalation when files essential to the system or owned by 
higher-privilege users are writable by lower-privilege accounts.

As an example, if an attacker can write to `/etc/passwd`, they can add a new user with root privileges:

{% highlight bash %}
echo 'backdoor:x:0:0::/root:/bin/bash' >> /etc/passwd
{% endhighlight %}

Alternatively, a writable `/etc/shadow` file can enable password manipulation for privileged users.

### Kernel Exploits

Linux kernel vulnerabilities are a frequent target for privilege escalation, especially in environments where patching 
is delayed. It is critical to remain patched and up to day, as well as to keep looking at 
[exploit registers](https://www.cvedetails.com/) to stay ahead.

### Cron Jobs and PATH Exploits

If cron jobs are running scripts with elevated privileges and the script location or PATH variable is misconfigured, 
attackers may be able to manipulate the outcome.

For instance, if a cron job executes a script owned by root from `/tmp`, an attacker can replace or edit this script to 
run commands with root privileges.

### Exploiting Misconfigured Capabilities

Linux capabilities allow fine-grained control of specific root privileges for binaries. For instance, a binary with 
`CAP_SETUID` capability can change user IDs without full root access. Misconfigured capabilities can be listed with:

{% highlight bash %}
getcap -r / 2>/dev/null
{% endhighlight %}

## Windows

### Misconfigured Service Permissions

In Windows, services running as SYSTEM or Administrator can be exploited if lower-privilege users have permission to modify them.

To enumerate services with exploitable permissions, use PowerShell:

{% highlight plain %}
Get-Service | Where-Object {$_.StartType -eq 'Automatic'}
{% endhighlight %}

Tools like **AccessChk** can also help determine whether services are misconfigured:

{% highlight plain %}
accesschk.exe -uwcqv "username" *
{% endhighlight %}

If a service is found to be modifiable, an attacker could replace the executable path with a malicious file to run as SYSTEM.

### DLL Hijacking

Windows programs often load DLLs from specific directories in a defined order. If a high-privilege process loads a DLL 
from a directory where an attacker has write access, they can place a malicious DLL in that directory to achieve code 
execution at a higher privilege level.

To locate DLL loading paths, analyze process dependencies with tools like **Process Monitor**.

### Weak Folder Permissions

Folder permissions can escalate privileges if users can write to directories containing executables or scripts used by 
high-privilege processes.

An attacker could replace a legitimate executable in a writable directory to execute malicious code. Check for writable 
directories in the PATH:

{% highlight plain %}
icacls "C:\path\to\directory"
{% endhighlight %}

### Token Impersonation

In Windows, processes running as SYSTEM can often create impersonation tokens, allowing privileged processes to 
temporarily “impersonate” another user. Attackers can exploit tokens left by privileged processes to escalate privileges 
using tools like Incognito or PowerShell.

For instance, PowerShell can be used to list tokens available:

{% highlight plain %}
whoami /priv
{% endhighlight %}

### Kernel Vulnerabilities

In the same way as Linux, Windows will also have kernel exploits that come up on the register. Make sure you're always 
patched and on top of the latest issues.

# Conclusion

Privilege escalation is a critical step in many cyberattacks, allowing attackers to move from restricted to privileged 
roles on a system. For both Linux and Windows, attackers leverage vulnerabilities in service configurations, 
permissions, and system processes to achieve this goal. Security professionals must stay informed about these techniques 
and patch, configure, and monitor systems to defend against them.

Regularly auditing permissions, keeping software up-to-date, and minimizing the attack surface are essential to 
mitigating privilege escalation risks. By understanding and addressing these common methods, organizations can 
significantly reduce the potential for unauthorized privilege escalation.
