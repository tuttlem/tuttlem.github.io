---
layout: post
title: Simple Backups with rsync
date: 2024-05-04
comments: false
categories: [ "admin", "backup", "rsync" ]
---

### Introduction

[Rsync](https://linux.die.net/man/1/rsync) is a powerful tool often utilized for its proficiency in file synchronization and transfer. Widely adopted in various computing environments, rsync excels in efficiently mirroring data across multiple platforms and minimizing data transfer by sending only changes in files. This utility is not only pivotal for maintaining backups but also ensures that the copies are consistent and up-to-date.

Today’s article is designed to guide you through the steps of setting up a basic yet effective backup system using rsync. By the end of this guide, you'll have the knowledge to implement a reliable backup solution that can be customized to fit your specific needs.

### Daily

The daily backup captures all of the changes for the day, giving you a backup with the fastest cadence.

{% highlight bash %}
#!/bin/bash

# daily-backup.sh
#
# Basic copy script to perform daily backups of the home dir

USER=$(whoami)
HOST=$(hostname)

rsync -aAX \
    --delete \
    --rsync-path="mkdir -p /path/to/backups/$HOST/daily/ && rsync" \
    --exclude-from=/home/$USER/.local/bin/rsync-homedir-excludes.txt \
    /home/$USER/ backup-user@backup-server:/path/to/backups/$HOST/daily/ 
{% endhighlight %}

Using `whoami` and `hostname` we can generalise this script so that you can reuse it between all of your machines.

The `rsync-homedir-excludes.txt` file allows you to define files that you're not interested in backing up.

The switches that we're sending into `rsync` are quite significant:

* `-a` puts `rsync` into archive mode, preserving file structures, and links
* `-A` preserves the ACLs so all of our permissions are preserved
* `-X` any extra attributes that are stored by your file system will be preserved
* `--delete` will delete files in the destination that are no longer present in the source, making the backup a true mirror

### Weekly

The weekly backup is very similar to the daily backup. It'll target different folders, and will have a different execution cadence.

{% highlight bash %}
#!/bin/bash

# weekly-backup.sh
#
# Basic copy script to perform weekly backups of the home dir

USER=$(whoami)
HOST=$(hostname)

rsync -aAX \
    --delete \
    --rsync-path="mkdir -p /path/to/backups/$HOST/weekly/ && rsync" \
    --exclude-from=/home/$USER/.local/bin/rsync-homedir-excludes.txt \
    /home/$USER/ backup-user@backup-server:/path/to/backups/$HOST/weekly/ 
{% endhighlight %}

There isn't much difference here. Just writing to the `/weekly` folder.

### Monthly

The longest cadence that we have is a monthly process which will archive the current state into an archive, and date the file for later use potentially.

{% highlight bash %}
#!/bin/bash

# monthly-backup.sh
#
# Monthly archive and copy

ARCHIVE=$(date +%Y-%m-%d).tar.gz
USER=$(whoami)
HOST=$(hostname)

tar --exclude-from=/home/$USER/.local/bin/rsync-homedir-excludes.txt \
    -zcvf \
    /tmp/$ARCHIVE \
    /home/$USER

scp /tmp/$ARCHIVE backup-user@backup-server:/path/to/backups/$HOST/monthly/

rm /tmp/$ARCHIVE
{% endhighlight %}

Using `tar` this script builds a full archive, and then sends that off to the backup server.

### Scheduling

Finally, we need to setup these scripts to execute automatically for us. For this, we'll use `cron`.

Here's an example crontab scheduling these scripts:

{% highlight text %}
# m h  dom mon dow   command
00 22 * * * /home/user/.local/bin/daily-backup.sh
00 21 * * 6 /home/user/.local/bin/weekly-backup.sh
00 22 1 * * /home/user/.local/bin/monthly-backup.sh
{% endhighlight %}

