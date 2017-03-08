---
layout: post
title: Commit squashing
date: 2017-03-08
comments: false
categories: [ "git", "commit", "squash", "history" ]
---

[Git](https://git-scm.com/) provides some excellent features when you need to amend your commit history before pushing your changes into a repository. In today's article, I'll run through a relativly simple use-case of the [rebase](https://git-scm.com/docs/git-rebase) command and how you can squash commits down.

## This blog

I've recently changed text editors from [Sublime](https://www.sublimetext.com/) back to [vim](http://www.vim.org/). In the repository of this blog, I have a script that bootstraps a blog post with the title and date; and then opens my text editor ready for me to write.

Today I also patched a small bug where, when the file name is calculated from the title of the blog I specify - consecutive letters were being de-duplicated. Made it a real pain for a lot of words . . so, this changed. These two changes occured on the same file, `new-post.sh`; but I didn't think about changing my text editor until after.

## The first change

So, the first change went into the file. I staged the change and then committed it:

{% highlight text %}
➜  tuttlem.github.io git:(master) ✗ git add .
➜  tuttlem.github.io git:(master) ✗ git commit -m "Patched create script"
[master 5b7b3b4] Patched create script
 1 file changed, 1 insertion(+), 1 deletion(-)
{% endhighlight %}

## The second change

Then, thinking about it, I changed my text editor after.

{% highlight text %}
➜  tuttlem.github.io git:(master) ✗ git add .
➜  tuttlem.github.io git:(master) ✗ git commit -m "Changed editor"
[master 7408fdf] Changed editor
 1 file changed, 1 insertion(+), 1 deletion(-)
{% endhighlight %}

This sucks. Really only wanted this in the one commit. So, I kicked off [rebase](https://git-scm.com/docs/git-rebase) in interactive mode (with `-i`).

{% highlight text %}
➜  tuttlem.github.io git:(master) git rebase -i origin/master
{% endhighlight %}

This opened up a text editor for me, allowing me to make changes.

## Changing history!

You're presented with a list of commits that haven't yet been pushed to the server. You're also given the opportunity to change what goes on with these before you push:

{% highlight text %}
pick 50412a2 New article
pick 3029ddc New article
pick e8df37a New article
pick ef5028c New article
pick e151644 New article
pick 378fc34 New article
pick eaccbd8 New article
pick 05c2d82 New article
pick d60be6a New articles
pick 5b7b3b4 Patched create script
pick 7408fdf Changed editor

# Rebase 5c2c93d..7408fdf onto 5c2c93d (11 command(s))
#
# Commands:
# p, pick = use commit
# r, reword = use commit, but edit the commit message
# e, edit = use commit, but stop for amending
# s, squash = use commit, but meld into previous commit
# f, fixup = like "squash", but discard this commit's log message
# x, exec = run command (the rest of the line) using shell
# d, drop = remove commit
#
# These lines can be re-ordered; they are executed from top to bottom.
#
# If you remove a line here THAT COMMIT WILL BE LOST.
#
# However, if you remove everything, the rebase will be aborted.
#
# Note that empty commits are commented out
{% endhighlight %}

I just want `7408fdf` squashed into `5b7b3b4`. I also want to change the commit message of `5b7b3b4`. Afterwards, I'd learned that git will give me the opportunity to re-word this to combine commit messages.

{% highlight text %}
pick 50412a2 New article
pick 3029ddc New article
pick e8df37a New article
pick ef5028c New article
pick e151644 New article
pick 378fc34 New article
pick eaccbd8 New article
pick 05c2d82 New article
pick d60be6a New articles
reword 5b7b3b4 Patched create script and changed editor
squash 7408fdf Changed editor
{% endhighlight %}

Saving this file off, we're now presented with another text editor that allows us to update the commit message accordingly. After a bit of processing, we're done:

{% highlight text %}
➜  tuttlem.github.io git:(master) git rebase -i origin/master
[detached HEAD f0b825a] Patched create script
 Date: Wed Mar 8 11:54:00 2017 +1000
 1 file changed, 1 insertion(+), 1 deletion(-)
[detached HEAD 7bc6902] Patched create script
 Date: Wed Mar 8 11:54:00 2017 +1000
 1 file changed, 2 insertions(+), 2 deletions(-)
Successfully rebased and updated refs/heads/master.
{% endhighlight %}

Your git log should now confirm that everything has gone to plan:

{% highlight text %}
commit 7bc6902d2543eb87dad14b088edc9a5506295809
Author: Michael Tuttle <tuttlem@gmail.com>
Date:   Wed Mar 8 11:54:00 2017 +1000

  Patched create script

  Changed editor

  Patched create script and changed editor

commit d60be6ae1acc508669193a5bfead887913284aef
Author: Michael Tuttle <tuttlem@gmail.com>
Date:   Fri Mar 3 20:04:06 2017 +1000

  New articles
{% endhighlight %}

Push to the server.
