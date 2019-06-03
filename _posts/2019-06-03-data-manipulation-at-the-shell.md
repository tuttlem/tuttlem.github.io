---
layout: post
title: Data manipulation at the shell
date: 2019-06-03
comments: false
categories: [ "shell", "tools", "data" ]
---

### Introduction

From time to time, you'll find that some tasks could be easily achieved at the command line if you just had that one tool that you could slot in. In today's article, I'll take you through a few common data manipulation/mangling tools that should get you pretty productive.

### head

> output the first part of files

The `head` command will allow you to peek into a file. This is really handy when you are dealing with huge files, and you only want to sample the first `n` lines (or chars).

{% highlight shell %}
head myfile.txt

# view the first 10 characters of a file
head -c 10 myfile.txt

# view the first 10 lines of a file
head -n 10 myfile.txt
{% endhighlight %}

### tail

> output the last part of files

The `tail` command will allow you to sample the end of a file. `tail` works as `head`'s compliment. The `--follow/-f` switch is very handy with the `tail` command. When a file is still being written to, `--follow` will allow you to continaully stream the latest bytes being written to a file as they arrive.

{% highlight shell %}
tail myfile.txt

# view the last 10 characters of a file
tail -c 10 myfile.txt

# view the last 10 lines of a file
tail -n 10 myfile.txt

# follow the output of a file 
tail -f myfile.txt
{% endhighlight %}

### iconv

> convert text from one character encoding to another

Being able to change the character encoding of files that you're working on can simply your processing greatly. By only needing to deal with a single encoding, you can remove this class of issue from your pipeline. A more comprehensive writeup on `iconv` can be found [here]({% post_url 2015-08-18-file-encodings-iconv %}).

{% highlight shell %}
# convert a file from ascii to unicode
iconv -f ascii -t unicode a-test-file > a-test-file.unicode
{% endhighlight %}

### tr

> translate or delete characters

`tr` will allow you to translate your input in such a way that you can cleanse information. _Translate, squeeze, and/or delete characters_ as the documentation says.

{% highlight shell %}
# replace the spaces with tab characters
echo "All spaced out" | tr [:space:] '\t'
{% endhighlight %}

The `[:space:]` identifier user here is a special class identifier. There are support for others, too.

| Identifier | Description |
|------------|-----------------------------|
| `[:alnum:]`  | all letters and digits |
| `[:alpha:]`  | all letters |
| `[:blank:]` | all horizontal whitespace |
| `[:cntrl:]` | all control characters |
| `[:digit:]` | all digits |
| `[:graph:]` | all printable characters, not including space |
| `[:lower:]` | all lower case letters |
| `[:print:]` | all printable characters, including space |
| `[:punct:]` | all punctuation characters |
| `[:space:]` | all horizontal or vertical whitespace |
| `[:upper:]` | all upper case letters |
| `[:xdigit:]` | all hexadecimal digits |
| `[=CHAR=]` | all characters which are equivalent to CHAR |

### wc

> print newline, word, and byte counts for each file

Takes the input and counts _things_.

{% highlight shell %}
# count the number of bytes in a file
wc -c myfile.txt

# count the number of lines
wc -l myfile.txt
{% endhighlight %}

### split

> split a file into pieces

`split` takes a file, and cuts it into smaller pieces. This is really handy when your input file is massive; cutting the job down into smaller pieces gives you the chance to parallelize this work appropriately.

{% highlight shell %}
split -l 100 contacts.csv contact-
{% endhighlight %}

### sort

> sort lines of text files

The `sort` command will allow you to sort a text file by any column, in a couple of different ways. 

{% highlight shell %}
# sort a csv by the 5th column, alpha
sort -t"," -k5,5 contacts.csv

# sort a csv by the 3rd column, numerically
sort -t"," -k3n,3 contacts.csv

# sort a csv by the 8th column, numberically reverse
sort -t"," -k8nr,8 contacts.csv
{% endhighlight %}

### uniq

> report or omit repeated lines

{% highlight shell %}
# show a unique list of names
cat names | uniq
{% endhighlight %}

### cut

> remove sections from each line of files

Cutting columns from your file can be useful if you need to trim information from your data source prior to moving to the next phase of your pipeline.

{% highlight shell %}
# remove the fifth column
cut -d, -f 5 contacts.csv

# remove columns 2-though-4
cut -d, -f 2-4 contacts.csv
{% endhighlight %}

### paste

> merge lines of files

The `paste` command takes multiple files, and links each line of data together.

{% highlight text %}
# colours.txt
blue
red
orange

# sports.txt
swimming
cricket
golf
{% endhighlight %}

These values can be pasted together:

{% highlight shell %}
paste -d ',' colours.txt sports.txt
{% endhighlight %}

The output of which would look like this:

{% highlight text %}
blue,swimming
red,cricket
orange,golf
{% endhighlight %}

### join

> join lines of two files on a common field

The `join` command will run a fairly basic `INNER JOIN` between two files. One column from each file will be chosen, and a strong join performed leaving you with the coninciding set.

{% highlight shell %}
# join contacts (col 5) on accounts (col 4)
join -t"," -1 5 -2 4 contacts.csv accounts.csv
{% endhighlight %}

### grep, sed, and awk

Each of these commands really needs their own articles. They are full programming tools in their own right.

* [Dissecting dmesg with awk]({ post_url 2015-01-21-dissecting-dmesg-with-awk })
* [Text Search with grep]({ post_url 2019-04-24-text-search-with-greGREPp })


All of these are excellent tools to allow you to build complex processing pipelines from your console.

