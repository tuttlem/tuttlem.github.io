---
layout: post
title: Basic file IO in Perl
date: 2017-03-03
comments: false
categories: [ "perl", "file" ]
---

One of the most basic, yet most useful operations you can perform in [Perl](https://www.perl.org/) is working with files. In today's post, I'll show you through a few basic patterns to get started with file IO in Perl.

## `open`

The cornerstone to working with a file, is the [open](http://perldoc.perl.org/functions/open.html) function. It takes the following forms:

* open FILEHANDLE,EXPR
* open FILEHANDLE,MODE,EXPR
* open FILEHANDLE,MODE,EXPR,LIST
* open FILEHANDLE,MODE,REFERENCE
* open FILEHANDLE

`FILEHANDLE` being the local variable that you'll use to reference the file. 

`MODE` determines the type of file access you're requesting over the file

| Mode | Description                                                    |
|------|----------------------------------------------------------------|
| `<`  | File is opened for reading                                     |
| `>`  | File is opened for writing                                     |
| `>>` | File is opened for appending                                   |
| `+<` | File is opened for reading and writing                         |
| `+>` | File is opened for reading and writing, but clobbered first    |
| `|-` | File is interpreted as a command and piped out                 |
| `-|` | File is interpreted as a command and piped in                  |
| `<:encoding(UTF-8)` | File is opened for reading and interpreted as UTF-8 |

### Throwing on failure

{% highlight perl %}
use strict;
use warnings;
 
my $filename = 'data.txt';
open(my $fh, '<:encoding(UTF-8)', $filename)
  or die "Could not open file '$filename' $!";

# TODO: work with the file here
{% endhighlight %}

### Warning on failure

{% highlight perl %}
use strict;
use warnings;
 
my $filename = 'data.txt';
if (open(my $fh, '<:encoding(UTF-8)', $filename)) {
  # TODO: work with the file here
} else {
  warn "Could not open file '$filename' $!";
}
{% endhighlight %}

## Diamond operator `<>`

The diamond-operator is normally used in `while` loops and used to iterate through files:

{% highlight perl %}
# File is opened here into $fh

while (my $row = <$fh>) {
  chomp $row;
  print "$row\n";
}
{% endhighlight %}

## Writing with `print`

Sending information into file is done so with [print](http://perldoc.perl.org/functions/print.html).

{% highlight perl %}
# File is opened here into $fh (using >)

print $fh, "This is a line of text for the file\n";
{% endhighlight %}

## Finishing up with `close`

When you're finished with your files, you'll use [close](http://perldoc.perl.org/functions/close.html)


{% highlight perl %}
# File is opened here into $fh 
# File work --happens--

close $fh or die "Can't close file: $!"; 
{% endhighlight %}

These are just the simple operations for working with files in Perl.