---
layout: post
title: File encodings and iconv
date: 2015-08-18
comments: false
categories: [ "file", "iconv", "libiconv" ]
---

There are a handful of really useful tools for dealing with [character encoding](https://en.wikipedia.org/wiki/Character_encoding). In today's post, I'll take you through identifying this characteristic and changing it.

### What is Character Encoding?

[Wikipedia](https://en.wikipedia.org/wiki/) has the most [comprehensive breakdown](https://en.wikipedia.org/wiki/Character_encoding) on the topic. The simplest way to look at it though is that a character encoding assigns a code to each character in an alphabet. 

| Code   | Encoding | Character |
|--------|----------|-----------|
| 65     | ASCII    | A         |
| U+2776 | UNICODE  | ❶         |
| 0xd8   | LATIN4   | Ø         |


The unicode and latin4 characters don't exist within the ASCII character space, therefore those characters simple don't translate and can't be encoded by ASCII.

### Querying files

To determine what encoding is being used with a file, you can use the `file` unix utility.

{% highlight bash %}
$ echo "Here is some text" > a-test-file
$ file a-test-file
a-test-file: ASCII text
{% endhighlight %}

Using the `-i` switch, we can turn the `ASCII text` output into a mime string which can yield some more information:

{% highlight bash %}
$ file -i a-test-file
a-test-file: text/plain; charset=us-ascii
{% endhighlight %}

To make this encoding representation change a little clearer, below I've pasted in the output of `hexdump` on that test file that we'd created earlier.

{% highlight text %}
0000000 6548 6572 6920 2073 6f73 656d 7420 7865
0000010 0a74                                   
0000012
{% endhighlight %}

Remember, these bytes are not only in hex; they're also flipped according to how the string is written. Let's take the first two bytes `6548`:

{% highlight text %}
0x65 = e
0x48 = H
{% endhighlight %}

We're using an 8-bit encoding, our string has 17 characters plus a newline (18). Easy.

### Changing the encoding of a file

We can use `iconv` to transition our text file from one encoding to another. We specify its current encoding with the `-f` switch and the encoding that we want to convert it to using the `-t` switch.

{% highlight bash %}
$ iconv -f ascii -t unicode a-test-file > a-test-file.unicode
{% endhighlight %}

This is changing our test file into an encoding that uses more data-space per character. Taking a look at the type of file we've just created:

{% highlight bash %}
$ file a-test-file.unicode a-test-file.unicode: Little-endian UTF-16 Unicode text, with no line terminators
{% endhighlight %}

If we take a look at the hexdump of this file, you can see that every byte is now padded with an extra zeroed-out byte. 

{% highlight text %}
0000000 feff 0048 0065 0072 0065 0020 0069 0073
0000010 0020 0073 006f 006d 0065 0020 0074 0065
0000020 0078 0074 000a                         
0000026
{% endhighlight %}

The file also starts with a [BOM](https://en.wikipedia.org/wiki/Byte_order_mark) of `feff` which was unseen in the ASCII counterpart.

### What encodings are supported

You can list the <em>known coded character sets</em> with `iconv` as well with the `--list` switch. This will dump a massive list of encodings (and aliases) that you can use.

### More!

A really good article was written about the [The Absolute Minimum Every Software Developer Absolutely, Positively Must Know About Unicode and Character Sets (No Excuses!)](http://www.joelonsoftware.com/articles/Unicode.html). It's certainly well written article that'll give you some schooling in encodings, quick smart.
