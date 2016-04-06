---
layout: post
title: Packing data with Python
date: 2016-04-06
comments: false
categories: [ "string", "byte", "data", "pack", "unpack", "python" ]
---

Defining how a sequence of bytes sits in a memory buffer or on disk can be challenging from time to time. Since everything that you'll work with is a byte, it makes sense that we have an intuitive way to work with this information agnostic of the overlying type restrictions that the language will enforce on us.

In today's post, I'm going to run through Python's byte string packing and unpacking using the [struct](https://docs.python.org/3.5/library/struct.html) package.

### Basics

From the Python documentation:

>This module performs conversions between Python values and C structs represented as Python bytes objects. This can be used in handling binary data stored in files or from network connections, among other sources. It uses Format Strings as compact descriptions of the layout of the C structs and the intended conversion to/from Python values.

When working with a byte string in Python, you prefix your literals with `b`.

{% highlight python %}
>>> b'Hello'
'Hello'
{% endhighlight %}

The `ord` function call is used to convert a text character into its character code representation.

{% highlight python %}
>>> ord(b'H')
72
>>> ord(b'e')
101
>>> ord(b'l')
108
{% endhighlight %}

We can use `list` to convert a whole string of byte literals into an array.

{% highlight python %}
>>> list(b'Hello')
[72, 101, 108, 108, 111]
{% endhighlight %}

The compliment to the `ord` call is `chr`, which converts the byte-value back into a character.

### Packing

Using the `struct` module, we're offered the `pack` function call. This function takes in a format of data and then the data itself. The first parameter defines how the data supplied in the second parameter should be laid out. We get started:

{% highlight python %}
>>> import struct
{% endhighlight %}

If we _pack_ the string `'Hello'` as single bytes:

{% highlight python %}
>>> list(b'Hello')
[72, 101, 108, 108, 111]
>>> struct.pack(b'BBBBB', 72, 101, 108, 108, 111)
b'Hello'
{% endhighlight %}

The format string `b'BBBBB'` tells `pack` to pack the values supplied into a string of 5 unsigned values.  If we were to use a lower case `b` in our format string, `pack` would expect the byte value to be signed.

{% highlight python %}
>>> struct.pack(b'bbbbb', 72, 101, 108, 108, 111)
b'Hello'
{% endhighlight %}

This only gets interesting once we send a value that would make the request overflow:

{% highlight python %}
>>> struct.pack(b'bbbbb', 72, 101, 108, 129, 111)
{% endhighlight %}

{% highlight text %}
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
struct.error: byte format requires -128 <= number <= 127
{% endhighlight %}

The following tables have been re-produced from the [Python documentation](https://docs.python.org/3.5/library/struct.html#struct-format-strings).

*Byte order, size and alignment*

| Character | Byte order               |  Size    | Alignment |
|-----------|--------------------------|----------|-----------|
| `@`       | native                   | native   | native    |
| `=`       | native                   | standard | none      |
| `<`       | little-endian            | standard | none      |
| `>`       | big-endian               | standard | none      |
| `!`       | network (= big-endian)   | standard | none      |

*Types*

| Format | C Type          | Python type     | Standard size | Notes |
|--------|-----------------|-----------------|---------------|-------|
| `x`    | pad byte        | no value        |               |       |     
| `c`    | char            | bytes of length 1 | 1           |       |  
| `b`    | signed char     | integer         | 1             | (1),(3) |
| `B`    | unsigned char   | integer         | 1             | (3) | 
| `?`    | _Bool           | bool            | 1             | (1) |
| `h`    | short           | integer         | 2             | (3) |
| `H`    | unsigned short  | integer         | 2             | (3) |
| `i`    | int             | integer         | 4             | (3) |
| `I`    | unsigned int    | integer         | 4             | (3) | 
| `l`    | long            | integer         | 4             | (3) |
| `L`    | unsigned long   | integer         | 4             | (3) |
| `q`    | long long       | integer         | 8             | (2), (3) |
| `Q`    | unsigned long long  | integer     | 8             | (2), (3) |
| `n`    | ssize_t         | integer         |               | (4) |
| `N`    | size_t          | integer         |               | (4) |
| `f`    | float           | float           | 4             | (5) |
| `d`    | double          | float           | 8             | (5) |
| `s`    | char[]          | bytes           |               |     |   
| `p`    | char[]          | bytes           |               |     |   
| `P`    | void *          | integer         |               | (6) |

### Unpacking

The direct reverse process of packing bytes into an array, is unpacking them again into usable variables inside of your python code.

{% highlight python %}
>>> struct.unpack(b'BBBBB', struct.pack(b'BBBBB', 72, 101, 108, 108, 111))
(72, 101, 108, 108, 111)
>>> struct.unpack(b'5s', struct.pack(b'BBBBB', 72, 101, 108, 108, 111))
(b'Hello',)
{% endhighlight %}


