---
layout: post
title: Getting istream to work off a byte array
date: 2014-08-18
comments: false
categories: ["istream", "c++", "array"]
---

### Introduction

The [C++ Standard Library](http://en.wikipedia.org/wiki/C%2B%2B_Standard_Library) provides an extensive [library for working with streams](http://en.wikipedia.org/wiki/C%2B%2B_Standard_Library#Streams_and_Input.2FOutput). These are abstract classes designed to work with data that is in a stream format. There are comprehensive concrete implementations for working with files and strings, however I'm still yet to find an implementation that will take a plain old c-array and allow you to treat it as a stream.

In today's post, I'll present a small `std::istream` implementation that will consume these plain old c-arrays so that you can keep the rest of your APIs uniform to using stream objects.

### A brief explanation

We'll actually be developing two classes here. We'll need a class to derive from `std::istream` which is what we'll pass around to other parts of our program, but internally this `std::istream` derived object will manage a `std::basic_streambuf<char>` derivative.

Looking at the [definition](http://en.cppreference.com/w/cpp/io/basic_streambuf) of a `std::basic_streambuf` we can see the following:

> The class basic_streambuf controls input and output to a character sequence.

It would appear that most of the work here has been done for us. `basic_streambuf` will take care of the I/O from our character sequence, we just need to supply it <em>(the character sequence, that is)</em>. I did say `byte` array in the title of this post, so the actual data type will be `uint8_t*` as opposed to `char*`.

### Implementation

{% highlight c++ %}
class membuf : public std::basic_streambuf<char> {
public:
  membuf(const uint8_t *p, size_t l) {
    setg((char*)p, (char*)p, (char*)p + l);
  }
};
{% endhighlight %}

Our implementation of `basic_streambuf` must abide by the [char_traits](http://en.cppreference.com/w/cpp/string/char_traits) type definition, so we get as close to our byte definition as possible with `char`. You can see that the constructor has a little bit of cast work going on to get `setg` to operate correctly.

Finally, we just create an `istream` derivative that uses this `membuf` object under the covers:

{% highlight c++ %}
class memstream : public std::istream {
public:
  memstream(const uint8_t *p, size_t l) :
    std::istream(&_buffer),
    _buffer(p, l) {
    rdbuf(&_buffer);
  }

private:
  membuf _buffer;
};
{% endhighlight %}

We set the internal buffer that `memstream` will use by making a call to `rdbuf`. The constructor performs some initialisation of the stream itself (to use a `membuf`) implementation.

### In Use

You can now treat your plain old c-arrays just like an input stream now. Something simple:

{% highlight c++ %}
uint8_t buf[] = { 0x00, 0x01, 0x02, 0x03 };
memstream s(buf, 4);

char b;

do {
  s.read(&b, 1);
  std::cout << "read: " << (int)b << std::endl;
} while (s.good());
{% endhighlight %}

That's all there is to it. From the snippet above, you can pass `s` around just like any other input stream, because, well, it <strong>is</strong> just any other input stream.


