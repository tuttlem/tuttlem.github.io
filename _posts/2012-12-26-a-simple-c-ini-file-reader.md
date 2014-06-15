---
layout: post
title: A simple C++ INI file reader
date: 2012-12-26
comments: false
categories: [ "C", "Programming", "INI" ]
---

### Introduction

I've been looking for a simple way to quickly supply some of my applications with easy-access configuration information. Most of the times these are games and smaller utility applications. Enterprise scale applications deserve configuration systems of their own gargantuan proportions, but that's another story. Thinking back quite a few years, the windows crew had a pretty simple way to supply this sort of information to their running programs, they used the [INI file format](http://en.wikipedia.org/wiki/INI_file).

### How's it look?

It's a pretty basic, plain-text format. Typically you'd see something along the lines of:

{% highlight ini %}
[section]
key1=value
key2=value
key3=value
[another section]
key4=value
key5=value
{% endhighlight %}

You get the idea. The wikipedia link above will give you a more thorough run-down should you need it.

### How to make it usable?

The aim is to basically feed an INI file into this process and have some very easy to use C++ objects out the other side. The [STL](http://en.wikipedia.org/wiki/Standard_Template_Library) contains some very suitable container style objects that will measure up, so I want something like this:

{% highlight cpp %}
// map<section, map<key, value>>
std::map<std::string, std::map<std::string, std::string> > config;
{% endhighlight %}

The map class is a convenient way to manage key-value pairs. Perfect. Looking at the code, the types are very long-winded. All of the namespace declaration actually mixed with the types makes for a very long line. This can be cleaned up by "using" the std namespace, but the whole type definition itself could be cleaned up by just using a typedef. I'll be writing the types long-hand for the duration of this tutorial, just so we're clear.

### Doing it smart

Thanks to the rigid format of the file, we've got a very solid standard set in place as to what we'll expect when we crack the file open. We can centralise all of our file processing around two regular expressions.

{% highlight text %}
\[(.*?)\]
{% endhighlight %}

This first regular expression is our test for the section parts. It tests that the line being interpreted is wrapped in square brackets. When we run this through the regular expression system, it'll allow us to extract just the name. Nice.

{% highlight text %}
(\w+)=([^\+]+(?!\+{3}))
{% endhighlight %}

This second expression will do our key value pair testing. When we use it in extraction with a regular expression, the first match will be the key, the second - the value. Double nice. So, we can fire these regular expressions up using the [boost regex library](http://www.boost.org/doc/libs/1_52_0/libs/regex/doc/html/index.html). I'm led to believe that parts of the boost library will be appearing in the newest C++ standard, regular expressions being one of them.

### Putting it all together

The block of code in the end is quite simple.

{% highlight cpp %}
std::string line, current;                                             

// regular expressions to process ini files
boost::regex section_test("\\[(.*?)\\]");                              
boost::regex value_test("(\\w+)=([^\\+]+(?!\\+{3}))");                 

// the result config object
std::map<std::string, std::map<std::string, std::string> > config;     

// assuming we've opened the file ok into a
// filestream object called "mapfile"
while (getline(mapfile, line)) {                                  
  
  boost::trim(line);

  if (line.length() > 0) {

    boost::smatch match;                                             

    if (boost::regex_search(line, match, section_test)) {
      // any key-value pairs from here to be attributed 
      // to this new name
      current = match[1];                                           
    } else if (boost::regex_search(line, match, value_test)) {
      // set this as a key value pair on the current name
      config[current][match[1]] = match[2];                         
    }                                                                
  }                                                                                                                                             
}                                                                      
{% endhighlight %}

So, with a couple of tests to assure us that the values are ok we've got a pretty crude implementation here. Erroneous lines are ignored rather than responded to in an exception case. If no initial section is supplied before some key value pairs, those pairs will go into an item section with an empty string.

Anyway, if the user is half-sane about how they treat their INI files, you'll be just fine.