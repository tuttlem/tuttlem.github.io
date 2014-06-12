---
layout: post
title: Getting started with Lua using C++
date: 2014-01-08
comments: false
---

### Introduction

[Lua](http://www.lua.org/) is a programming language that has seen increased popularity from the game development industry. It's put to use in the disciplines of providing configuration data all the way through to scripting automated character actions.

In today's post, I'll walk you through the setup process of the libraries up to writing your first testing application within a Linux environment.

### Library Installation

Before you get downloading and building this library, you'll just need to ensure that you have a build environment installed and are able to compile. 

At the time of writing this article, the [Lua](http://www.lua.org/) website had 5.2.3 as their latest release. From their [downloads](http://www.lua.org/download.html) page, grab the latest tar.gz archive and extract it onto your machine. Following along with their building instructions, issuing "make linux test" whilst in the extracted folder did the trick. It built Lua ready for me to use. A nice convenience of the make file was the "make local" option. Upon issuing this request, the make system will prepare an installation folder that is suitable for you to use locally (i.e. not installed to the system).

When it comes to downloading the latest versions of libraries, I'll rarely install these to my system. Rather, I drag them around for each project that needs them so that the project determines its dependencies as opposed to my system.

From here, I prepare a distributable directory of development files that I know that each of my project needs. In the case of Lua, I create the following structure:

{% highlight text %}
.
└── lua-5.2.3
    ├── include
    │   ├── lauxlib.h
    │   ├── luaconf.h
    │   ├── lua.h
    │   ├── lua.hpp
    │	  └── lualib.h
    └── lib
    	├── liblua.a
    	└── lua
    		└── 5.2
{% endhighlight %}

I have all of the development headers available (under the "include" folder) and a static version of the Lua library (under lib).

### Building applications

When building Lua applications, you'll need to specify the libraries and include folders to your compiler so it knows where to find them. For a test application that I'd written, the following command line compiled an application for me without any trouble.

{% highlight bash %}
$ g++ -Ilib/lua-5.2.3/include -Llib/lua-5.2.3/lib/ -llua -ldl
{% endhighlight %}

You can see at the end there, mention of both the "lua" and "dl" libraries.

### Test application

A very simple test to start will be creating a program that will execute a Lua script, pull out on of its global variables and display it to screen.

Here's our Lua script:

{% highlight lua %}
x = 10
{% endhighlight %}

Pretty simple. We have one variable `x` set to `10`. Now here's the C++ code that we use to read that one variable out and present it to screen.

{% highlight cpp %}
#include <iostream>
#include <lua.hpp>

int main(int argc, char *argv[]) {
  // create a new lua context to work with
  lua_State *L = luaL_newstate();

  // open any library we may use
  luaL_openlibs(L);

  // read the Lua script off disk and execute it
  if ((luaL_dofile(L, "test.lua")) != 0) {
    
    // handle any errors 
    std::cout << "unable to load test.lua" << std::endl;
    return 1;
    
  }

  // put the value of X at the top of the stack
  lua_getglobal(L, "x");
  
  // interpret the value at the top of the stack 
  // as an integer and put it in the variable "val"
  int val = (int)lua_tointeger(L, -1);
  
  // pop the value of X off the stack
  lua_pop(L, 1);

  // write the value out
  std::cout << "Value of X: " << val << std::endl;

  // finish up with the Lua context
  lua_close(L);

  return 0;

}
{% endhighlight %}

I think that the source code above (paired with the documentation on the Lua website) should make things pretty straight forward.

That's it for today. This is only scratching the surface on what Lua can do. For my purposes right now, I just need to feed configuration values into programs, this fits the purpose really well.