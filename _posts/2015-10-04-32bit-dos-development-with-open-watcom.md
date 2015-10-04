---
layout: post
title: 32bit DOS Development with Open Watcom
date: 2015-10-04
comments: false
categories: [ "dos", "watcom", "32" ]
---

The [Watcom Compiler](https://en.wikipedia.org/wiki/Watcom_C/C%2B%2B_compiler) is an open source C & C++ compiler that has a very successful history when it was [discovered that the DOOM developers were using it](https://groups.google.com/forum/#!topic/rec.games.programmer/yJOLqa9g3Wk). That was a very long time ago, but that shouldn't stop us having a go!

### Installation

I've grabbed the dos bundle from the [Open Watcom FTP](ftp://ftp.openwatcom.org/pub/) site and installed it into [DosBox](http://www.dosbox.com/). The only problem with this setup, it that I much prefer to use a text editor that's outside of the DOS environment (like emacs/sublime, etc.) DosBox sometimes has a bit of difficulty picking up file system changes that have been mounted in.

<kbd>Shift</kbd> + <kbd>Ctrl</kbd> + <kbd>F4</kbd> (documented as just <kbd>Ctrl</kbd> + <kbd>F4</kbd>) forces DosBox to refresh its mounts.

Very handy. 

### The Tools

There are a bucket of binaries that are bundled with the installation. 

| Utility    | Description         |
|------------|---------------------|
| wasm.exe   | Assembler           |
| whelp.exe  | Help Command Line   |
| wmake.exe  | Make utility        |
| wcl386.exe | Compile and Link    |
| wpp386.exe | Optimizing compiler |
| wcc386.exe | Optimizing compiler |
| wd.exe     | Debugger            |
| wlib.exe   | Library manager     |
| wlink.exe  | Linker              |
| dos32a.exe | DOS32A extender     |
| wdis.exe   | Disassembler        |

For convenience, we'll use `wcl386.exe` as this will perform the compilation and linking step in one for us.

### Compiling and Linking

Prior to compilation and linking, things will go a lot smoother if you've prepared your environment variables correctly. 

{% highlight text %}
SET PATH C:\WATCOM\BINW;%PATH%;
SET INCLUDE=C:\WATCOM\H;
SET WATCOM=C:\WATCOM
SET EDPATH=C:\WATCOM\EDDAT
SET WIPFC=C:\WATCOM\WIPFC
{% endhighlight %}

Open up your favorite editor and create a hello world application, called `hello.cpp`.

{% highlight cpp %}
#include <stdio.h>

int main(int argc, char *argv[]) {
  printf("Hello, world!\n");
  return 0;
}
{% endhighlight %}

Now build it with `wcl386.exe`:

{% highlight text %}
C:\SRC> wcl386 hello.cpp
Open Watcom C/C++32 Compile and Link Utility Version 1.9
Portions Copyright (c) 1988-2002 Sybase, Inc. All Rights Reserved.
Source code is available under the Sybase Open Watcom Public License.
See http://www.openwatcom.org/ for details.
  wpp386 HELLO.CPP 
DOS/4GW Protected Mode Run-time  Version 1.97
Copyright (c) Rational Systems, Inc. 1990-1994 
Open Watcom C++32 Optimizing Compiler Version 1.9
Portions Copyright (c) 1989-2002 Sybase, Inc. All Rights Reserved.
Source code is available under the Sybase Open Watcom Public License.
See http://www.openwatcom.org/ for details.
HELLO.CPP: 7 lines, included 1160, no warnings, no errors
  wlink @__wcl__.lnk
DOS/4GW Protected Mode Run-time  Version 1.97
Copyright (c) Rational Systems, Inc. 1990-1994 
Open Watcom Linker Version 1.9
Portions Copyright (c) 1985-2002 Sybase, Inc. All Rights Reserved.
Source code is available under the Sybase Open Watcom Public License.
See http://www.openwatcom.org/ for details.
loading object files
searching libraries
creating a DOS/4G executable
{% endhighlight %}

We can now run our application:

{% highlight text %}
C:\SRC> hello.exe
DOS/4GW Protected Mode Run-time  Version 1.97
Copyright (c) Rational Systems, Inc. 1990-1994 
Hello, world
{% endhighlight %}

### What is DOS/4GW?

To a lot of us, the *DOS/4GW* is a very familiar banner that we saw when we'd fire up one of our favorite games. But, what is it?

[Wikipedia's article](https://en.wikipedia.org/wiki/DOS/4G) defines its role pretty well:

> DOS/4G is a 32-bit DOS extender developed by Rational Systems (now Tenberry Software). It allows DOS programs to eliminate the 640 KB conventional memory limit by addressing up to 64 MB of extended memory on Intel 80386 and above machines.

It's the resident binary that gets packaged with your compiled application that facilitates access to the computers' full array of resources. Without it, you'd be stuck with what DOS provides you by default.

### Conclusion

Well, it's always nice to go over this old stuff. In my next posts, I'll cover inline assembly and mode 13/x to get a head start on writing DOS games in the 90's!