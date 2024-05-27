---
layout: post
title: Getting Started with GNUstep
date: 2024-05-27
comments: false
categories: [ "" ]
---

### Introduction

[GNUstep](https://gnustep.github.io/) is a development framework for writing GUI applications. It aims to follow [Apple's Cocoa API](https://en.wikipedia.org/wiki/Cocoa_(API)) but allows you to write applications for more platforms than just OSX.

In today's article we'll setup a local environment for writing [GNUstep](https://gnustep.github.io/) programs, and we'll also write and compile a simple "Hello, world" application to make sure everything is setup.

### Brief History

[GNUstep](https://gnustep.github.io/) is an open-source implementation of the [OpenStep](https://en.wikipedia.org/wiki/OpenStep) specification, which originated from [NeXT](https://en.wikipedia.org/wiki/NeXT), a company founded by Steve Jobs after he left [Apple](https://en.wikipedia.org/wiki/Apple_Inc) in 1985. [NeXT](https://en.wikipedia.org/wiki/NeXT) developed the [NeXTSTEP](https://en.wikipedia.org/wiki/NeXTSTEP) operating system, which introduced an advanced object-oriented framework for software development. In 1993, [NeXT](https://en.wikipedia.org/wiki/NeXT) partnered with [Sun Microsystems](https://en.wikipedia.org/wiki/Sun_Microsystems) to create the [OpenStep standard](https://gnustep.github.io/resources/OpenStepSpec/OpenStepSpec.html), which aimed to make [NeXT's](https://en.wikipedia.org/wiki/NeXT) frameworks available on other platforms.

When Apple acquired [NeXT](https://en.wikipedia.org/wiki/NeXT) in 1996, the technology from [NeXTSTEP](https://en.wikipedia.org/wiki/NeXTSTEP) and [OpenStep](https://en.wikipedia.org/wiki/OpenStep) formed the foundation of Apple's new operating system, [Mac OS X](https://en.wikipedia.org/wiki/Mac_OS_X). Apple's [Cocoa](https://en.wikipedia.org/wiki/Cocoa_(API)) framework, a core part of [macOS](https://en.wikipedia.org/wiki/MacOS), is directly derived from OpenStep. [GNUstep](http://www.gnustep.org/), initiated in the early 1990s, aims to provide a free and portable version of the OpenStep API, allowing developers to create cross-platform applications with a foundation rooted in the same principles that underpin macOS development.

So, this still leaves us with [GNUstep](https://gnustep.github.io/) to get up and running.

### Developer environment

First up, we need to install all of the dependencies in our developer environment. I'm using [Debian](https://www.debian.org/) so all of my package management will be specific to that distribution. All of these packages will be available on all distributions though.

{% highlight shell %}
sudo apt-get install build-essential gnustep gnustep-devel
{% endhighlight %}

Once this is installed, we can move on to writing some code.

### "Hello World" alert

The following program is very basic. It'll show an alert to screen, and then exit after the user has dismissed the alert.

{% highlight objc %}
// hello.m

#include <AppKit/AppKit.h>

@interface AppDelegate : NSObject<NSApplicationDelegate>

@end

@implementation AppDelegate

- (void)applicationDidFinishLaunching:(NSNotification *)notification {
  NSAlert *alert = [[[NSAlert alloc] init] autorelease];
  [alert setMessageText:@"Hello, World!"];
  [alert runModal];
  [NSApp terminate:nil];
}

@end

int main(int argc, char *argv[]) {
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  NSApplication *app = [NSApplication sharedApplication];

  AppDelegate *appDelegate = [[[AppDelegate alloc] init] autorelease];
  [app setDelegate:appDelegate];

  [app run];
  [pool drain];

  return 0;
}

{% endhighlight %}

### Walkthrough

First of all, we include `AppKit/AppKit.h` so that we get access to the programming API. We then define our own `AppDelegate` so we capture the `applicationDidFinishLaunching` slot:

{% highlight objc %}
#include <AppKit/AppKit.h>

@interface AppDelegate : NSObject<NSApplicationDelegate>

@end

@implementation AppDelegate

- (void)applicationDidFinishLaunching:(NSNotification *)notification {
  NSAlert *alert = [[[NSAlert alloc] init] autorelease];
  [alert setMessageText:@"Hello, World!"];
  [alert runModal];
  [NSApp terminate:nil];
}

@end
{% endhighlight %}

The handler sets up the alert to show to screen, runs this modal as "the program" with `runModal`, and then we finish with `terminate'.

Next is the main program itself.

We start with an `NSAutoreleasePool` to give our program some automatic memory management. This is cleaned up at the end with a call to `[pool drain]`.

The `app` variable is setup as an `NSApplication` which allows us to instantiate and attach our `AppDelegate` via the `[app setDelegate:appDelegate];` call.

{% highlight objc %}
int main(int argc, char *argv[]) {
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  NSApplication *app = [NSApplication sharedApplication];

  AppDelegate *appDelegate = [[[AppDelegate alloc] init] autorelease];
  [app setDelegate:appDelegate];

  [app run];
  [pool drain];

  return 0;
}

{% endhighlight %}

### Building

Now that we have our code written into `hello.m`, we can compile and link. There are some compile and link time libraries required to get this running. For this, we'll use `gnustep-config` to do all of the heavy lifting for us.

{% highlight shell %}
gcc `gnustep-config --objc-flags` -o hello hello.m `gnustep-config --gui-libs`
{% endhighlight %}

If everything has gone to plan, you should be left with an executable called `hello`.

<img alt="GNUstep Hello World" src="{{ site.url }}/assets/gnustep_hello.png" width="100%">

Happy GNUstep'ing!
