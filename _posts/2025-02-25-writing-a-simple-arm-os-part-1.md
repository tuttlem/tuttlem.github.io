---
layout: post
title: Writing A Simple ARM OS - Part 1
date: 2025-02-25
comments: false
categories: [ asm, arm, os ]
---

# Introduction

ARM, which originally stood for Acorn RISC Machine and now is known as Advanced RISC Machine, is a cornerstone of 
modern embedded systems and mobile devices. Its design is rooted in the principles of Reduced Instruction Set Computing 
(RISC), which emphasizes a small, highly optimized set of instructions that allow for greater efficiency and lower 
power consumption.

In a [previous post]({% post_url 2025-01-24-writing-an-arm-bootloader %}) we looked at setting up a bootloader. In this 
series of blogposts, I want to develop these concepts a little more to implement a simple operating system.

For part 1 today, we'll aim at the following:

* Installing prerequisites
* Setting up a project structure
* Creating a repeatable build environment
* Running your bootloader

The code for this article is [available in my Github repository](https://github.com/tuttlem/armos/releases/tag/part1).

Let's make a start.

# Installing prerequisites

We need to install our toolchain for building our software. The `arm-none-eabi` set is what we'll be targeting.

{% highlight bash %}
sudo pacman -S arm-none-eabi-binutils arm-none-eabi-gcc
{% endhighlight %}

We will also need a virtual machine / emulator to run the software that we build. We'll use [QEMU](https://www.qemu.org/).

{% highlight bash %}
sudo pacman -S qemu-system-arm
{% endhighlight %}

We have enough installed now, let's move on.

# Setup the Project

I've called my project `armos`, and have created the following structure:

{% highlight text %}
.
├── asm
├── build
├── docs
├── README.md
└── src
{% endhighlight %}

* `asm` will hold our assembly language modules
* `build` is where our binaries are built to
* `docs` is for any documentation that we might have
* `src` will hold our c language modules

# Code!

At this point we can add some code. If we add `bootstrap.s` to the `asm` folder we can make a start on the bootloader.

{% highlight asm %}
// ./asm/bootloader.s

    .section    .text
    .global     _start

 _start:
    // initialize the stack pointer
    LDR     sp, =stack_top

    // jump to the kernel main loop
    BL      kernel_main

kernel_main:
    // infinite loop to keep the OS running
1:  B 1b

    // fallback loop
    B .

    // reserve space for the stack in a separate section
    .section    .bss
    .align      4
stack_top:
    // allocate 1kb for the stack
    .space      1024
{% endhighlight %}

This is a pretty basic module to begin with. At the start we define our code with a `.text` section and `_start` is a 
global symbol:

{% highlight asm %}
    .section    .text
    .global     _start
{% endhighlight %}

Next, we setup our stack pointer `sp` by loading the address of our `stack_top`. The equal sign preceeding `stack_top` 
tells the assembler to load the immediate value of the address. We have `stack_top` defined down a little further.

Then, we jump on to our kernel.

Interesting note, `BL` which is _Branch with Link_ works very much like a branch (`B`) but it will store the address 
from where we branched into the link register `r14`. 

{% highlight asm %}
 _start:
    // initialize the stack pointer
    LDR     sp, =stack_top

    // jump to the kernel main loop
    BL      kernel_main
{% endhighlight %}

Now we have two endless loops setup. The first one loops back to the `1:` loop:

{% highlight asm %}
    // infinite loop to keep the OS running
1:  B 1b
{% endhighlight %}

If we do get an unexpected address sneak in for whatever reason, we've got a fallback loop that continually jumps to 
itself using the shorthand `.`.

{% highlight asm %}
    // fallback loop
    B .
{% endhighlight %}

Finally, we complete the module by defining our stack with the `.bss` section. You'll notice the `stack_top` label that 
we referenced earlier. 

{% highlight asm %}
    // reserve space for the stack in a separate section
    .section    .bss
    .align      4
stack_top:
    // allocate 1kb for the stack
    .space      1024
{% endhighlight %}

# Build environment

We need to make this easy to build, so we create a `Makefile` in the root directory. The `Makefile` will use the 
toolchain that we installed earlier, building our binaries into the `build` folder:

{% highlight makefile %}
AS = arm-none-eabi-as
LD = arm-none-eabi-ld
OBJCOPY = arm-none-eabi-objcopy

# Files and directories
ASM_SRCS = asm/bootloader.s
BUILD_DIR = build
TARGET = armos.elf

all: $(BUILD_DIR)/$(TARGET)

$(BUILD_DIR)/$(TARGET): $(ASM_SRCS)
	@mkdir -p $(BUILD_DIR)
	$(AS) -o $(BUILD_DIR)/bootloader.o $(ASM_SRCS)
	$(LD) -Ttext 0x0 -o $(BUILD_DIR)/$(TARGET) $(BUILD_DIR)/bootloader.o
	$(OBJCOPY) -O binary $(BUILD_DIR)/$(TARGET) $(BUILD_DIR)/armos.bin

clean:
	rm -rf $(BUILD_DIR)
{% endhighlight %}

Our call out to our assembler is pretty straight forward, trading our `.s` files for `.o` object files. The linker is 
told that the text section is at `0x0` via the `-Ttext`. We need to give the linker some extra help in these situations 
where we are building for bare-metal targets, such is the case today.

Give it a build.

{% highlight bash %}
make
{% endhighlight %}

All going well you should see some output as follows:

{% highglight text %}
arm-none-eabi-as -o build/bootloader.o asm/bootloader.s
arm-none-eabi-ld -Ttext 0x0 -o build/armos.elf build/bootloader.o
arm-none-eabi-objcopy -O binary build/armos.elf build/armos.bin
{% endhighlight %}

You should also find some built binaries in your `build` folder.

# First launch

We can give our new operating system a run via qemu with the following instruction:

{% highlight bash %}
qemu-system-arm -M versatilepb -kernel build/armos.elf -nographic
{% endhighligh %}

Here we have a few switches:

* `-M versatilepb` emulates a popular ARM development board
* `-kernel build/armos.elf` loads our compiled bootloader/OS binary
* `-nographic` runs qemu without a graphical user interface

If everything as worked, you shouldn't see much at all. Your bootloader/OS is now spinning in a loop, waiting for you 
to turn the computer off!

# Conclusion

In this post, we’ve built the foundation for **armos**. We’ve installed and configured the ARM toolchain, set up QEMU 
to emulate our target board, organized our project directory for clarity and scalability, and even wrote a simple 
bootloader to jumpstart our operating system. With these critical components in place, you’re now ready to embark on 
the next steps—enhancing the bootloader, adding essential kernel functionalities, and ultimately constructing a 
full-fledged minimalist OS. 
