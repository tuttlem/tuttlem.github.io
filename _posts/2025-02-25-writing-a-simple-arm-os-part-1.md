---
layout: post
title: Writing A Simple ARM OS - Part 1
date: 2025-02-25
comments: false
categories: [ asm, arm, os ]
---

# Introduction

In this series, we'll build a small operating system for the ARM platform from the ground up. Along the way, we'll 
explore fundamental OS concepts and incrementally add components, turning abstract ideas into working code. Each 
article will focus on a specific piece of the system, guiding you through the process step by step.

We're going to use QEMU, an open-source emulator, so we can develop and test our code directly on a PC—no hardware 
required (for now).

In Part 1, we’ll first discuss ARM, then move on to the following:

* Installing prerequisites
* Setting up a project structure
* Creating a repeatable build environment
* Running your bootloader

The code for this article is [available in my Github repository](https://github.com/tuttlem/armos/releases/tag/part1).

Let's make a start.

## What is ARM?

ARM, short for Advanced RISC Machine, is a family of Reduced Instruction Set Computing ([RISC](https://en.wikipedia.org/wiki/Reduced_instruction_set_computer)) architectures that power 
billions of devices, from smartphones and tablets to embedded systems and IoT devices. Originally known as Acorn RISC 
Machine, ARM has become a cornerstone of modern computing due to its energy efficiency and simplicity compared to 
Complex Instruction Set Computing ([CISC](https://en.wikipedia.org/wiki/Complex_instruction_set_computer)) architectures like x86. Designed around the RISC philosophy, ARM processors 
use a small, highly optimized instruction set, enabling greater performance per watt and making them ideal for 
low-power and mobile environments.

## Why Emulation?

While ARM assembly is usually executed on physical devices, emulation tools like [QEMU](https://www.qemu.org/) allow
you to:

* Test code without requiring hardware.
* Experiment with different ARM-based architectures and peripherals.
* Debug programs more effectively using tools like GDB.

## Supported ARM Hardware

Before we begin coding, let’s take a brief look at some popular ARM-based platforms:

* **Raspberry Pi**: A widely used single-board computer.
* **BeagleBone Black**: A powerful option for embedded projects.
* **STM32 Microcontrollers**: Common in IoT and robotics applications.

# Installing prerequisites

Before we begin, we need to setup our development and build environment. I'm using [Manjaro](https://manjaro.org/) so package
names might be slightly different for your distro of choice.

To build our software, we’ll install the `arm-none-eabi` toolchain, which provides the assembler (`as`), linker (`ld`), 
and other essential utilities.

{% highlight bash %}
sudo pacman -S arm-none-eabi-binutils arm-none-eabi-gcc
{% endhighlight %}

We will also need a virtual machine / emulator to run the software that we build. We'll use [QEMU](https://www.qemu.org/).

{% highlight bash %}
sudo pacman -S qemu-system-arm
{% endhighlight %}

With our toolchain and emulator installed, we’re ready to move forward.

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

Now that our project structure is in place, we can begin writing our first piece of assembly code: the bootloader. 

If we add `bootstrap.s` to the `asm` folder we can make a start on the bootloader.

{% highlight asm %}
.section    .text
.global     _start

_start:
    LDR     sp, =stack_top   @ initialize the stack pointer
    BL      kernel_main      @ jump to the kernel main loop

kernel_main:
1:  B 1b                     @ infinite loop to keep the OS running

    B .                      @ fallback loop
    
.section    .bss             
.align      4
stack_top:
.space      1024             @ allocate 1kb for the stack
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
    LDR     sp, =stack_top   @ initialize the stack pointer
    BL      kernel_main      @ jump to the kernel main loop
{% endhighlight %}

Now we have two endless loops setup. The first one loops back to the `1:` loop:

{% highlight asm %}
1:  B 1b                     @ infinite loop to keep the OS running
{% endhighlight %}

If we do get an unexpected address sneak in for whatever reason, we've got a fallback loop that continually jumps to 
itself using the shorthand `.`.

{% highlight asm %}
B .                      @ fallback loop
{% endhighlight %}

Finally, we complete the module by defining our stack with the `.bss` section. You'll notice the `stack_top` label that 
we referenced earlier. 

{% highlight asm %}
.section    .bss             
.align      4
stack_top:
.space      1024             @ allocate 1kb for the stack
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

Our call out to our assembler is pretty straight forward, trading our `.s` files for `.o` object files. We use 
`-Ttext 0x0` to explicitly tell the linker that our program should start at address `0x0`, which is necessary for 
bare-metal environments. 

Give it a build.

{% highlight bash %}
make
{% endhighlight %}

All going well you should see some output as follows:

{% highlight text %}
arm-none-eabi-as -o build/bootloader.o asm/bootloader.s
arm-none-eabi-ld -Ttext 0x0 -o build/armos.elf build/bootloader.o
arm-none-eabi-objcopy -O binary build/armos.elf build/armos.bin
{% endhighlight %}

You should also find some built binaries in your `build` folder.

# First launch

We can give our new operating system a run via qemu with the following instruction:

{% highlight bash %}
qemu-system-arm -M versatilepb -kernel build/armos.elf -nographic
{% endhighlight %}

Here we have a few switches:

* `-M versatilepb` emulates a popular ARM development board
* `-kernel build/armos.elf` loads our compiled bootloader/OS binary
* `-nographic` runs qemu without a graphical user interface

If everything works, you won’t see much—your bootloader is running in an infinite loop, waiting for further development.

# Debugging

Because we are running in a virtualised environment, we have a full debugger at our disposal. Having a debugger attached 
to your code when things aren't quite going to plan can be very valuable to understand what's happening in the internals 
of your program.

Using the `-gdb` option, you can instruct qemu to open a debugging port.

{% highlight shell %}
qemu-system-arm -M versatilepb -kernel boot.elf -S -gdb tcp::1234
{% endhighlight %}

You can then connect to gdb with the following:

{% highlight shell %}
arm-none-eabi-gdb boot.elf
target remote :1234
{% endhighlight %}

# Deployment

Finally, we'll touch on deployment.

For deployment, we’ll use a Raspberry Pi as an example. This process is similar for other ARM-based boards.

## Flashing

First, we need to convert the ELF file to a raw binary format suitable for booting:

{% highlight shell %}
arm-none-eabi-objcopy -O binary boot.elf boot.bin
{% endhighlight %}

Use a tool like `dd` to write the binary to an SD card:

{% include callout.html type="warning" title="Caution:" text="Be very careful with the dd command! Double-check /dev/sdX before running it to avoid overwriting important data." %}

{% highlight shell %}
dd if=boot.bin of=/dev/sdX bs=512 seek=2048
{% endhighlight %}

# Conclusion

In this post, we’ve built the foundation for **armos**. We’ve installed and configured the ARM toolchain, set up QEMU 
to emulate our target board, organized our project directory for clarity and scalability, and even wrote a simple 
bootloader to jumpstart our operating system. With these critical components in place, you’re now ready to embark on 
the next steps—enhancing the bootloader, adding essential kernel functionalities, and ultimately constructing a 
full-fledged minimalist OS. 
