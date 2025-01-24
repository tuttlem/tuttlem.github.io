---
layout: post
title: Writing an ARM Bootloader
date: 2025-01-24
comments: false
categories: [ "arm", "bootloader", "assembly" ]
---

# Introduction

In this blog post, we’ll dive into the fascinating world of [ARM](https://en.wikipedia.org/wiki/ARM_architecture_family) 
assembly programming by writing and running a basic bootloader. ARM’s 
dominance in mobile and embedded systems makes it an essential architecture to understand for developers working with 
low-level programming or optimization. 

We're going to use QEMU, an open-source emulator, we can develop and test our code right on your PC. So we won't need any
hardware (just yet).

# What is ARM?

ARM, short for Advanced RISC Machine, is a family of Reduced 
Instruction Set Computing ([RISC](https://en.wikipedia.org/wiki/Reduced_instruction_set_computer)) architectures. 
ARM processors power billions of devices, from smartphones 
and tablets to embedded systems and IoT devices. Its popularity stems from its energy efficiency and simplicity 
compared to complex instruction set computing ([CISC](https://en.wikipedia.org/wiki/Complex_instruction_set_computer)) 
architectures like x86.

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

# Setup

Before we begin, we need to setup our development and build environment. I'm using [Manjaro](https://manjaro.org/) so package 
names might be slightly different for your distro of choice.

*QEMU* emulates a variety of hardware architectures, including ARM.

{% highlight shell %}
sudo pacman -Ss qemu-system-arm
{% endhighlight %}

Now we need to install the ARM toolchain which will include the assembler (`as`), linker (`ld`), and other essential tools.

{% highlight shell %}
sudo pacman -S arm-none-eabi-gcc binutils
{% endhighlight %}

Now you should have everything you need to get going.

# Bootloader

Our goal is to write a minimal ARM assembly program that outputs “Hello, World!" via the UART interface.

## The Code

Here is the source code for our bootloader, saved as `boot.s`:

{% highlight assembly %}
.section .text
.global _start

_start:
    LDR R0, =0x101f1000     @ UART0 base address
    LDR R1, =message        @ Address of the message
    LDR R2, =message_end    @ Address of the end of the message

loop:
    LDRB R3, [R1], #1       @ Load a byte from the message and increment the pointer
    CMP R1, R2              @ Check if we’ve reached the end of the message
    BEQ done                @ If yes, branch to done
    STRB R3, [R0]           @ Output the character to UART0
    B loop                  @ Repeat for the next character

done:
    B done                  @ Infinite loop to prevent execution from going beyond

message:
    .asciz "Hello, World!\n"  @ Null-terminated string
message_end:
{% endhighlight %}

Breaking this down line by line, we get:

* `LDR R0, =0x101f1000`: Load the memory address of UART0 (used for serial output) into register R0.
* `LDR R1, =message`: Load the starting address of the message into R1.
* `LDR R2, =message_end`: Load the end address of the message into R2.

After this setup, we move into a loop. 

* Load a byte from the message (`R3`).
* Compare `R1` (current pointer) with `R2` (end of the message).
* Write the character to `UART0` and repeat.

Finally, we finish up with an infinite loop to prevent the program from running into uninitialized memory.

## Building

First we need to assemble the code into an object file:

{% highlight shell %}
arm-none-eabi-as -o boot.o boot.s
{% endhighlight %}

Next, we link the object file to produce an executable:

{% highlight shell %}
arm-none-eabi-ld -Ttext=0x10000 -o boot.elf boot.o
{% endhighlight %}

The `-Ttext=0x10000` flag specifies the memory address where the program will start executing. 

# Running

We can give our bootloader a go now using the `versatilepb` machine in QEMU:

{% highlight shell %}
qemu-system-arm -M versatilepb -nographic -kernel boot.elf
{% endhighlight %}

`-nographic` here redirects UART ouput to the terminal, so we should see:

{% highlight text %}
Hello, World!
{% endhighlight %}

## Debugging

If you run into problems with your program, you do have an option to attach `gdb` for debugging:

{% highlight shell %}
qemu-system-arm -M versatilepb -kernel boot.elf -S -gdb tcp::1234
{% endhighlight %}

You can then connect to gdb with the following:

{% highlight shell %}
arm-none-eabi-gdb boot.elf
target remote :1234
{% endhighlight %}

# Deployment

For deployment, we’ll use a Raspberry Pi as an example. This process is similar for other ARM-based boards.

## Flashing

First, we need to convert the ELF file to a raw binary format suitable for booting:

{% highlight shell %}
arm-none-eabi-objcopy -O binary boot.elf boot.bin
{% endhighlight %}

Use a tool like `dd` to write the binary to an SD card:

{% highlight shell %}
dd if=boot.bin of=/dev/sdX bs=512 seek=2048
{% endhighlight %}

## Running 

* Insert the SD card into the board.
* Power up the device and connect to its UART output (e.g., using a USB-to-serial adapter).
* You should see “Hello, World!" printed on the serial console.

# Conclusion

Congratulations! You’ve successfully written, emulated, and deployed a simple ARM bootloader. Along the way, you learned:

* How to write and debug ARM assembly.
* How to use QEMU for emulation.
* How to deploy code to real hardware.

From here, you can explore more advanced topics like interrupts, timers, or even writing a simple operating system kernel. The journey into ARM assembly has just begun!