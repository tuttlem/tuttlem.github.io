---
layout: post
title: Build your own x86 Kernel Part 1
date: 2025-10-05
comments: false
categories: [ osdev, systems, x86 ]
---

# Introduction

One of the best ways to learn how computers work is to get as close to the hardware as possible. Writing assembly 
language with no other tools or libraries really helps you to understand exactly what makes them tick. I'm building 
this article series to walk through the full setup of an [x86](https://en.wikipedia.org/wiki/X86) system to go from 
power on to a minimal running operating system.

I'll gradually build this from the ground up, introducing concepts as we go through these articles.

Today, we'll get all the tooling and build environment setup so we can develop comfortably.

# Tools

Before we begin, we need some tools installed.

* **QEMU** for virtualising the system that will run our operating system
* **NASM** to be our assembler
* **Make** to manage our build chain

Get these installed on your respective system, and we can get started getting the project directory setup.

# Project Setup

First up, let's create our project directory and get our Makefile and bootloader started.

{% highlight bash %}
mkdir byo_os
mkdir byo_os/boot

cd byo_os
{% endhighlight %}

# Boot loader

A boot loader is the very first piece of software that runs when a computer starts. Its job is to prepare the CPU and
memory so that a full operating system can take over. When the machine powers on, the **BIOS** (or **UEFI**) firmware
looks for a bootable program and transfers control to it.

In this tutorial we’re building a **BIOS-style boot loader**.
When a machine boots in legacy BIOS mode, the firmware reads the **first 512 bytes** of the boot device — called the
boot sector — into memory at address 0x7C00 and jumps there. Those 512 bytes must end with the magic signature
0xAA55, which tells the BIOS that this sector is bootable. From that point, our code is executing directly on the CPU
in **16-bit real mode**, with no operating system or filesystem support at all.

Modern systems use UEFI, which is the successor to BIOS. UEFI firmware looks for a structured executable (a
**PE/COFF** file) stored on a FAT partition and provides a much richer environment — including APIs for disk I/O,
graphics, and memory services. It’s powerful, but it’s also more complex and hides many of the low-level details we
want to understand.

Starting with BIOS keeps things simple: one sector, one jump, and full control. Once we’ve built a working
real-mode boot loader and kernel, it’ll be easy to explore a UEFI variant later because the CPU initialization concepts
remain the same — only the firmware interface changes.

Here is our first boot loader.

{% highlight nasm %}
; ./boot/boot.asm

ORG 0x7C00                  ; our code starts at 0x7C00
BITS 16                     ; we're in 16-bit real mode

main:
  cli                       ; no interrupts
  hlt                       ; stop the processor

.halt:
  jmp   .halt

times 510-($-$$) db 0       ; pad out to 510 bytes
dw 0AA55h                   ; 2 byte signature
{% endhighlight %}

Our boot loader **must** be `512` bytes. We ensure that it is with `times 510-($-$$) db 0`. This directive pads our 
boot loader out to `510` bytes, leaving space for the final 2 signature bytes `dw 0AA55h` which all boot loaders must 
finish with.

# Building

With this code written, we need to be able to build and run it. Using a `Makefile` is an easy way to wrap up all of 
these actions so we don't need to remember all of the build steps.

{% highlight makefile %}
CC64 = x86_64-elf-gcc
LD64 = x86_64-elf-ld
OBJCOPY = x86_64-elf-objcopy
NASM = nasm

OBJDIR = build

all: os.img

$(OBJDIR):
	mkdir -p $(OBJDIR)

boot/boot.bin: boot/boot.asm
	$(NASM) -f bin $< -o $@

os.img: boot/boot.bin 
	rm -f $@
	dd if=boot/boot.bin of=$@ bs=512 count=1 conv=notrunc
	truncate -s $$((32*512)) $@

run: os.img
	qemu-system-x86_64 -drive file=os.img,format=raw -serial stdio -debugcon file:debug.log -global isa-debugcon.iobase=0xe9 -display none -no-reboot -no-shutdown -d guest_errors,cpu_reset -D qemu.log

clean:
	rm -rf build os.img boot/*.bin *.log
{% endhighlight %}

This will build a `boot/boot.bin` for us, and it will also pack it into an `os.img` which we will use to run our os.

The key lines in making the os image are the `dd` and `truncate`. They get our 512 byte boot sector first in the image, 
and then the `truncate` extends the image to **32 sectors** (16 KB total) by padding it with zeros. The extra space 
simulates a small disk, leaving room for later stages like a kernel or filesystem. The first 512 bytes remain our boot 
sector; the rest is just blank space the BIOS ignores for now.

When we issue `make run`, this turns into this:

{% highlight bash %}
qemu-system-x86_64 -drive file=os.img,format=raw \
                   -serial stdio \
                   -debugcon file:debug.log \
                   -global isa-debugcon.iobase=0xe9 \
                   -display none \
                   -no-reboot \
                   -no-shutdown \
                   -d guest_errors,cpu_reset \
                   -D qemu.log
{% endhighlight %}

* `-drive file=os.img,format=raw` Attach a raw disk image as the primary drive. When QEMU boots in BIOS mode, it loads the first sector (the MBR) if it ends with the signature `0xAA55`.
* `-serial stdio` redirect the guest’s `COM1` serial port (I/O `0x3F8`) to this terminal’s stdin/stdout, so any serial output from the guest appears in your console.
* `-debugcon file:debug.log` will dump the debug console into a file called `debug.log`
* `-global isa-debugcon.iobase=0xe9` Map QEMU’s simple **debug console** to I/O port `0xE9`. Any `out 0xE9, al` from your code is appended to `debug.log`
* `-display none` Disables the graphical display window. No VGA text output will be visible unless you use `-nographic`, serial, or the `0xE9` debug console
* `-no-reboot` on a guest reboot request, do not reboot; QEMU exits instead (handy for catching triple-fault loops).
* `-no-shutdown` on a guest power-off, don’t quit QEMU; keep it running so logs/console remain available.
* `-d guest_errors,cpu_reset` Enables QEMU’s internal debug logging for guest faults and CPU resets (for example, triple faults). The messages are written to the file specified by `-D` 
* `-D qemu.log` Write QEMU’s debug logs (from `-d`) to `qemu.log` instead of stderr.

We will plan to print with BIOS INT `0x10` later on, so this instruction will evolve as we go.

# Running

Let's give it a go.

By running `make` you should see output like this:

{% highlight plain %}
➜  make
nasm -f bin boot/boot.asm -o boot/boot.bin
rm -f os.img
dd if=boot/boot.bin of=os.img bs=512 count=1 conv=notrunc
1+0 records in
1+0 records out
512 bytes copied, 9.4217e-05 s, 5.4 MB/s
truncate -s $((32*512)) os.img
{% endhighlight %}

You can then run this `make run`:

{% highlight plain %}
➜  make run
qemu-system-x86_64 -drive file=os.img,format=raw -serial stdio -debugcon file:debug.log -global isa-debugcon.iobase=0xe9 -display none -no-reboot -no-shutdown -d guest_errors,cpu_reset -D qemu.log
{% endhighlight %}

And there you have it. Our bootloader ran very briefly, and now our machine is halted.

# Conclusion

We've managed to setup our build environment and get a very simple boot loader being executed by QEMU. In further 
tutorials we'll look at integrating the serial COM1 ports so that we can get some signs of life reported out to the 
console.

