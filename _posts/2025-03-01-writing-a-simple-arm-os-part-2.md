---
layout: post
title: Writing A Simple ARM OS - Part 2
date: 2025-03-01
comments: false
categories: [ arm, os, asm ]
---

# Introduction

In the [first article of this series]({% post_url 2025-02-25-writing-a-simple-arm-os-part-1 %}), we built a basic ARM 
bootloader and ran it in QEMU. However, debugging without output can be frustrating. In this part, we’ll set up 
[UART](https://en.wikipedia.org/wiki/Universal_asynchronous_receiver-transmitter) 
(Universal Asynchronous Receiver-Transmitter) to send simple text messages from our OS.

UART is a fundamental interface used for **serial communication** in embedded systems. It allows us to 
**send and receive characters** over a hardware interface, making it useful for early-stage debugging before more 
complex peripherals like displays or networking are available.

By the end of this article, we’ll have a **basic UART driver** that prints text to the terminal using QEMU.

# What is UART?

UART is a hardware component that enables serial communication by **sending and receiving data one bit at a time**. It 
typically operates over two wires:
- **TX (Transmit)**: Sends data.
- **RX (Receive)**: Receives data.

In most ARM-based systems, **UART is memory-mapped**, meaning we can control it by writing to specific memory addresses.

## Configuring UART in QEMU

We’ll use [PL011 UART](https://developer.arm.com/documentation/ddi0183/latest/), a common ARM serial interface. QEMU 
provides an emulated PL011 UART at a known memory address, allowing us to send text output to the terminal.

To enable UART output, we need to run QEMU with the following command:

{% highlight bash %}
qemu-system-arm -M versatilepb -kernel build/armos.elf -serial stdio -nographic
{% endhighlight %}

- `-serial stdio` redirects UART output to our terminal.
- `-nographic` ensures we run without a graphical display.

You may receive an error message like the following:

{% highglight text %}
qemu-system-arm: -serial stdio: cannot use stdio by multiple character devices
qemu-system-arm: -serial stdio: could not connect serial device to character backend 'stdio'
{% endhighlight %}

This is just telling you that you're already redirecting the serial output because of the `-nographic` switch. If you 
do see this, you're free to simply drop the `-serial stdio`.

With this setup, once we implement our UART driver, we’ll see printed text appear in the terminal.

# Writing a UART Driver

## PL011 UART Memory-Mapped Registers

The PL011 UART controller is accessible via **memory-mapped I/O registers**. The key register we need for output is at 
address `0x101f1000` and is called the **UART Data Register (DR)**. Writing a byte to this register sends a character 
over UART.

## Implementing UART Functions

We create a new file, `uart.s`, in the `asm/` directory:

{% highlight asm %}
.equ UART0_DR, 0x101f1000

.section .text
.global uart_putc
.global uart_puts

uart_putc:
    STRB r0, [r1]        @ Store byte from r0 into UART data register
    BX lr

uart_puts:
    LDR r1, =UART0_DR
1:
    LDRB r0, [r2], #1   @ Load byte from string, increment pointer
    CMP r0, #0          @ Check for null terminator
    BEQ 2f              @ Branch to 2 if we are done
    BL uart_putc        @ If not, call putc
    B 1b                @ Keep looping
2:
    BX lr               @ Return to caller
{% endhighlight %}

- **`uart_putc(char)`**: Sends a single character to the UART register.
- **`uart_puts(string)`**: Iterates through a null-terminated string and sends each character.

# Printing a Message from the Bootloader

Now that we have a UART driver, we can modify our bootloader (`bootstrap.s`) to print a message.

{% highlight asm %}
.section .text
.global _start

_start:
    LDR sp, =stack_top
    LDR r2, =hello_msg
    BL uart_puts
    B .

.data
hello_msg:
    .asciz "Hello, ARM World!\n"

.section .bss
.align 4
stack_top:
.space 1024
{% endhighlight %}

Here’s what’s happening:
- We **load the stack pointer** (`sp`).
- We **load the address of the string `hello_msg`** into `r2`.
- We **call `uart_puts`** to print the message.
- The program enters an infinite loop (`B .`).

# Updating the Makefile

We need to update our `Makefile` to include `uart.s`:

{% highlight makefile %}
AS = arm-none-eabi-as
LD = arm-none-eabi-ld
OBJCOPY = arm-none-eabi-objcopy

# Files and directories
ASM_SRCS = asm/bootstrap.s asm/uart.s
BUILD_DIR = build
TARGET = armos.elf

all: $(BUILD_DIR)/$(TARGET)

$(BUILD_DIR)/$(TARGET): $(ASM_SRCS)
	@mkdir -p $(BUILD_DIR)
	$(AS) -o $(BUILD_DIR)/bootloader.o asm/bootstrap.s
	$(AS) -o $(BUILD_DIR)/uart.o asm/uart.s
	$(LD) -Ttext 0x0 -o $(BUILD_DIR)/$(TARGET) $(BUILD_DIR)/bootloader.o $(BUILD_DIR)/uart.o
	$(OBJCOPY) -O binary $(BUILD_DIR)/$(TARGET) $(BUILD_DIR)/armos.bin

clean:
	rm -rf $(BUILD_DIR)
{% endhighlight %}

# Building

We can give our new modifications a build now.

{% highlight bash %}
make
{% endhighlight %}

If successful, you should see:

{% highlight text %}
arm-none-eabi-as -o build/bootloader.o asm/bootstrap.s
arm-none-eabi-as -o build/uart.o asm/uart.s
arm-none-eabi-ld -Ttext 0x0 -o build/armos.elf build/bootloader.o build/uart.o
arm-none-eabi-objcopy -O binary build/armos.elf build/armos.bin
{% endhighlight %}

# Running

We can run our new image now:

{% highlight bash %}
qemu-system-arm -M versatilepb -kernel build/armos.elf -nographic
{% endhighlight %}

Expected output:

{% highlight text %}
Hello, ARM World!
{% endhighlight %}

# Conclusion

We now have **basic UART output** working in our OS! This is a critical step because it allows us to debug our OS by 
printing messages. As always you find [the code](https://github.com/tuttlem/armos/releases/tag/part2) up in my GitHub 
repository.


With this foundational work in place, we’re one step closer to a functional ARM-based OS. Stay tuned for Part 3!

