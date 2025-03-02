---
layout: post
title: Writing A Simple ARM OS - Part 4
date: 2025-03-02
comments: false
categories: [ arm, os, asm ]
---

# Introduction

In [Part 3]({% post_url 2025-03-01-writing-a-simple-arm-os-part-3 %}), we explored ARM calling conventions, debugging, 
and cleaned up our UART driver. While assembly has given us fine control over the hardware, writing an entire OS in 
assembly would be painful.

It's time to enter **C land**.

This post covers:
- Modifying the **bootloader** to transition from assembly to C
- Updating the **UART driver** to be callable from C
- Writing our **first C function** (`kmain()`)
- Adjusting the **Makefile and linker script** for C support

# Booting into C

We still need a bit of assembly to set up the stack and call `kmain()`. Let's start by modifying our bootloader.

## Updated `bootloader.s`

{% highlight asm %}
.section .text
.global _start

_start:
    LDR sp, =stack_top  @ Set up the stack
    BL kmain            @ Call the C kernel entry function
    B .                 @ Hang forever

.section .bss
.align 4
stack_top:
.space 1024
{% endhighlight %}

### What's changed?
- We **load the stack pointer** (`sp`) before calling `kmain()`. This ensures C has a valid stack to work with.
- We **branch-and-link (BL) to `kmain()`**, following ARM's calling conventions.
- The infinite loop (`B .`) prevents execution from continuing into unknown memory if `kmain()` ever returns.

With this setup, execution will jump to `kmain()`—which we’ll define next.

# Our First C Function: `kmain()`

Now that we can transition from assembly to C, let’s create our first function.

## `kmain.c`

{% highlight c %}
#include "uart.h"

void kmain() {
    uart_puts("Hello from C!\n");
    while (1);
}
{% endhighlight %}

### What's happening?
- We **include** our `uart.h` header so we can call `uart_puts()`.
- `kmain()` prints `"Hello from C!"` using our **UART driver**.
- The infinite `while(1);` loop prevents execution from continuing into unknown territory.

At this point, our OS will **boot from assembly, call `kmain()`, and print text using our UART driver**—but we need to 
make a few more changes before this compiles.

# Making the UART Driver Callable from C

Right now, `uart_puts` and `uart_putc` are assembly functions. To call them from C, we need to:
1. Ensure they follow the **ARM calling convention**.
2. Declare them properly in a **header file**.

## `uart.h` (Header File)

{% highlight c %}
#ifndef UART_H
#define UART_H

void uart_putc(char c);
void uart_puts(const char *str);

#endif
{% endhighlight %}

## Updated `uart.s`

{% highlight asm %}
.section .text
.global uart_putc
.global uart_puts

uart_putc:
    PUSH {lr}
    ldr r1, =0x101f1000  @ UART0 Data Register
    STRB r0, [r1]        @ Store byte
    POP {lr}
    BX lr

uart_puts:
    PUSH {lr}

next_char:
    LDRB r1, [r0], #1    @ Load byte from string
    CMP r1, #0
    BEQ done

wait_uart:
    LDR r2, =0x101f1018  @ UART0 Flag Register
    LDR r3, [r2]
    TST r3, #0x20        @ Check if TX FIFO is full
    BNE wait_uart

    LDR r2, =0x101f1000  @ UART0 Data Register
    STR r1, [r2]         @ Write character
    B next_char

done:
    POP {lr}
    BX lr
{% endhighlight %}

### How this works:
- **Function names** are declared `.global` so they are visible to the linker.
- **`uart_putc(char c)`**
  - Expects a character in `r0` (following ARM's C calling convention).
  - Writes `r0` to the UART data register.
- **`uart_puts(const char *str)`**
  - Expects a pointer in `r0`.
  - Iterates through the string, sending each character until it reaches the null terminator (`\0`).
- **Preserving Registers**
  - `PUSH {lr}` ensures `lr` is restored before returning.

# Updating the Build System

To compile both assembly and C, we need to adjust the **Makefile** and **linker script**.

## Updated `Makefile`

{% highlight make %}
# Makefile for the armos project

# Cross-compiler tools (assuming arm-none-eabi toolchain)
AS = arm-none-eabi-as
CC = arm-none-eabi-gcc
LD = arm-none-eabi-ld
OBJCOPY = arm-none-eabi-objcopy

# Compiler and assembler flags
CFLAGS = -ffreestanding -nostdlib -O2 -Wall -Wextra
ASFLAGS =
LDFLAGS = -T linker.ld -nostdlib

# Files and directories
BUILD_DIR = build
TARGET = armos.elf
BIN_TARGET = armos.bin

# Source files
ASM_SRCS = asm/bootloader.s asm/uart.s
C_SRCS = src/kmain.c
OBJS = $(BUILD_DIR)/bootloader.o $(BUILD_DIR)/uart.o $(BUILD_DIR)/kmain.o

# Build rules
all: $(BUILD_DIR)/$(BIN_TARGET)

$(BUILD_DIR):
	@mkdir -p $(BUILD_DIR)

# Assemble the bootloader and UART
$(BUILD_DIR)/bootloader.o: asm/bootloader.s | $(BUILD_DIR)
	$(AS) $(ASFLAGS) -o $@ $<

$(BUILD_DIR)/uart.o: asm/uart.s | $(BUILD_DIR)
	$(AS) $(ASFLAGS) -o $@ $<

# Compile the C source file
$(BUILD_DIR)/kmain.o: src/kmain.c | $(BUILD_DIR)
	$(CC) $(CFLAGS) -c -o $@ $<

# Link everything together
$(BUILD_DIR)/$(TARGET): $(OBJS)
	$(LD) $(LDFLAGS) -o $@ $(OBJS)

# Convert ELF to binary
$(BUILD_DIR)/$(BIN_TARGET): $(BUILD_DIR)/$(TARGET)
	$(OBJCOPY) -O binary $< $@

# Clean build artifacts
clean:
	rm -rf $(BUILD_DIR)
{% endhighlight %}

# Updating the Linker Script

The linker script ensures that `kmain()` and our code are properly loaded in memory.

## Updated `linker.ld`

{% highlight ld %}
ENTRY(_start)

SECTIONS
{
    . = 0x10000; /* Load address of the kernel */

    .text : {
        *(.text*)
    }

    .rodata : {
        *(.rodata*)
    }

    .data : {
        *(.data*)
    }

    .bss : {
        *(COMMON)
        *(.bss*)
    }
}
{% endhighlight %}

### Key Changes:
- **Code starts at `0x10000`**, ensuring it is loaded correctly.
- `.text`, `.rodata`, `.data`, and `.bss` sections are properly defined.

# Build

Now that all of these changes in place, we can `make` our kernel and run it. If everything has gone to plan, you should 
see our kernel telling us that it's jumped to C.

{% highlight text %}
Hello from C!
{% endhighlight %}

# Conclusion

We've successfully transitioned from **pure assembly** to using **C** for higher-level logic, while keeping low-level 
hardware interaction in **assembly**.

The code for this article is available in the [GitHub repo](https://github.com/tuttlem/armos/releases/tag/part4).
