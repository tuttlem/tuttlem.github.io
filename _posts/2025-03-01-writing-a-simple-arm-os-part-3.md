---
layout: post
title: Writing A Simple ARM OS - Part 3
date: 2025-03-01
comments: false
categories: [ arm, os, asm ]
---

# Introduction

In [Part 2]({% post_url 2025-03-01-writing-a-simple-arm-os-part-2 %}), we implemented a basic UART driver to send text 
output from our OS. We put some functions together that deal with single characters and strings. 

Today's article has a focus on some calling conventions, debugging, and hopefully making that UART driver a little 
more robust.

Let's take a hot lap of the ARM architecture first.

# Registers

ARM processors have a well-defined set of registers, which can be categorized based on their usage. Below is a 
breakdown of all the ARM registers, including their general purpose, special purpose, and system control registers.

## General purpose registers

These registers are used for storing temporary values and passing function arguments.

| Register     | Purpose                                                                      |
|--------------|------------------------------------------------------------------------------|
| `r0`–`r11`   | Function arguments & return values (general purpose)                         |
| `r12`	    | Intra-procedure scratch register (IP, sometimes used as a temporary register) |

{% include callout.html type="success" title="Note:" text="ARM conventions say that r0-r3 and r12 can be freely modified by functions, and the caller must save them if needed. r4-r11 should be restored by the function before returning." %}

## Special purpose registers

These registers serve specific roles in function calls, memory access, and system control.

| Register     | Purpose                                                                      |
|--------------|------------------------------------------------------------------------------|
| `r13` `sp`   | (Stack Pointer)	Points to the top of the current stack |
| `r14` `lr`   | (Link Register)	Stores return address for function calls |
| `r15` `pc`   | (Program Counter)	Holds the address of the next instruction to execute |

* `lr` (`r14`) is used for storing return addresses when calling functions.
* `pc` (`r15`) is automatically incremented as instructions execute. You can branch by writing directly to pc.

## Program Status Registers

The Current Program Status Register (CPSR) and Saved Program Status Register (SPSR) store flags and mode-related information.

| Register | Purpose | 
|----------|----------------|
| CPSR	| Holds flags, processor mode, and interrupt status |
| SPSR	| Stores CPSR when entering an exception mode |

Key CPSR flags (Condition Flags):
* `N` (Negative) – Set if the result of an operation is negative.
* `Z` (Zero) – Set if the result of an operation is zero.
* `C` (Carry/Borrow) – Set if an operation results in a carry/borrow.
* `V` (Overflow) – Set if an arithmetic operation overflows.

Processor Mode Bits (`M[4:0]`):

* `0b10000` – User mode
* `0b10001` – FIQ (Fast Interrupt) mode
* `0b10010` – IRQ (Normal Interrupt) mode
* `0b10011` – Supervisor mode
* `0b10111` – Abort mode
* `0b11011` – Undefined instruction mode
* `0b11111` – System mode (privileged user mode)

## Banked Registers (Mode-Specific Registers)

ARM has banked registers that are only accessible in specific processor modes (e.g., IRQ, FIQ). These registers allow 
fast context switching between different execution states.

| Mode	           | Extra Registers Available                            |
|-----------------|------------------------------------------------------|
| FIQ Mode	       | `r8_fiq` – `r14_fiq` (separate registers for FIQ context) |
| IRQ Mode	       | `r13_irq`, `r14_irq` (separate SP and LR for IRQ)        |
| Supervisor Mode	 | `r13_svc`, `r14_svc` (separate SP and LR for SVC)        |
| Abort Mode	     | `r13_abt`, `r14_abt`                                     |
| Undefined Mode	 | `r13_und`, `r14_und`                                     |

### Why banked registers?

* Interrupt handlers can run without disturbing normal user-space registers.
* Faster execution because it eliminates the need to save/restore shared registers.

## Debug Registers (ARMv7+)
   
ARM processors often include special registers for debugging, including breakpoints and watchpoints.

|Register|	Purpose|
|-------|--------------------|
|`DBGDSCR`|	Debug Status and Control Register|
|`DBGBVR`|	Breakpoint Value Register|
|`DBGBCR`|	Breakpoint Control Register|
|`DBGWVR`|	Watchpoint Value Register|
|`DBGWCR`|	Watchpoint Control Register|

# Understanding ARM Calling Conventions

ARM assembly follows a convention for passing function parameters and preserving registers:

- **Caller-saved registers (`r0-r3, r12`)**: These are freely used by functions and must be saved by the caller if needed.
- **Callee-saved registers (`r4-r11, lr`)**: A function must **preserve and restore** these if it modifies them.
- **Return values**: `r0` holds the return value.

Understanding this is key to writing reliable functions.

## Upgrading `uart_puts`

We're going to upgrade our `uart_puts` to "behave" a little nicer for us.

{% highlight asm %}
uart_puts:
    push {lr}              @ Save return address

next_char:
    ldrb r1, [r0], #1      @ Load byte from string and increment pointer
    cmp r1, #0             @ Check if null terminator
    beq done               @ If so, return

wait_uart:
    ldr r2, =UART0_FR      @ Load address of UART flag register
    ldr r3, [r2]           @ Read UART flag register
    tst r3, #TXFF          @ Check if TX FIFO is full
    bne wait_uart          @ If full, wait

    ldr r2, =UART0_DR      @ Load address of UART data register
    str r1, [r2]           @ Write character to UART
    b next_char            @ Process next character

done:
    pop {lr}               @ Restore return address
    bx lr                  @ Return
{% endhighlight %}

Let's break this down piece by piece.

We save off `lr` (the link register) which is our return address from where we were called.

{% highlight asm %}
uart_puts:
    push {lr}              @ Save return address
{% endhighlight %}

`ldrb` takes the next source byte from our string, and we check if we're finished. `next_char` is the loop point that 
we come back to, to process the remainder of the string.

{% highlight asm %}
next_char:
    ldrb r1, [r0], #1      @ Load byte from string and increment pointer
    cmp r1, #0             @ Check if null terminator
    beq done               @ If so, return
{% endhighlight %}

Next we wait for the UART buffer in case it's full

{% highlight asm %}
wait_uart:
    ldr r2, =UART0_FR      @ Load address of UART flag register
    ldr r3, [r2]           @ Read UART flag register
    tst r3, #TXFF          @ Check if TX FIFO is full
    bne wait_uart          @ If full, wait
{% endhighlight %}

Using `str` we write that source byte out to the UART data register, and continue to the next character in the loop.

{% highlight asm %}
    ldr r2, =UART0_DR      @ Load address of UART data register
    str r1, [r2]           @ Write character to UART
    b next_char            @ Process next character
{% endhighlight %}

We finish up by restoring `lr` before returning to where we were called from.

{% highlight asm %}
done:
    pop {lr}               @ Restore return address
    bx lr                  @ Return
{% endhighlight %}

# Debugging ARM Assembly

Debugging low-level assembly can be challenging. Here are some useful techniques to diagnose function issues.

## Print Debug Markers

One of the simplest ways to trace execution is to print special **debug characters** in the UART output:

{% highlight asm %}
MOV r0, #'!'
BL uart_putc    @ Print a debug marker to trace execution
{% endhighlight %}

If your code stops working after a certain point, inserting markers helps pinpoint **where execution breaks**. 

## Step Through Execution in QEMU

QEMU provides debugging features that let us **step through execution**. Start QEMU with GDB support:

{% highlight bash %}
qemu-system-arm -M versatilepb -kernel build/armos.elf -S -gdb tcp::1234 -nographic
{% endhighlight %}

Then, in another terminal, start GDB:

{% highlight bash %}
gdb-multiarch -ex "target remote localhost:1234" -ex "symbol-file build/armos.elf"
{% endhighlight %}

You can now use GDB to **step through instructions**, inspect register values, and find where execution goes wrong:

- `layout asm` – View assembly.
- `info registers` – Check register values.
- `si` – Step instruction-by-instruction.

### Dump Memory to Check String Pointers

If your function is reading garbage characters, your pointer might be wrong. Dump memory to check:

{% highlight bash %}
x/20xb 0x100000  # View first 20 bytes at memory location 0x100000
{% endhighlight %}

This helps verify if `r4` is pointing to the correct string.

# Conclusion

In this part, we focused a little bit more on assembly language and some of the calling conventions.

The code for this article can be found up [in the github repo](https://github.com/tuttlem/armos/releases/tag/part3).