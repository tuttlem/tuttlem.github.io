---
layout: post
title: Build your own x86 Kernel Part 2
date: 2025-10-05
comments: false
categories: [ osdev, systems, x86 ]
---

# Introduction

In our [previous post]({% post_url 2025-10-05-build-your-own-x86-kernel-part-1 %}) we successfully setup our development 
and build environment, booting a very basic boot loader in QEMU.

In today's post, we're going to add some serial integration so that we can see some "signs of life" when our boot 
loader runs.

# Stack Setup

Before moving on any further, it'll be a good move for us to setup a temporary stack. It'll live for as long as our 
boot loader lives. We know our boot code is loaded at `0x7C00`. The stack grows downward, so we place it below the boot 
sector at `0x7000`. This keeps it out of the way of our code/data and gives us space to work with. We disable 
interrupts while changing `SS:SP` (an interrupt during this window would push onto an uninitialized stack), then 
re-enable them once the stack and data segments are valid.

{% highlight nasm %}
main:
cli                   ; disable interrupts

xor   ax, ax
mov   ss, ax
mov   sp, 0x7000      ; stack at 0000:7000 (grows downward)

mov   ds, ax          ; DS = 0 so [label] addresses resolve correctly
mov   es, ax          ; ES = 0

cld                   ; string ops auto-increment

sti                   ; re-enable interrupts
{% endhighlight %}

We make sure we can't be interrupted while doing this, so we clear the interrupt flag with cli. Next, set up the 
stack so that `SS:SP` points to `0000:7000`. Making `ds` and `es` point to the same segment as our code `0000` 
simplifies things for us. `cld` makes sure that our `lods` and `stos` operations always count ascending. Finally, we 
re-enable interrupts.

It'll look something like this:

<div class="mermaid">
graph TB
    A0["0x0000 — 0x03FF<br/>IVT (Interrupt Vector Table)"]
    A1["0x0400 — ~0x04FF<br/>BDA (BIOS Data Area)"]
    A2["..."]
    S["0x7000 (SS:SP start)<br/>Stack top → grows downward"]
    GAP["0x7000 — 0x7BFF<br/>Gap (free space)"]
    BOOT["0x7C00 — 0x7DFF<br/>Boot sector (512 bytes)"]
    A3["... up to conventional memory"]
    A0 --> A1 --> A2 --> S --> GAP --> BOOT --> A3
</div>

# Serial

[UART](https://en.wikipedia.org/wiki/Universal_asynchronous_receiver-transmitter) (the serial port) is the early 
debugging channel that we'll use. It's the standard for debugging x86 and embedded work, so it's perfect for what we're 
doing.

## Registers

We write-to and read-from the UART registers to setup communications over serial. Here's each of the registers.

{% include callout.html type="info" title="COM1" text="is defined at a base-address of 0x3F8. The following map is an offset from that base." %}

| Offset | Description |
|--------|-------------|
| `+0`   | THR/RBR or DLL (when DLAB=1) — Transmit Holding / Receive Buffer / Divisor Latch Low |
| `+1`   | IER or DLM (when DLAB=1) — Interrupt Enable / Divisor Latch High |
| `+2`   | IIR/FCR — Interrupt ID / FIFO Control |
| `+3`   | LCR — Line Control (word length, parity, stop, DLAB) |
| `+4`   | MCR — Modem Control (DTR, RTS, OUT1, OUT2, LOOP) |
| `+5`   | LSR — Line Status (TX empty, RX ready, etc.) |
| `+6`   | MSR — Modem Status |
| `+7`   | SCR — Scratch |

## Init

We can now walk through the init code for the serial line.

{% highlight nasm %}
%define COM1 0x3F8

serial_init:
  ; 1) Disable UART-generated interrupts (clear IER)
  mov   dx, COM1 + 1
  xor   ax, ax            ; AL=0
  out   dx, al            ; IER = 0 (no UART IRQs)

  ; 2) Enable DLAB so we can set the baud rate divisor
  mov   dx, COM1 + 3
  mov   al, 0x80          ; LCR: DLAB=1
  out   dx, al

  ; 3) Set divisor to 1 -> 115200 baud (on standard PC clock)
  mov   dx, COM1 + 0
  mov   al, 0x01          ; DLL = 1
  out   dx, al
  mov   dx, COM1 + 1
  xor   al, al            ; DLM = 0
  out   dx, al

  ; 4) 8 data bits, no parity, 1 stop bit; clear DLAB to use data regs
  mov   dx, COM1 + 3
  mov   al, 0x03          ; LCR: 8N1 (DLAB=0)
  out   dx, al

  ; 5) Enable FIFO, clear RX/TX FIFOs, set 14-byte RX threshold
  mov   dx, COM1 + 2
  mov   al, 0xC7          ; FCR: 1100_0111b
  out   dx, al

  ; 6) Modem Control: assert DTR, RTS, and OUT2
  mov   dx, COM1 + 4
  mov   al, 0x0B          ; MCR: DTR|RTS|OUT2
  out   dx, al

  ret
{% endhighlight %}

Each of these steps is needed in the init phase:

* **IER=0 (no UART IRQs)**: We’re going to use polling (check LSR bits) in early boot, so we explicitly disable UART interrupts.
* **DLAB=1, set divisor**: Standard PC UART clock (1.8432 MHz / 16 = 115200). A divisor of 1 yields 115200 baud. Later you can choose 2 (57600), 12 (9600), etc.
* **LCR=0x03 (8N1)**: The classic “8 data bits, No parity, 1 stop.” Clearing DLAB returns access to THR/RBR/IER instead of the divisor latches.
* **FCR=0xC7**: Enables the FIFO, clears both FIFOs, and sets the RX trigger level to 14 bytes. (On 8250/16450 parts without FIFOs this is ignored—harmless.)
* **MCR=0x0B**: Asserts DTR and RTS so the other side knows we’re ready; sets OUT2, which on PCs typically gates the UART interrupt line (even if we aren’t using IRQs yet, OUT2 is commonly left on).

## Waiting

Because working with UART is asynchronous, we need to wait for the transmitter holding register is ready. So this waits 
for the **THR** empty (bit 5). 

{% highlight nasm %}
serial_wait_tx:
  push  ax
  push  dx

  mov   dx, COM1 + 5
.wait:
  in    al, dx
  test  al, 0x20
  jz    .wait
  
  pop   dx
  pop   ax
  ret
{% endhighlight %}

This is important when trying to write data out. 

We must wait until we're ready.

## `putc` and `puts`

We can now use `serial_wait_tx` before going to send a character.

The character that we put into `al` gets sent directly to our `COM1` which is at `0x3F8`.

{% highlight nasm %}
serial_putc:
  push  dx

  call  serial_wait_tx

  mov   dx, COM1
  out   dx, al

  pop   dx
  ret
{% endhighlight %}

Finally, we can use `putc` in a loop to implement a `puts` which iterates though the string at `si`, sending each of 
the chars to `serial_putc`.

{% highlight nasm %}
serial_puts:
  push  ax
  push  si

.next:
  lodsb
  test  al, al
  jz    .done

  call  serial_putc
  jmp   .next

.done:
  pop   si
  pop   ax

  ret
{% endhighlight %}

# Signs of life

Now that we have a way to integrate with the serial line, we can use it to prove signs of life in our bootloader.

After our stack is setup, we can start using these functions.

{% highlight nasm %}
  call  serial_init         ; initialize serial

  mov   si, msg_alive       ; si = our string to print
  call  serial_puts         ; print the string

  hlt                       ; halt

.halt:
  jmp   .halt

%include "boot/serial.asm"  ; serial functions

msg_alive db "Serial communications are alive!", 0

times 510-($-$$) db 0
dw 0AA55h
{% endhighlight %}

To clean up the boot code, I tucked all of the serial communication code away into an `asm` file of its own. It's still 
assembled as part of the `boot.asm` as it's just text included.

Setting this up for a run, you should see a message in your console.

{% highlight plain %}
➜  make run
qemu-system-x86_64 -drive file=os.img,format=raw -serial stdio -debugcon file:debug.log -global isa-debugcon.iobase=0xe9 -display none -no-reboot -no-shutdown -d guest_errors,cpu_reset -D qemu.log
Serial communications are alive!
{% endhighlight %}

We are alive! 

# Conclusion

We have put ourselves in a very strong position with these recent additions. This is an invaluable debugging and 
diagnostic tool being able to write breadcrumbs into the console to check where execution has made it to.

We'll continue to build on this when we return for Stage2.
