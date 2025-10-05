---
layout: post
title: Build your own x86 Kernel Part 4
date: 2025-10-05
comments: false
categories: [ osdev, systems, x86 ]
---

# Introduction

In [Part 3]({% post_url 2025-10-05-build-your-own-x86-kernel-part-3 %}) finalised out boot loader, so that it now 
successfully loads Stage 2 for us. In this post, we'll focus on setting the system so that we unlock the more advanced 
features. 

Inside of Stage 2 we'll look at setting up the following:

* Enable the A20 line
* Set up a Global Descriptor Table (GDT)
* Switch to 32-bit Protected Mode

By the end of this article, we'll at least be in 32-bit protected mode.

# A20 Line

Before we can enter 32-bit protected mode, we need to enable the A20 line.

Back in the original **Intel 8086**, there were only 20 address lines — A0 through A19 — meaning it could address 
**1 MiB** of memory (from `0x00000` to `0xFFFFF`). When Intel introduced the **80286**, it gained more address lines 
and could access memory above 1 MiB. However, to remain compatible with older DOS software that relied on address 
wrap-around (where `0xFFFFF + 1` rolled back to `0x00000`), IBM added a hardware gate: **A20**.

When the **A20 line is disabled**, physical address bit 20 is forced to 0.
So addresses “wrap” every 1 MiB — `0x100000` looks the same as `0x000000`.

When A20 is enabled, memory above 1 MiB becomes accessible.
Protected-mode code, paging, and modern kernels all assume that A20 is on.

## Enabling A20

To enable the A20 Line, we use the **Fast A20 Gate** (port `0x92`). 

Most modern systems and emulators expose bit 1 of port `0x92` (the “System Control Port A”) as a direct A20 enable bit.

* **Bit 0** — system reset (don’t touch this)
* **Bit 1** — A20 gate (1 = enabled)

We add the following to do this:

{% highlight nasm %}
%define A20_GATE  0x92

in    al, A20_GATE      ; read system control port A
or    al, 0x02          ; set bit 1 (A20 enable)
and   al, 0xFE          ; clear bit 0 (reset)
out   A20_GATE, al
{% endhighlight %}

# Global Descriptor Table (GDT)

When the CPU is in **real mode**, memory addressing is done through **segment:offset** pairs. Each segment register 
(`CS`, `DS`, `SS`, etc.) represents a base address (shifted left by 4), and the offset is added to that. This gives you 
access to 1 MiB of address space — the legacy 8086 model.

When we switch to **protected mode**, the segmentation model changes. Instead of using raw segment values, each segment 
register now holds a selector — an index into a table called the **Global Descriptor Table (GDT)**.

The GDT tells the CPU what each segment means:
* Its base address
* size (limit)
* access rights
* flags like “code or data”, “read/write”, or “privilege level”

The descriptor layout in 32-bit mode looks like this:

| Bits | Field | Description |
|------|-------|-------------|
| 0-15 | Limit (low) | Segment limit (low 16 bits) |
| 16-31 | Base (low) | Segment base address (low 16 bits) |
| 32-39 | Base (mid) | Segment base (middle 8 bits) |
| 40-47 | Access Byte | Type, privilege level, presence |
| 48-51 | Limit (high) | High 4 bits of segment limit |
| 52-55 | Flags | Granularity, 32-bit flag, etc. |
| 56-63 | Base (high) | Segment base (high 8 bits) |


In our boot setup, we’ll create a very simple GDT with:
* A **null descriptor** (required; selector 0 is invalid by design).
* A **code segment descriptor** — flat 4 GiB region, readable, executable.
* A **data segment descriptor** — flat 4 GiB region, readable, writable.

This gives us a **flat memory model**, where all segments start at base 0 and cover the entire address space.
That makes protected mode addressing behave almost like real mode linear memory, simplifying everything until paging 
and virtual memory come later.

Once that GDT is loaded with `lgdt`, we can safely set the PE (Protection Enable) bit in `CR0` and perform a far jump 
into 32-bit protected mode code.

## Defining the GDT

We define our GDT as three quad words. One for null, one for code, and one for data.

{% highlight nasm %}
align 8
; --- GDT for entering 32-bit PM (null, code, data) ---
gdt32:
    dq 0x0000000000000000         ; null
    dq 0x00CF9A000000FFFF         ; 0x08: 32-bit code, base=0, limit=4GiB
    dq 0x00CF92000000FFFF         ; 0x10: 32-bit data, base=0, limit=4GiB

gdt32_desc:
    dw gdt32_end - gdt32 - 1      ; limit = (size of GDT - 1)
    dd gdt32                      ; base  = address of GDT
gdt32_end:
{% endhighlight %}

Breaking down the 32-bit code GDT:

{% highlight plain %}
0x00CF9A000000FFFF
{% endhighlight %}

If we split this into bytes (little-endian in memory):

{% highlight plain %}
[FFFF] [0000] [00][9A] [CF][00]
{% endhighlight %}

We can now start to map these to the fields:

| Field                  | Value | Meaning |
|------------------------|-------|----------|
| Limit (low 16)         | `0xFFFF` | segment limit = 0xFFFF |
| Base (low 16)          | `0x0000` | base = 0x00000000 |
| Base (mid 8)           | `0x00`   | base = 0x00000000 |
| Access Byte            | `0x9A` | flags that define "code, ring 0, present" |
| Limit (high 4) + flags | `0xCF` | limit high nibble=0xF, flags=0xC |
| Base (high 8)          | `0x00` | base = 0x00000000 |                     

The “limit” of `0xFFFF` and granularity bit (`G=1`) combine to make the segment effectively 4 GiB in size 
(0xFFFFF × 4 KiB pages = 4 GiB).

## Loading the GDT

Now that we have our GDT defined, we can use `lgdt` to load it.

{% highlight nasm %}
cli
lgdt  [gdt32_desc]
mov   eax, cr0
or    eax, 1                   ; CR0.PE=1
mov   cr0, eax
{% endhighlight %}

The operand to `lgdt` wants to see a 16bit limit first, and then a 32-bit linear address (in 32-bit mode) to where the 
GDT starts.

## Protected Mode

With the GDT now loaded, we're free to push over to protected mode. This is 32-bit protected mode, so we're jumping into 
code that needs the `[BITS 32]` directive.

{% highlight nasm %}
  ; selectors: 0x08 = code32, 0x10 = data32
  jmp   0x08:pm_entry            ; far jump to load 32-bit CS

[BITS 32]
pm_entry:
  mov   ax, 0x10                 ; 0x10 = data32
  mov   ds, ax
  mov   es, ax
  mov   ss, ax
  mov   fs, ax
  mov   gs, ax
  mov   esp, 0x90000             ; temporary 32-bit stack  

.hang:
  hlt
  jmp   .hang
{% endhighlight %}

We make our *far* jump into 32-bit land. This jump both updates `CS` and flushes the prefetch 
queue — it’s the required way to officially enter protected mode.

Immediately we set all of our segment selectors to `0x10` which is data GDT entry.

We're now in 32-bit protected mode.

# Stage 2 (full listing)

Our current code for Stage 2 now looks like this:

{% highlight nasm %}
; ---------------------------------------------------------
; boot/stage2.asm — loaded by MBR at 0000:8000 (LBA 1..16)
; ---------------------------------------------------------
BITS 16
ORG  0x8000

%define A20_GATE          0x92

start2:
  cli
  xor   ax, ax
  mov   ds, ax        ; ds = 0 so labels assembled with ORG work as absolute
  mov   es, ax
  cld                 ; count upwards
  sti

  call  serial_init

  mov   si, stage2_msg
  call  serial_puts

  in    al, A20_GATE          ; A20 fast
  or    al, 0x02
  and   al, 0xFE
  out   A20_GATE, al

  mov   si, a20_msg
  call  serial_puts

  cli
  lgdt  [gdt32_desc]
  mov   eax, cr0
  or    eax, 1                   ; CR0.PE=1
  mov   cr0, eax

  mov   si, gdt_msg
  call  serial_puts

  ; selectors: 0x08 = code32, 0x10 = data32
  jmp   0x08:pm_entry            ; far jump to load 32-bit CS

stage2_msg db "Stage2: OK", 13, 10, 0
a20_msg    db "A20 Line: Enabled", 13, 10, 0
gdt_msg    db "GDT: Loaded", 13, 10, 0

%include "boot/serial16.asm"

[BITS 32]
pm_entry:
  mov   ax, 0x10                 ; 0x10 = data32
  mov   ds, ax
  mov   es, ax
  mov   ss, ax
  mov   fs, ax
  mov   gs, ax
  mov   esp, 0x90000             ; temporary 32-bit stack  

  mov   esi, pm_msg
  call  serial_puts32

.hang:
  hlt
  jmp   .hang


align 8
; --- GDT for entering 32-bit PM (null, code, data) ---
gdt32:
    dq 0x0000000000000000         ; null
    dq 0x00CF9A000000FFFF         ; 0x08: 32-bit code, base=0, limit=4GiB
    dq 0x00CF92000000FFFF         ; 0x10: 32-bit data, base=0, limit=4GiB

gdt32_desc:
    dw gdt32_end - gdt32 - 1
    dd gdt32
gdt32_end:

pm_msg db "Entered protected mode ...", 13, 10, 0

%include "boot/serial32.asm"
{% endhighlight %}

## Notes

I've had to duplicate the serial assembly file. Originally it was 16 bits only, but now we need 32-bit support.

These routines look alot like their 16-bit counterparts:

{% highlight nasm %}
; ---------------------------------------------------------
; serial32.asm — COM1 (0x3F8) UART helpers for 32-bit PM
; ---------------------------------------------------------
[BITS 32]

%define COM1 0x3F8
; LSR bits: 0x20 = THR empty, 0x40 = TSR empty

; init: 115200 8N1, FIFO on
serial_init32:
    push eax
    push edx
    ; IER=0 (disable UART interrupts)
    mov  dx, COM1 + 1
    xor  eax, eax
    out  dx, al
    ; DLAB=1
    mov  dx, COM1 + 3
    mov  al, 0x80
    out  dx, al
    ; divisor = 1 (DLL=1, DLM=0)
    mov  dx, COM1 + 0
    mov  al, 0x01
    out  dx, al
    mov  dx, COM1 + 1
    xor  al, al
    out  dx, al
    ; 8N1, DLAB=0
    mov  dx, COM1 + 3
    mov  al, 0x03
    out  dx, al
    ; FIFO enable/clear, 14-byte trigger
    mov  dx, COM1 + 2
    mov  al, 0xC7
    out  dx, al
    ; MCR: DTR|RTS|OUT2
    mov  dx, COM1 + 4
    mov  al, 0x0B
    out  dx, al
    pop  edx
    pop  eax
    ret

; wait until THR empty
serial_wait_tx32:
    push eax
    push edx
    mov  dx, COM1 + 5
.wait:
    in   al, dx
    test al, 0x20
    jz   .wait
    pop  edx
    pop  eax
    ret

; putc: AL = character
serial_putc32:
    push edx
    call serial_wait_tx32
    mov  dx, COM1
    out  dx, al
    pop  edx
    ret

; putc with '\n' -> "\r\n"
serial_putc_nl32:
    cmp  al, 10              ; '\n'
    jne  .send
    push eax
    mov  al, 13              ; '\r'
    call serial_putc32
    pop  eax
.send:
    jmp  serial_putc32

; puts: ESI -> zero-terminated string
serial_puts32:
    push eax
    push esi
.next:
    lodsb                    ; AL = [ESI], ESI++
    test al, al
    jz   .done
    call serial_putc_nl32
    jmp  .next
.done:
    pop  esi
    pop  eax
    ret
{% endhighlight %}

# Running

Getting this built and running now, we can see that we're successfully in 32-bit protected mode.

{% highlight plain %}
➜ make run  
qemu-system-x86_64 -drive file=os.img,format=raw,if=ide,media=disk -serial stdio -debugcon file:debug.log -global isa-debugcon.iobase=0xe9 -display none -no-reboot -no-shutdown -d guest_errors,cpu_reset -D qemu.log
Booting ...
Starting Stage2 ...
Stage2: OK
A20 Line: Enabled
GDT: Loaded
Entered protected mode ...
{% endhighlight %}

# Conclusion

We’ve now built the minimal foundation of a protected-mode operating system: flat memory model, GDT, and a working 
serial console. From this point on, we can start using true 32-bit instructions and data structures. In the next post, 
we’ll extend this with an Interrupt Descriptor Table (IDT), Programmable Interrupt Timer (PIT), and paging, preparing 
the system for 64-bit long mode.
