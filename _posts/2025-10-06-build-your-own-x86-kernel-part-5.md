---
layout: post
title: Build your own x86 Kernel Part 5
date: 2025-10-06
comments: false
categories: [ osdev, systems, x86 ]
---

# Introduction

In [the last post]({% post_url 2025-10-05-build-your-own-x86-kernel-part-4 %}) we landed in 32-bit protected mode. 
Before moving on though, let's tidy up this project a little bit so we can start managing larger pieces of code cleanly. 

So far, everything has been hard-wired: `ORG` directives, absolute addresses, and flat binaries. That works for a boot 
sector, but we're about to deal with paging, higher memory, and higher-level languages - so we need a proper linker 
script to take over memory layout.

# Linker

A new file has been added to manage how the result of `stage2.asm` is laid out, and we call it `stage2.ld`. 

The linker script gives us precise control over where each section of code and data ends up in memory — something `ORG` 
statements alone can’t handle once we start mixing multiple object files, sections, and languages.

Up until now, our binaries were assembled as raw flat images: every byte went exactly where NASM told it. But in larger 
systems (especially when we introduce C or Rust later), each file gets compiled separately into relocatable object 
files (.o). The linker then combines them — and the .ld script tells it exactly how to do that.

## What does a linker script do?

At a high level, the linker script acts as a map for your binary. It tells the linker:

* where the program starts (`ENTRY(start2)`)
* where each section should live in memory (`. = 0x8000`)
* how to group sections from different files (`.text`, `.data`, `.bss`, etc.)
* which global symbols to export for use in assembly or C (e.g. `_stack_top`, `_kernel_end`)

This becomes essential when we start paging or using virtual addresses — because physical load addresses and virtual 
execution addresses will differ.

## Example

Here's a minimal example of what we might use in `stage2.ld`.

{% highlight ld %}
ENTRY(start2)

SECTIONS
{
  . = 0x8000;            /* Stage 2 load address */

  .text : {
    *(.text16*)
    *(.text32*)
    *(.text*)
  }

  .rodata : { *(.rodata*) }
  .data   : { *(.data*)   }
  .bss (NOLOAD) : { *(.bss*) *(COMMON) }

  _stack_top = . + 0x1000;   /* simple 4 KiB stack symbol */
}
{% endhighlight %}

A file like this replaces the hard-coded layout logic from our assembly. NASM now just emits relocatable code, and the 
linker (`ld`) uses this script to position everything properly inside the final binary.

# Updating the Makefile

We now assemble `stage2.asm` into an object file and link it with the new script to produce an ELF image:

{% highlight makefile %}
boot/stage2.o: boot/stage2.asm
    $(NASM) -f elf32 -g -F dwarf -Wall -O0 $< -o $@

build/stage2.elf: boot/stage2.o boot/stage2.ld
    ld -T boot/stage2.ld -m elf_i386 -nostdlib -o $@ $<

boot/stage2.bin: build/stage2.elf
    objcopy -O binary $< $@
    truncate -s 8192 $@
{% endhighlight %}

This new process might look like extra work, but it pays off the moment we start mixing in C code and paging structures.
The linker will take care of symbol addresses and memory offsets — no more hardcoded numbers scattered through our 
assembly.

# Paging and Long Mode

Now that our build is structured and linkable, we can continue where we left off in Stage 2 — by preparing to enter 
64-bit long mode.

To do that, we first need to enable paging and set up basic 64-bit page tables.

Modern x86 CPUs can only enter 64-bit mode after paging and PAE (Physical Address Extension) are enabled.
That means we’ll build just enough of a paging hierarchy to identity-map the first few megabytes of physical memory — 
enough to run the kernel.

## Understanding Paging and PAE

To execute 64-bit code, the CPU insists on PAE paging being enabled and on using the long-mode paging format 
(the 4-level tree). Concretely:

* **CR4.PAE** = 1 (turn on PAE)
* **EFER.LME** = 1 (allow long mode)
* **CR0.PG** = 1 (turn on paging)
* Then a **far jump** into a 64-bit code segment (CS.L=1) to start executing in 64-bit.

### The 4-level tree (long-mode paging)

Long mode uses this hierarchy (all entries are 64-bit):

| Level | Entries | Coverage     | Entry Size |
|-------|---------|--------------|------------|
| PML4  | 512     | 512 GiB each | 8 bytes |
| PDPT  | 512     | 1 GiB each   | 8 bytes |
| PD     | 512    | 2 MiB each   | 8 bytes |
| PT     | 512    | 4 KiB each   | 8 bytes |

These tables are used in a hierarchy:

{% highlight plain %}
Virtual addr bits:  [47:39]   [38:30]   [29:21]   [20:12]   [11:0]
                    PML4 idx  PDPT idx  PD idx    PT idx    page offset

Tables:               PML4  ->  PDPT  ->  PD  ->  PT  ->  4 KiB page
                                           \-> 2 MiB page (PS=1, no PT)
{% endhighlight %}

For a minimal set up, we'll skip **PT** by using **one 2 MiB page**: create PML4[0] -> PDPT[0] -> PD[0] with **PS=1** 
which identity-maps `0x00000000–0x001FFFFF`. 

That's enough for Stage 2 and the jump.

### Why not `pt`?

You only need a **PT (page table)** if you want **4 KiB granularity** (e.g., guard pages, mapping tiny regions, 
marking some pages NX later, etc.). Early on, 2 MiB pages are simpler and faster to set up:

* Use 2 MiB page (no PT):
  * Fewer entries to touch
  * Great for identity-mapping “just get me to 64-bit”

* Use 4 KiB pages (needs PT):
  * Fine-grained control (per-page permissions)
  * Slightly more code and memory for the PT

### Paging entries

Each paging entry (in PML4, PDPT, PD, and PT) follows the same 64-bit structure and flag semantics you listed. The 
only difference is which address bits and specific flag bits are valid at that level.

Each paging entry = 64 bits (low 32 bits + high 32 bits)

Each paging entry = 64 bits (low 32 bits + high 32 bits)

| Bits  | Name                               | Meaning                                                   |
|-------|------------------------------------|-----------------------------------------------------------|
| 0     | **P (Present)**                    | Must be 1 for valid entries                               |
| 1     | **RW (Writable)**                  | 1 = writable, 0 = read-only                               |
| 2     | **US (User/Supervisor)**           | 0 = kernel-only, 1 = user-accessible                      |
| 3     | **PWT (Page Write-Through)**       | Cache control bit (leave 0)                               |
| 4     | **PCD (Page Cache Disable)**       | Cache disable bit (leave 0)                               |
| 5     | **A (Accessed)**                   | CPU sets when accessed                                    |
| 6     | **D (Dirty)**                      | For pages only (set by CPU)                               |
| 7     | **PS (Page Size)**                 | 0 = points to next table, 1 = large page (2 MiB or 1 GiB) |
| 8     | **G (Global)**                     | Optional: prevents TLB flush on CR3 reload                |
| 9–11  | **Available (ignored by hardware)** | You can use for OS bookkeeping                            |
| 12–51 | **Physical Address**               | Base address of next-level table or physical page         |
| 52–58 | **Available (ignored)**            | —                                                         |
| 59    | **PAT (Page Attribute Table)**     | Rarely used; controls memory type                         |
| 60–62 | **Ignored / Reserved**             | —                                                         |
| 63    | **NX (No Execute)**                | 1 = non-executable (if EFER.NXE = 1)                      |

How that applies per level:

| Level     | Structure            | What address field points to                  | Special bits        |
|-----------|----------------------|-----------------------------------------------|---------------------|
| **PML4E** | PML4 entry           | Physical base of PDPT                         | P, RW, US same      |
| **PDPTE** | PDPT entry           | Physical base of PD (or 1 GiB page if PS = 1) | PS = 1 → 1 GiB page |
| **PDE**   | Page directory entry | Physical base of PT (or 2 MiB page if PS = 1) | PS = 1 → 2 MiB page |
| **PTE**   | Page table entry     | Physical base of 4 KiB page                   | PS ignored          |

## Setup

To setup these page entries, we configure the flags that we need at each level.

{% highlight nasm %}
; zero 4 KiB table at EDI
%macro ZERO_PAGE 1
  mov edi, %1
  xor eax, eax
  mov ecx, 4096/4
  rep stosd
%endmacro

%define PTE_P     (1 << 0)   ; present
%define PTE_RW    (1 << 1)   ; writable
%define PTE_PS    (1 << 7)   ; page size (1 = 2 MiB)
%define PTE_FLAGS (PTE_P | PTE_RW)

setup_paging:
  ZERO_PAGE pml4
  ZERO_PAGE pdpt
  ZERO_PAGE pd

  ; PML4[0] -> PDPT
  mov   eax, pdpt
  or    eax, PTE_FLAGS
  mov   [pml4], eax
  mov   dword [pml4 + 4], 0      ; high dword = 0

  ; PDPT[0] -> PD
  mov   eax, pd
  or    eax, PTE_FLAGS
  mov   [pdpt], eax
  mov   dword [pdpt + 4], 0

  ; PD[0] -> 2 MiB page starting at 0x00000000
  mov   eax, 0x00000000 | PTE_FLAGS | PTE_PS
  mov   [pd], eax
  mov   dword [pd + 4], 0

  ; PD[1] -> 2 MiB page starting at 0x00200000
  mov   eax, 0x00200000 | PTE_FLAGS | PTE_PS
  mov   [pd + 8], eax
  mov   dword [pd + 8 + 4], 0

  ret
{% endhighlight %}

We can then wrap up the setting of this structure behind a function call, and enable paging:

{% highlight nasm %}
    call  setup_paging
    
    mov   eax, pml4
    mov   cr3, eax           ; set root paging structure
    
    mov   eax, cr4
    or    eax, (1 << 5)      ; PAE = Physical Address Extension
    mov   cr4, eax
    
    mov   ecx, 0xC0000080    ; EFER MSR
    rdmsr
    or    eax, (1 << 8)      ; enable long mode (LME)
    wrmsr
    
    mov   eax, cr0
    or    eax, (1 << 31)     ; enable paging
    mov   cr0, eax
{% endhighlight %}

At this point, all of our paging setup is complete. We now have the first 2 MiB identity mapped, ready for our kernel
to use.

# Long Mode

We're almost ready to head over to long mode now.

We do need to add two more descriptors to our gdt. These are for our 64-bit code, and 64-bit data.

{% highlight nasm %}
; GDT
gdt:
    dq 0x0000000000000000         ; 0x00: null
    dq 0x00CF9A000000FFFF         ; 0x08: 32-bit code (base=0, limit=4GiB)
    dq 0x00CF92000000FFFF         ; 0x10: 32-bit data (base=0, limit=4GiB)
    dq 0x00209A0000000000         ; 0x18: 64-bit code  (L=1, D=0, G=0 ok)
    dq 0x0000920000000000         ; 0x20: 64-bit data  (L ignored)

gdt_desc:
    dw gdt_end - gdt - 1
    dd gdt
gdt_end:
{% endhighlight %}

For the 64-bit code descriptor: **Access = 0x9A**, **Flags = 0x20** (L=1). Granularity (G) and Limit are ignored in long mode, 
so this minimalist form is fine.

Now can can push to long mode.

{% highlight nasm %}
lgdt  [gdt_desc]            ; GDT that includes 0x18 (64-bit CS)
jmp   0x18:long_entry       ; load CS with L=1 → CPU switches to 64-bit
{% endhighlight %}

This means that we need a new block of 64-bit code to jump to:

{% highlight nasm %}
BITS 64
SECTION .text64

extern _stack        ; from your linker script

long_entry:
  mov     ax, 0x20         ; 64-bit data selector
  mov     ds, ax
  mov     es, ax
  mov     ss, ax
  mov     fs, ax
  mov     gs, ax

  mov     rsp, _stack      ; top of your stage2 stack from .ld

  lea     rsi, [rel msg_long]
  call    serial_puts64

.hang:
  hlt
  jmp     .hang

%include "boot/serial64.asm"

SECTION .rodata

msg_long db "64-bit Long mode: Enabled", 13, 10, 0
{% endhighlight %}

Of course, we've had to re-implement the serial library as well to be supported in 64-bit mode.

{% highlight nasm %}
BITS 64

%define COM1 0x3F8

serial_wait_tx64:
  push rdx
  push rax

  mov  dx, COM1+5
.wait:
  in   al, dx
  test al, 0x20
  jz   .wait
  
  pop  rax
  pop  rdx

  ret

; AL = byte
serial_putc64:
  push rdx
  
  call serial_wait_tx64
  
  mov  dx, COM1
  out  dx, al
  
  pop  rdx
  
  ret

; RSI -> zero-terminated string
serial_puts64:
  push rax

.next:
  lodsb
  test al, al
  jz   .done
  ; translate '\n' -> "\r\n"
  cmp  al, 10
  jne  .send
  push rax
  mov  al, 13
  call serial_putc64
  pop  rax
.send:
  call serial_putc64
  jmp  .next
.done:
  pop  rax

  ret
{% endhighlight %}

# Building and running

Once we've got this built, we can see that we've successfully jumped across to long mode.

{% highlight plain %}
qemu-system-x86_64 -drive file=os.img,format=raw,if=ide,media=disk -serial stdio -debugcon file:debug.log -global isa-debugcon.iobase=0xe9 -display none -no-reboot -no-shutdown -d guest_errors,cpu_reset -D qemu.log
Booting ...
Starting Stage2 ...
Stage2: OK
A20 Line: Enabled
GDT: Loaded
Protected Mode: Enabled
Paging: Enabled
64-bit Long mode: Enabled
{% endhighlight %}

# Conclusion

We cleaned up the build with a linker script, set up a minimal long-mode paging tree (PML4 → PDPT → PD with 2 MiB pages), 
extended the GDT with 64-bit descriptors, and executed the CR4/EFER/CR0 sequence to reach 64-bit long mode—while 
keeping serial output alive the whole way. The result is a small but realistic bootstrap that moves from BIOS real mode 
to protected mode to long mode using identity-mapped memory and clean section layout.

In the next part we’ll start acting like an OS: map more memory (and likely move to a higher-half layout), add early 
IDT exception handlers, and bring up a simple 64-bit “kernel entry” that can print, panic cleanly, and prepare for 
timers/interrupts.
