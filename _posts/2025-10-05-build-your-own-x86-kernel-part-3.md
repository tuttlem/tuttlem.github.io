---
layout: post
title: Build your own x86 Kernel Part 3
date: 2025-10-05
comments: false
categories: [ osdev, systems, x86 ]
---

# Introduction

In [Part 2]({% post_url 2025-10-05-build-your-own-x86-kernel-part-2 %}) we wired up a serial port so we could see life 
signs from our bootloader. Now we’re going to take the next big step — load a second stage from disk. We'll keep 
stage2 simple for now - we'll just prove that control has been transferred.

Our 512-byte boot sector is tiny, so it’ll stay simple: it loads the next few sectors (Stage 2) into memory and jumps 
there. Stage 2 will then have a lot more room to move to enable our processor.

# Finishing Stage 1

Before we can get moving with Stage 2, the first stage of our boot process still has a few things left to do. 

The BIOS hands our boot sector the **boot drive number** in `DL` (e.g., `0x80` for the first HDD, `0x00` for the floppy).

We need to stash that away for later.

{% highlight nasm %}
mov   [boot_drive], dl
{% endhighlight %}

## Disk Address Packet (DAP) 

The BIOS via `int 0x13` `(AH=0x42)` provides a read function that will allow us to read Stage 2 off of disk and into 
memory. The extended read function uses a 16-byte structure pointed to by `DS:SI`:

| Offset | Size | Field |
|--------|------|-----------------|
| `0x00` | 1    | Size of packet (16 for basic) |
| `0x01` | 1    | Reserved (0) |
| `0x02` | 2    | Number of sectors to read |
| `0x04` | 2    | Buffer offset (destination) |
| `0x06` | 2    | Buffer segment (destination) |
| `0x08` | 8    | Starting LBA (64-bit, little-endian) |

We fill this structure like so:

{% highlight nasm %}
%define STAGE2_SEG      0x0000
%define STAGE2_OFF      0x8000
%define STAGE2_LBA      1
%define STAGE2_SECTORS  16

mov   si, dap           ; DAP for stage2 -> 0000:8000
mov   byte [si], 16
mov   byte [si + 1], 0
mov   word [si + 2], STAGE2_SECTORS
mov   word [si + 4], STAGE2_OFF
mov   word [si + 6], STAGE2_SEG
mov   dword [si + 8], STAGE2_LBA
mov   dword [si + 12], 0
{% endhighlight %}

And now we can actually load it off of disk:

{% highlight nasm %}
mov   dl, [boot_drive]
mov   ah, 0x42
mov   si, dap
int   0x13
jc    disk_error
{% endhighlight %}

This call reads `count` sectors from `starting LBA` into `segment:offset` specified in the DAP. 

If you recall, we setup our stack at `0x7000`. By loading Stage 2 at `0x8000` and having 16 sectors (8 KiB), Stage 2 will 
occupy `0x8000..0x9FFF`, so there won't be a collision.

After this call we either have Stage 2 successfully loaded at `STAGE2_SEG:STAGE2_OFF` or the carry flag will be set; in 
which case, we have an error.

If everything has gone ok, we can use a **far** `jmp` to transfer control there in real mode.

{% highlight nasm %}
jmp   STAGE2_SEG:STAGE2_OFF
{% endhighlight %}

Now that we've got a bit more space to work with, we can set some more things up (video, disk i/o, a20 lines, gdt, etc.).

## Boot loader

Here's a full rundown of the boot loader so far:

{% highlight nasm %}
; ---------------------------------------------------------
; boot/boot.asm: Main boot loader
; ---------------------------------------------------------
;
BITS 16
ORG  0x7C00

%define STAGE2_SEG      0x0000
%define STAGE2_OFF      0x8000
%define STAGE2_LBA      1
%define STAGE2_SECTORS  16

main:
  cli
  
  xor   ax, ax
  mov   ss, ax
  mov   bp, 0x7000
  mov   sp, bp            ; temp stack setup (so it's below code)

  mov   ds, ax            ; DS = 0 -> labels are absolute 0x7Cxx
  mov   es, ax            ; ES = 0

  cld                     ; lods/stos auto-increment

  sti

  mov   [boot_drive], dl  ; remember the BIOS drive

  call  serial_init

  mov   si, boot_msg
  call  serial_puts

  mov   si, dap           ; DAP for stage2 -> 0000:8000
  mov   byte [si], 16
  mov   byte [si + 1], 0
  mov   word [si + 2], STAGE2_SECTORS
  mov   word [si + 4], STAGE2_OFF
  mov   word [si + 6], STAGE2_SEG
  mov   dword [si + 8], STAGE2_LBA
  mov   dword [si + 12], 0

  mov   ax, STAGE2_SEG
  mov   es, ax
  mov   dl, [boot_drive]
  mov   ah, 0x42
  mov   si, dap
  int   0x13
  jc    disk_error

  mov   si, stage2_msg
  call  serial_puts
  jmp   STAGE2_SEG:STAGE2_OFF

disk_error:
  mov   si, derr_msg
  call  serial_puts

.hang:
  hlt
  jmp   .hang

%include "boot/serial.asm"

boot_msg    db "Booting ...", 13, 10, 0
stage2_msg  db "Starting Stage2 ...", 13, 10, 0
derr_msg    db "Disk error!", 13, 10, 0

boot_drive  db 0
dap:        db 16, 0
            dw 0, 0, 0
            dd 0, 0

times 510-($-$$) db 0
dw 0AA55h
{% endhighlight %}

If we were to run this now without a Stage 2 in place, we should pretty reliably get a `Disk error!`:

{% highlight plain %}
qemu-system-x86_64 -drive file=os.img,format=raw,if=ide,media=disk -serial stdio -debugcon file:debug.log -global isa-debugcon.iobase=0xe9 -display none -no-reboot -no-shutdown -d guest_errors,cpu_reset -D qemu.log
Booting ...
Disk error!
{% endhighlight %}

# Stage 2

Our Stage 2 runs in **real mode**, but it's free of the 512-byte limit that our boot loader had. We'll keep the 
implementation very simple right now, just to prove that we've jumped over to Stage 2 - and fill it out later.

{% highlight nasm %}
; ---------------------------------------------------------
; boot/stage2.asm — loaded by MBR at 0000:8000 (LBA 1..16)
; ---------------------------------------------------------
BITS 16
ORG  0x8000           ; the offset where we were loaded to by MBR

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

.hang:
  hlt
  jmp   .hang


stage2_msg db "Stage2: OK", 13, 10, 0

%include "boot/serial.asm"
{% endhighlight %}

# Building

We need to include Stage 2 as a part of the build now in the Makefile. Not only do we need to assemble this, but it needs 
to make it into our final os image:

{% highlight Makefile %}
boot/boot.bin: boot/boot.asm
    $(NASM) -f bin $< -o $@

boot/stage2.bin: boot/stage2.asm
    $(NASM) -f bin $< -o $@
    truncate -s 8192  $@

os.img: boot/boot.bin boot/stage2.bin
    rm -f $@
    dd if=boot/boot.bin   of=$@ bs=512 count=1 conv=notrunc
    dd if=boot/stage2.bin of=$@ bs=512 seek=1  conv=notrunc
    truncate -s $$((32*512)) $@
{% endhighlight %}

After `stage2.bin` is assembled, you can see we pad it out to the full 8k which is our 16 sectors. This gets appended 
after the boot loader in the image.

With this very simple Stage 2 in place, we give this a quick build and run we should be able to confirm that we are up 
and running in Stage 2.

{% highlight plain %}
➜  make run    
qemu-system-x86_64 -drive file=os.img,format=raw,if=ide,media=disk -serial stdio -debugcon file:debug.log -global isa-debugcon.iobase=0xe9 -display none -no-reboot -no-shutdown -d guest_errors,cpu_reset -D qemu.log
Booting ...
Starting Stage2 ...
Stage2: OK
{% endhighlight %}

# Conclusion

We've made it to Stage 2. We've got a great base to work from here. In the next upcoming posts in this series we'll start 
to use Stage 2 to setup more of the boot process.
