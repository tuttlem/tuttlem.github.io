---
layout: post
title: A GDT Primer
date: 2014-07-11
comments: false
categories: ["GDT", "Assembly", "Operating Systems"]
---

For Intel chips, the major processes (like memory management, interrupts, etc) are managed through a set of tables. These tables are as simple as length and a linear address to the actual table data.

The GDT or <strong>Global Descriptor Table</strong> is one of these tables and it's what your CPU uses to describe its internal memory segmentation for the system.

In today's post, I'll take you through how the GDT is defined and how it is applied to your system.

### What is it and how is it defined?

Like I said in the introduction, all that the GDT is made up of is a length and a linear address to the data. Here's an example below, defined in assembly language

{% highlight nasm %}
gdt_data:
   DQ 0x0000000000000000
   DQ 0x00CF9A000000FFFF
   DQ 0x00CF92000000FFFF

gdt_tab:
   DW 23
   DD gdt_data
{% endhighlight %}

In the above snippet, `gdt_data` defines the actual GDT entries. We'll get into what the values mean shortly, but for now it's important to understand that this block of data starts with a null entry (or all zeros) and then the entries begin. You'll see that each entry is defined by `DQ`, so each entry is 8 bytes.

`gdt_tab` starts with the length of the structure (minus 1). The whole "minus 1" part comes in because the expected data type of the length is a word, it can only hold a maximum value of 65535 but you are allowed up to 65536 entries in the table. Obviously, it's invalid to specify a table that has a zero length. Next, `gdt_tab` defines a linear address to the table data itself, `gdt_data`.

### How is a GDT entry assembled?

Each GDT entry conforms to the following format:

| Start | End | Meaning              | Size    |
|-------|-----|----------------------|---------|
| 63    | 56  | Base (bits 24 - 31)  | 8 bits  |
| 55    | 52  | Flags                | 4 bits  |
| 51    | 48  | Limit (bits 16 - 19) | 4 bits  |
| 47    | 40  | Access byte          | 8 bits  |
| 39    | 16  | Base (bits 0 - 23)   | 24 bits |
| 15    | 0   | Limit (bits 0 - 15)  | 16 bits |

From this table, you can see that it defines a 32 bit <em>base</em> which is a linear address of where the segment begins and a 20 bit <em>limit</em> which is the maximum addressable unit.

The <em>access byte</em> is 8 bits in flags that describe different access privileges. The byte breaks down like this:

| Bit | Code  | Description |
|-----|-------|-------------|
| 7   | Pr    | <strong>Present</strong> bit. Must be 1 for all selectors. |
| 6-5 | Privl | <strong>Privilege</strong> bits. Defines the ring level this selector is allowed to be used from. |
| 4   |       | Always 1 |
| 3   | Ex    | <strong>Executable</strong> bit. 1 for code, 0 for data |
| 2   | DC    | <strong>Direction</strong> bit/<strong>Conforming</strong> bit. This is a direction bit for data selectors, in which case when it is set to 0, the segment grows up. 1, it'll grow down. This is a conforming bit for code selectors. When is is set to 1, execution is allowed by the defined privilege level or below. When it's 0, it's only allowed from the defined privilege level. |
| 1   | RW    | <strong>Readable</strong> for code selectors, <strong>Writeable</strong> for data selectors. Code selectors can't have write access and data selectors don't have read access. |
| 0   | Ac    | Leave this as 0. The CPU will set it to 1 once the segment is <strong>accessed</strong> |

The <em>flags</em> nibble is 4 bits that control size:

| Bit | Code | Description                                                             |
|-----|------|-------------------------------------------------------------------------|
| 7   | Gr   | <strong>Granularity</strong> when set to 0 will make the <em>limit</em> be interpreted in bytes. When it's set to 1, the limit is defined in pages (4KiB blocks)  |
| 6   | Sz   | <strong>Size</strong> when 0 defines 16-bit protected mode. 1 defines as 32-bit mode selectors |
| 5   | L    | <strong>Long</strong> when set to 1 will setup 64-bit mode selectors. Sz must be set to 0 |
| 4   |      | Unused. Set to 0                                                        |

### How do the values breakdown?

Above, we had some example data that we were setting up for a GDT. Here's how those values break down.

{% highlight text %}
Original value
00CF9A000000FFFF

base  24-31 : 00
flags       : C  (1100b)
limit 16-19 : F
access      : 9A (10011010b)
base 23-0   : 000000
limit 15-0  : FFFF

{% endhighlight %}

This particular entry says it's at a base of 0x00000000, has a limit of 0xFFFFF. The access byte tells us that the segment is:

* Present
* Is privileged to Ring-0
* Is executable
* Can ONLY be executed in Ring-0
* Is readable

The flags also tell us that the segment has:

* A limit that is expressed in 4KiB units
* Our selectors are 32 bits

### How is it set?

Actually defining the GDT entries is one thing, but you also need to set them as well. This is quite an easy process.

{% highlight nasm %}
mov   eax, gdt_tab   ; load in the address of the table
lgdt  [eax]          ; load the new GDT
{% endhighlight %}

After this has happened, we need to jump into our new segment to continue executing code. In the table `gdt_tab`, the code segment was defined 2nd (after the null entry). The code segment definition is `0x08` (or just 8) bytes into the table.

After jumping to our code segment, we need to refresh all of the segment selectors so that they're now pointing at the right place as well. 16 bytes (`0x10`) into the table (the third entry) is where we've defined the data segment.

{% highlight nasm %}
   jmp   0x08:refresh_segments

refresh_segments:

   mov   eax, 0x10
   mov   ds, ax
   mov   es, ax
   mov   fs, ax
   mov   gs, ax
   mov   ss, ax

{% endhighlight %}

### Differences between 32 and 64 bit

Segmentation is very simple once you enter the 64 bit world. Four of the segment registers: `CS`, `SS`, `DS` and `ES` start at `0x00` and have a limit of `0xFFFFFFFFFFFFFFFF`. Pretty simple. `FS` and `GS` are still capable of a non-zero base address.

An example table on how this would look is like this:

{% highlight nasm %}
gdt_tab_64:
   DQ 0x0000000000000000
   DQ 0x00A09A0000000000
   DQ 0x00A0920000000000
{% endhighlight %}

You can see how the base and limits have simplified greatly here.

### Conclusion

There's quite a bit more you can learn in this field. There's also some excellent resources around the web to help out. Here's just a few:

* [Wikipedia](http://en.wikipedia.org/wiki/Global_Descriptor_Table)
* [OS Dev Wiki](http://wiki.osdev.org/Global_Descriptor_Table)
* [GDT Tutorial](http://wiki.osdev.org/GDT_Tutorial)


