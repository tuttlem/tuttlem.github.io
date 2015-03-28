---
layout: post
title: MZ EXE files
date: 2015-03-28
comments: false
categories: [ "16bit", "exe", "dos", "mz" ]
---

[Executable files](https://en.wikipedia.org/wiki/.exe) in [MS-DOS](https://en.wikipedia.org/wiki/MS-DOS) come in a few different formats. The original 16-bit version of this file format is referred to as the [DOS MZ Executable](http://en.wikipedia.org/wiki/DOS_MZ_executable). 

In today's post, we're going to dissect the internals of this format.

### MZ

This particular gets its name "MZ" due to the first two bytes of the file `0x4d` and `0x5a`. Translated to ASCII text, these two bytes form the characters "MZ". This is the opening signature <em>(or magic number)</em> for a file of this format.

### The header

The first chunk of an EXE file is the header information. It stores relocation information important to the execution of the file. A few important notes when reading the header:

* All values spanning more than one byte are stored LSB first
* A block is 512 bytes in size
* A paragraph is 16 bytes in size

| Offset    | Description |
|-----------|-------------|
| 0x00-0x01 | The values 0x4d and 0x5a translating to the ASCII string "MZ". This is the magic number for the file |
| 0x02-0x03 | The number of bytes used in the last block of the EXE. A zero value indicates that the whole block is used |
| 0x04-0x05 | The number of blocks that form part of the EXE |
| 0x06-0x07 | The number of relocation entries. These are stored after the header |
| 0x08-0x09 | The number of paragraphs in the header |
| 0x0A-0x0B | The number of paragraphs required for uninitialized data |
| 0x0C-0x0D | The number of paragraphs of additional memory to constrain this EXE to |
| 0x0E-0x0F | Relative value for the SS register |
| 0x10-0x11 | Initial SP register value |
| 0x12-0x13 | Word checksum |
| 0x14-0x15 | Initial IP register value |
| 0x16-0x17 | Relative value for the CS register |
| 0x18-0x19 | Offset of the first relocation item |
| 0x1A-0x1B | Overlay number |

### An example

Take the following "Hello, world" program written in [x86 assembly language](http://en.wikipedia.org/wiki/X86_assembly_language):

{% highlight asm %}

section .text

start:

  mov   ax, seg hello
  mov   ds, ax

  mov   dx, hello
  mov   ah, 09h
  int   21h

  mov   ah, 4ch
  xor   al, al
  int   21h

section .data

  hello db 'Hello, world!', 13, 10, '$'
{% endhighlight %}

I assembled this file with [NASM](http://www.nasm.us/):

{% highlight bash %}
$ nasm -f obj hello2.asm -o hello2.obj
{% endhighlight %}

I then transferred the resulting `obj` file from my linux machine over to a dos machine and ran `TLINK` which was part of the [Turbo Assembler](http://en.wikipedia.org/wiki/Turbo_Assembler) product.

{% highlight text %}
> tlink hello2.obj
{% endhighlight %}

Once we've assembled and linked this file to produce a 16-bit dos executable, we can pull it apart again with `objdump`.

{% highlight bash %}
$ objdump -s -D -b binary -mi8086 HELLO2.EXE
{% endhighlight %}

The output of this dump is quite detailed. I've removed a fair bit of it for brevity:

{% highlight text %}
HELLO2.EXE:     file format binary

Contents of section .data:
 0000 4d5a2200 02000100 20000000 ffff0000  MZ"..... .......
 0010 00000000 00000000 3e000000 0100fb50  ........>......P
 0020 6a720000 00000000 00000000 00000000  jr..............
 0030 00000000 00000000 00000000 00000100  ................
 0040 00000000 00000000 00000000 00000000  ................   
  . . .
  . . .

 0200 b801008e d8ba0200 b409cd21 b44c30c0  ...........!.L0.
 0210 cd214865 6c6c6f2c 20776f72 6c64210d  .!Hello, world!.
 0220 0a24                                 .$              

Disassembly of section .data:

00000000 <.data>:
   0: 4d                    dec    %bp
   1: 5a                    pop    %dx
   2: 22 00                 and    (%bx,%si),%al
   4: 02 00                 add    (%bx,%si),%al
   6: 01 00                 add    %ax,(%bx,%si)
   8: 20 00                 and    %al,(%bx,%si)
   a: 00 00                 add    %al,(%bx,%si)
   c: ff                    (bad)  
   d: ff 00                 incw   (%bx,%si)
  ...
  17: 00 3e 00 00           add    %bh,0x0
  1b: 00 01                 add    %al,(%bx,%di)
  1d: 00 fb                 add    %bh,%bl
  1f: 50                    push   %ax
  20: 6a 72                 push   $0x72
  ...
  3e: 01 00                 add    %ax,(%bx,%si)
  ...
 200: b8 01 00              mov    $0x1,%ax
 203: 8e d8                 mov    %ax,%ds
 205: ba 02 00              mov    $0x2,%dx
 208: b4 09                 mov    $0x9,%ah
 20a: cd 21                 int    $0x21
 20c: b4 4c                 mov    $0x4c,%ah
 20e: 30 c0                 xor    %al,%al
 210: cd 21                 int    $0x21
 212: 48                    dec    %ax
 213: 65                    gs
 214: 6c                    insb   (%dx),%es:(%di)
 215: 6c                    insb   (%dx),%es:(%di)
 216: 6f                    outsw  %ds:(%si),(%dx)
 217: 2c 20                 sub    $0x20,%al
 219: 77 6f                 ja     0x28a
 21b: 72 6c                 jb     0x289
 21d: 64 21 0d              and    %cx,%fs:(%di)
 220: 0a 24                 or     (%si),%ah
{% endhighlight %}

Focusing on the top representation, we get a direct view of the values in the header.

{% highlight text %}
0000 4d5a2200 02000100 20000000 ffff0000  MZ"..... .......
0010 00000000 00000000 3e000000 0100fb50  ........>......P
0020 6a720000 00000000 00000000 00000000  jr..............
0030 00000000 00000000 00000000 00000100  ................
0040 00000000 00000000 00000000 00000000  ................
{% endhighlight %}

<strong>(0x00-0x01) 4d5a</strong>

The first two bytes are indeed "MZ", or `0x4d` `0x5a`. So we've got the correct signature.

<strong>(0x02-0x03) 2200</strong>

This is the number of bytes used in the last block of the EXE. Remember, we've got LSB first when we're dealing with multi-byte values, so this is `0x22` bytes. If you take a look at the resulting code listing above, you'll see that the code for the executable starts at address `0x200` and ends at `0x220`. At the address of `0x220`, 2 additional bytes are used. 

This is our `0x22` bytes as it is the first, <strong>last</strong> and only block that we have!

<strong>(0x04-0x05) 0200</strong>

This is the number of blocks (remember: 512 bytes chunks) that comprise of our EXE. We have 2. Our header is using the first block, our code and data is in the second.

<strong>(0x06-0x07) 0100</strong>

We have 1 relocation item. A relocation item is just a 16-bit value for the offset followed by a 16-bit value for the segment.

<strong>(0x08-0x09) 2000</strong>

There are `0x20` paragraphs in the header. 

{% highlight text %}
0x20 = 32 (decimal)
paragraph size = 16 bytes

32 * 16 = 512 bytes
{% endhighlight %}

This calculates out. 512 bytes in the header. We can see that the file offset starts at `0x00`. Code doesn't appear until `0x200`. `0x200` is 512 in decimal.

<strong>(0x0A-0x0B) 0000</strong>

Our program didn't define any uninitialized data, only a pre-initialized string: "Hello, world".

<strong>(0x0C-0x0D) ffff</strong>

This is the default mode of operation for memory constraints. It says, use everything (i.e. don't place any constraint).

<strong>(0x0E-0x0F) 0000</strong>

No translation to the stack segment (SS) will go on here. This value gets added to the segment value of where the program was loaded at and that's how SS is initialized. The program that we've written didn't define a stack, so no translation required.

<strong>(0x10-0x11) 0000</strong>

SP's initial value

<strong>(0x12-0x13) 0000</strong>

This is the word checksum. It's seldom used.

<strong>(0x14-0x15) 0000</strong>

The instruction pointer will start at `0x0000`.

<strong>(0x16-0x17) 0000</strong>

This value would adjust CS. 

<strong>(0x18-0x19) 3e00</strong>

This is the address of the first relocation item in the file. If we take a look back at the dump now, we can see the value sat at that address:

{% highlight text %}
0030 ________ ________ ________ ____0100  ................
0040 0000____ ________ ________ ________  ................ 
{% endhighlight %}

This takes the format of offset:segment here, so we've got 0000:0100. This will be used at execution time and will also influence the resulting stack segments and offsets.

<strong>(0x1A-0x1B) 0000</strong>

Overlay number. Zero indicates that this is the main program.

### The rest

Everything from here looks pretty familiar. We can see our assembly code start off and our string defined at the end.

