---
layout: post
title: Binary recon techniques
date: 2024-10-14
comments: false
categories: [ "security", "recon" ]
---

# Introduction

In the world of cybersecurity, understanding how binaries operate and interact with a system is critical for both defense and troubleshooting. Whether you're analyzing potential malware, reverse engineering an application, or simply trying to understand how a piece of software works, performing a thorough system reconnaissance is a vital skill.

Linux offers a wide range of powerful tools that allow you to investigate and inspect binaries, revealing everything from their internal structure to their external behavior. These tools help in uncovering useful information such as file type, embedded strings, dependencies, system calls, and much more. By using them effectively, you can gain valuable insights into how a binary interacts with the system and what potential risks or vulnerabilities it might pose.

In this article, we’ll walk through a collection of essential Linux tools for binary analysis, starting with the basics and working toward more advanced techniques. Whether you're a seasoned engineer or just getting started with system-level investigation, these tools will arm you with the knowledge to perform comprehensive binary analysis in a Linux environment.

# Shell Tools

In this section, I’ll cover a variety of shell-based tools designed to provide insights into files you encounter. These tools help you gather critical intel, allowing you to better understand and reason about the file's nature and potential behavior.

## file

The file command will attempt to classify the file that you've given it. The classification that comes back can give you valuable information about the contents of the file itself.

The man page says the following:

> file tests each argument in an attempt to classify it.  There are three sets of tests, performed in this order: filesystem tests, magic tests, and language  tests. The first test that succeeds causes the file type to be printed.

### Usage

{% highlight shell %}
$ file /bin/sh
/bin/sh: symbolic link to bash
$ file README.md
README.md: ASCII text, with no line terminators
$ file gouraud.png
gouraud.png: PNG image data, 320 x 157, 8-bit/color RGB, non-interlaced
{% endhighlight %}

Immediately, you're given feedback classifying the content of the file.

## strings

The `strings` command is useful when you want to extract readable text from a binary file. This can be particularly helpful for identifying embedded text, error messages, function names, or other human-readable content hidden within the binary. By filtering out non-text data, `strings` allows you to gain clues about the file’s purpose or origin without needing to disassemble the entire binary.

The man page describes it as follows:

> For each file given, GNU `strings` prints the sequences of printable characters that are at least 4 characters long (or the number given with the options below) and are followed by an unprintable character.

### Usage

As an example, let's look at the first few strings in `/bin/bash`.

{% highlight shell %}
strings /bin/bash 

/lib64/ld-linux-x86-64.so.2
DJ
E $
@B       
B8"
@P@@

. . .
. . .

fchmod
libreadline.so.8
libc.so.6
initialize_job_control
sys_error
maybe_make_export_env
dispose_word

{% endhighlight %}

The presence of `libc.so.6` tells us that the binary likely uses the standard C library, for instance.

## lsof

The `lsof` command, short for "list open files," shows all the files currently opened by active processes. Since everything in Linux is treated as a file (including directories, devices, and network connections), `lsof` is a powerful tool for monitoring what a binary interacts with in real time. By using `lsof`, you can track which resources the binary accesses, providing insight into its operations.

From the man page:

> `lsof` lists on its standard output file information about files opened by processes for the given conditions.

### Usage

For these examples, the program that we're probing needs to be running. We'll be able to list out open files, and network connections.

{% highlight shell %}
$ lsof -p 25524       

COMMAND   PID    USER  FD   TYPE             DEVICE SIZE/OFF     NODE NAME
zsh     25524    user cwd    DIR                8,2     4096  1703938 /home/user
zsh     25524    user rtd    DIR                8,2     4096        2 /
zsh     25524    user txt    REG                8,2   947360 14055379 /usr/bin/zsh
zsh     25524    user mem    REG                8,2  3060208 14038967 /usr/lib/locale/locale-archive
zsh     25524    user mem    REG                8,2    76240 14055393 /usr/lib/zsh/5.9/zsh/computil.so

. . .
{% endhighlight %}

We can also look at the network connections of these:

{% highlight shell %}
➜  ~ lsof -i -p 23316   
COMMAND     PID    USER  FD      TYPE             DEVICE  SIZE/OFF     NODE NAME
jetbrains  1103    user  74u     IPv6              14281       0t0      TCP localhost:52829 (LISTEN)
kdeconnec  1109    user  26u     IPv6              34398       0t0      UDP *:xmsg
kdeconnec  1109    user  27u     IPv4              17425       0t0      UDP *:mdns
kdeconnec  1109    user  28u     IPv6              17426       0t0      UDP *:mdns
kdeconnec  1109    user  29u     IPv6              34399       0t0      TCP *:xmsg (LISTEN)
kdeconnec  1109    user  30u     IPv4              39158       0t0      UDP *:60200
kdeconnec  1109    user  31u     IPv6              39159       0t0      UDP *:49715

. . .
{% endhighlight %}

You can also look for what files are being accessed by users on your system:

{% highlight shell %}
$ lsof -u user
{% endhighlight %}

## ldd

The `ldd` command lists the shared libraries that a binary depends on. Since many binaries rely on external libraries for functionality, `ldd` helps you map out these dependencies and check whether all required libraries are present. This is particularly useful when investigating dynamically linked binaries or troubleshooting issues related to missing libraries.

From the man page:

> `ldd` prints the shared libraries required by each program or shared library specified on the command line.

### Usage

{% highlight shell %}
$ ldd /bin/bash
  linux-vdso.so.1 (0x00007aa7b7743000)
  libreadline.so.8 => /usr/lib/libreadline.so.8 (0x00007aa7b759e000)
  libc.so.6 => /usr/lib/libc.so.6 (0x00007aa7b73ad000)
  libncursesw.so.6 => /usr/lib/libncursesw.so.6 (0x00007aa7b733e000)
  /lib64/ld-linux-x86-64.so.2 => /usr/lib64/ld-linux-x86-64.so.2 (0x00007aa7b7745000)
{% endhighlight %}

This is really helpful. We can see all of the shared libraries that `/bin/bash` relies on.

## objdump

The `objdump` command provides detailed information about object files, offering insight into a binary’s internal structure. With `objdump`, you can disassemble the binary, inspect its headers, and examine its machine code and data sections. This tool is invaluable when you're conducting a deep analysis, as it gives you a granular look at the file's components.

From the man page:

> `objdump` displays information about one or more object files. The options control what particular information to display.

### Usage

In a [previous post]({% post_url 2017-10-28-executing-shellcode-in-c %}) I had written about `objdump` usage while creating shell code to execute.

## nm

The `nm` command allows you to list symbols from object files, providing insight into the binary's functions, variables, and other symbols. It’s a useful tool when trying to reverse engineer a binary, as it helps map out its structure and function entry points. You can also use it to debug symbol-related issues in your own compiled binaries.

From the man page:

> `nm` lists the symbols from object files. The symbol names are shown in the name column, and additional information includes the type and the value associated with each symbol.

### Usage

I've just written and compiled a "Hello, world" executable.

After a standard compilation `gcc hello.c -o hello`, the following is returned.

{% highlight shell %}
$ nm hello
0000000000004018 B __bss_start
w __cxa_finalize@GLIBC_2.2.5
0000000000004008 D __data_start
0000000000004008 W data_start
0000000000004010 D __dso_handle
0000000000003de0 d _DYNAMIC
0000000000004018 D _edata
0000000000004020 B _end
0000000000001160 T _fini
0000000000003fe8 d _GLOBAL_OFFSET_TABLE_
w __gmon_start__
0000000000002014 r __GNU_EH_FRAME_HDR
0000000000001000 T _init
0000000000002000 R _IO_stdin_used
w _ITM_deregisterTMCloneTable
w _ITM_registerTMCloneTable
U __libc_start_main@GLIBC_2.34
0000000000001139 T main
U puts@GLIBC_2.2.5
0000000000001040 T _start
0000000000004018 D __TMC_END__
{% endhighlight %}

## readelf

The `readelf` command is similar to `objdump`, but it focuses specifically on displaying information from ELF (Executable and Linkable Format) files. This tool can show detailed information about sections, headers, program headers, and other parts of an ELF binary. `readelf` is a go-to tool for investigating how ELF binaries are structured, particularly in understanding what segments the binary contains and how it’s loaded into memory.

From the man page:

> `readelf` displays information about one or more ELF format object files. The options control what particular information to display.

### Usage

For the "Hello, world" program, `readelf` breaks the ELF header down:

{% highlight shell %}
$ readelf -h hello
ELF Header:
Magic:   7f 45 4c 46 02 01 01 00 00 00 00 00 00 00 00 00
Class:                             ELF64
Data:                              2's complement, little endian
Version:                           1 (current)
OS/ABI:                            UNIX - System V
ABI Version:                       0
Type:                              DYN (Position-Independent Executable file)
Machine:                           Advanced Micro Devices X86-64
Version:                           0x1
Entry point address:               0x1040
Start of program headers:          64 (bytes into file)
Start of section headers:          13520 (bytes into file)
Flags:                             0x0
Size of this header:               64 (bytes)
Size of program headers:           56 (bytes)
Number of program headers:         13
Size of section headers:           64 (bytes)
Number of section headers:         30
Section header string table index: 29
{% endhighlight %}

## strace

The `strace` command traces the system calls made by a binary as it runs. System calls are the interface between user-space applications and the Linux kernel, so tracing them can give you a real-time view of how the binary interacts with the system. Whether you’re debugging or investigating suspicious behavior, `strace` is an essential tool for following what a binary is doing at a low level.

From the man page:

> `strace` is a diagnostic, debugging, and instructional userspace utility for Linux. It is used to monitor and tamper with interactions between processes and the Linux kernel, including system calls, signal deliveries, and changes of process state.

### Usage

I've removed a lot of the calls here. I've run the Hello, world program through `strace` and you can trace through to the 
system calls.

{% highlight shell %}
$ strace ./hello
execve("./hello", ["./hello"], 0x7ffc231b4fc0 /* 82 vars */) = 0

. . .
. . .

write(1, "Hello, world\n", 13Hello, world
)          = 13
exit_group(0)                           = ?
+++ exited with 0 +++
{% endhighlight %}

## ltrace

The `ltrace` command works similarly to `strace`, but instead of tracing system calls, it tracks the dynamic library calls made by a binary. If you want to see how a program interacts with shared libraries, such as the C standard library (`libc`), `ltrace` is the tool to use. It’s particularly useful when debugging issues related to dynamically linked functions.

From the man page:

> `ltrace` is a program that simply runs the specified command until it exits. It intercepts and records the dynamic library calls that are called by the executed process and the signals received by that process.

### Usage

Looking again at our "Hello, world" program:

{% highlight shell %}
$ ltrace ./hello
puts("Hello, world"Hello, world
)                                         = 13
+++ exited (status 0) +++
{% endhighlight %}

## gdb

The GNU Debugger (`gdb`) is a powerful tool for interactively debugging binaries. You can use it to set breakpoints, inspect memory and variables, and step through a binary’s execution line by line. `gdb` is a versatile tool not only for developers debugging their own code but also for reverse engineers looking to analyze how a binary works.

From the man page:

> `gdb` is a portable debugger that works for many programming languages, including C, C++, and Fortran. The main purpose of `gdb` is to allow you to see what is going on inside another program while it is executing or what another program was doing at the moment it crashed.

I have a write up in a [previous post]({% post_url 2012-11-25-using-the-gnu-debugger %}) about gdb.

## hexedit

The `hexedit` command is a hex editor that allows you to directly view and edit the raw binary content of a file. This can be useful for making minor modifications to a binary or simply for inspecting its content at the byte level. It’s especially helpful when you need to look at binary structures or strings that aren’t visible using regular text-based tools.

From the man page:

> `hexedit` shows a file both in hexadecimal and in ASCII. The file can be modified, and the changes take effect immediately.

## objcopy

The `objcopy` command allows you to copy and translate object files from one format to another. It’s often used to extract or remove sections from binaries, making it a useful tool for tailoring object files to specific requirements. `objcopy` can be helpful when you need to analyze or modify specific sections of a binary, such as stripping debugging symbols.

From the man page:

> `objcopy` copies the contents of an object file to another. It can also extract specific sections from the object file or remove sections from it.

## patchelf

The `patchelf` command lets you modify ELF binaries, enabling you to change key properties like the dynamic loader path or `RPATH`. This is useful when you want to adjust how an ELF binary locates shared libraries, or when you’re working in an environment where libraries are stored in non-standard locations.

From the man page:

> `patchelf` is a simple utility for modifying existing ELF executables and libraries. It can change the dynamic loader ("ELF interpreter") of executables and change the `RPATH` and `RUNPATH` of executables and libraries.

## checksec

The `checksec` command provides a quick way to check the security properties of a binary. It examines whether the binary uses common security mechanisms like stack canaries, non-executable memory (NX), or position-independent execution (PIE). This tool is great for assessing how hardened a binary is against common vulnerabilities.

From the man page:

> `checksec` is a bash script to check the properties of executables (e.g., whether they are compiled with stack protection, DEP, ASLR, etc.).

### Usage

Let's look at Hello, world.

{% highlight shell %}
$ checksec --format=cli --file=hello
RELRO           STACK CANARY      NX            PIE             RPATH      RUNPATH      Symbols    FORTIFY  Fortified       Fortifiable     FILE
Partial RELRO   No canary found   NX enabled    PIE enabled     No RPATH   No RUNPATH   24 Symbols   N/A    0               0               hello
{% endhighlight %}

# Other Tools

While the shell-based tools discussed above are invaluable for quick inspection and analysis of binaries, there are several more advanced tools that provide deeper functionality and broader capabilities. Below are some additional tools worth exploring for more in-depth binary analysis and reverse engineering:

- **[GHIDRA](https://ghidra-sre.org/)**: A powerful open-source reverse engineering tool developed by the NSA. It supports analysis of binary code for a wide variety of architectures and provides a graphical interface for decompilation and analysis.
- **[Radare2](https://rada.re/n/)**: An advanced open-source framework for reverse engineering and analyzing binaries. It provides a rich command-line interface as well as a visual mode for inspecting file structures.
- **[Binary Ninja](https://binary.ninja/)**: A commercial reverse engineering platform offering a clean interface and scriptable analysis features for binary inspection and disassembly.
- **[Hopper Disassembler](https://www.hopperapp.com/)**: A reverse engineering tool designed for macOS and Linux that helps with disassembly, decompilation, and debugging of binaries.
- **[IDA Pro](https://hex-rays.com/ida-pro/)**: A well-known, industry-standard disassembler and debugger, widely used in reverse engineering for deeply analyzing executables and debugging code across various architectures.
- **[capstone](https://www.capstone-engine.org/)**: A lightweight, multi-architecture disassembly framework that can be integrated into other tools or used to write custom disassemblers.
- **[RetDec](https://retdec.com/)**: An open-source decompiler developed by Avast, designed to convert machine code into human-readable code.
- **[pwntools](https://github.com/Gallopsled/pwntools)**: A CTF framework and exploit development library, useful for writing scripts to exploit vulnerabilities in binaries and automate tasks.
- **[Angr](https://angr.io/)**: A platform for analyzing binaries, capable of both static and dynamic analysis, widely used in vulnerability research and symbolic execution.

These tools are generally more sophisticated than the shell-based ones and are essential for deep binary analysis, reverse engineering, and exploit development. Many are extensible with scripting capabilities, allowing for custom and automated analysis workflows.
