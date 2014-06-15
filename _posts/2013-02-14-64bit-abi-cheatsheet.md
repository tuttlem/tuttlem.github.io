---
layout: post
title: 64bit ABI Cheatsheet
date: 2013-02-14
comments: false
categories: [ "Assembly", "64 bit", "ABI" ]
---

### Introduction

Using a few different articles around the web, I thought it might be a good idea to aggregate all of the information around calling conventions in 64bit land. This could come in handy when wanting to write a cross OS compliant library at the assembly level. This particular article will directly target the assembly programming layer so that topics like C++ name mangling and caller clean-up are removed from its scope.

### References

* [x86 calling conventions](http://en.wikipedia.org/wiki/X86_calling_conventions)
* [x86-64.org documentation](http://www.x86-64.org/documentation/)
* [Mac OSX 64 bit Assembly System Calls](http://thexploit.com/secdev/mac-os-x-64-bit-assembly-system-calls/)

### Microsoft

Windows will use `RCX`, `RDX`, `R8` and `R9` for the first four integer or pointer arguments. `XMM0`, `XMM1`, `XMM2` and `XMM3` are used for floating point arguments. Additional arguments are passed via the stack (right to left).

An integer or pointer return value will be returned in `RAX`. Floating point return will be in `XMM0`.

### System V

System V operating systems will use `RDI`, `RSI`, `RDX`, `RCX`, `R8` and `R9`. `XMM0`, `XMM1`, `XMM2`, `XMM3`, `XMM4`, `XMM5`, `XMM6` and `XMM7` will be used to pass floating point parameters. `RAX` will hold the syscall number. Additional arguments are passed via the stack (right to left).

Return values are sent back via `RAX`.

### Syscall Numbers

It's interesting to note the structure of the syscall number when it comes time to execute. Looking at [syscall_sw.h](http://www.opensource.apple.com/source/xnu/xnu-792.13.8/osfmk/mach/i386/syscall_sw.h), you'll see that apple machines want a 2 in the higher-order double word such that the `write` syscall, normally passed as `0x04` would be passed as `0x2000004` in OSX.

That's it for today. Just a cheat sheet really.