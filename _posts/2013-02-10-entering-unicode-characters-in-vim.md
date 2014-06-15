---
layout: post
title: Entering Unicode Characters in VIM
date: 2013-02-10
comments: false
categories: [ "Tip", "VIM", "Unicode" ]
---

### Introduction

Just a quick tip sheet on how to enter unicode characters into a text page when using vim. From time to time I've needed characters listed up [here](http://www.fileformat.info/info/unicode/char/search.htm) just to give my applications that little extra touch.

### Entering characters by code

| Entry 																| Code
|-----------------------------------------------------------------------|-------------------------------
| Enter a character by its decimal value								| `^Vnnn`
| Enter a character by its octal value									| `^Vonnn`
| Enter a character by its hex value									| `^Vxnn`
| Enter a character by its hex value for BMP unicode codepoints 		| `^Vunnnn`
| Enter a character by its hex value for any unicode codepoint 			| `^VUnnnnnnnn`

In all of these examples, the `n`'s are the code and `^V` means `Control-V`.

That's it!