---
layout: post
title: An inline assembly primer with gcc
date: 2015-01-13
comments: false
categories: [ "gcc", "asm", "inline" ]
---

A really handy feature of [GCC](https://gcc.gnu.org/) is that it allows you to [inter-mix assembly code with your C code](https://gcc.gnu.org/onlinedocs/gcc/Using-Assembly-Language-with-C.html#Using-Assembly-Language-with-C). 

There are so many great references on this topic already. One of the best is from [Lockless](http://locklessinc.com/articles/gcc_asm/) however there are [many](http://asm.sourceforge.net/articles/rmiyagi-inline-asm.txt), [many](http://wiki.osdev.org/Inline_Assembly) more.

Today's article is more of a quick reference for inlining your assembly code.

### Syntax

When preparing a block of assembly to be put inline in your C code, you use the `asm` keyword. 

{% highlight text %}
asm [volatile] ( 
	assembly code
	: outputs
	: inputs
	: clobbers
)
{% endhighlight %}

The first parameter that is passed is the assembly code itself. It'll be in AT&T syntax, but will also have some extra rules apply to it which will allow for the compiler to make some decisions for you. The <strong>outputs</strong>, <strong>inputs</strong> and <strong>clobbers</strong> are optional lists consisting of directives instructing the compiler how to handle inputs, outputs and what's expected to be trashed (clobbered) in your assembly block.

A simple example usage, to add two integers and return the result might look like this:

{% highlight c %}
int add(int a, int b) {
	int c;

	asm (
		"xorl	%%eax, %%eax\n\t"
		"addl	%2, %1\n\t"
		"movl 	%1, %0"
		: "=m" (c)
		: "r" (a), "r" (b)
		: "%eax"
	);

	return c;
}
{% endhighlight %}

Once this code is compiled, you can see what the compiler has done with it:

{% highlight text %}
	asm (
   a:	8b 55 ec             	mov    edx,DWORD PTR [rbp-0x14]
   d:	8b 4d e8             	mov    ecx,DWORD PTR [rbp-0x18]
  10:	31 c0                	xor    eax,eax
  12:	01 ca                	add    edx,ecx
  14:	89 55 fc             	mov    DWORD PTR [rbp-0x4],edx
		: "=m" (c)
		: "r" (a), "r" (b)
		: "%eax"
	);
{% endhighlight %}

`edx` and `ecx` were chosen as our general purpose registers for inputs, so they're loaded first-up. 
The addition occurs and then the result (as requested) is placed in the memory location of our output.
Back in the inline code, you can see that these registers have been symbolically referenced as `%1`, `%2`, etc.

<strong>Outputs</strong> are a mix of <em>constraints</em> and <em>modifiers</em>, <strong>inputs</strong> are just <em>constraints</em> and <strong>clobbers</strong> list out what was modified (register-wise or other).

### What about volatile?

The `volatile` keyword allows you to tell the compiler to not optimise away our code if it deems that it isn't required (i.e. is has no effect on anything). 

### Constraints

| Constraint | Description |
|---------|-------------|
| m       | Any kind of a memory address  |
| o       | Memory address if it's offsettable |
| V       | Memory address if it's not offsettable |
| <       | Memory with autodecrement addressing  |
| >       | Memory with autoincrement addressing  |
| r       | General purpose register  |
| i       | Immediate integer value  |
| n       | Immediate integer with a known value |
| I . . P | Range based immediate integer values |
| E       | Immediate format-dependent floating point number  |
| F       | Immediate floating point number |
| G, H    | Range based immediate float values |
| s       | Immediate integer that is not an explicit integer |
| g       | Any register, memory or immediate value; not a general purpose register though |
| X       | Any operand is allowed |
| p       | Any operand that is a valid memory address | 

A full description of all of these constraints can be found [here](https://gcc.gnu.org/onlinedocs/gcc/Simple-Constraints.html#Simple-Constraints).

### Modifiers

| Modifier | Description |
|----------|-------------|
| =        | Operand is written to |
| +        | Operand is read from and written to |
| &        | Operand is written to (clobbered) before input operands are used |
| %        | Instruction is cumulative for this operand |

A full description of all of these modifiers can be found [here](https://gcc.gnu.org/onlinedocs/gcc/Modifiers.html#Modifiers).

### Clobbers

| Clobber | Description |
|---------|-------------|
| cc      | Flags are modified |
| memory  | Memory outside of what is in the constraints is modified |

A full description of clobbers can be found [here](https://gcc.gnu.org/onlinedocs/gcc/Extended-Asm.html#Clobbers).

