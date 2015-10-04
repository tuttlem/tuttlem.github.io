---
layout: post
title: Inline assembly with Watcom
date: 2015-10-04
comments: false
categories: [ "dos", "watcom", "32" ]
---

In a [previous post]({% post_url 2015-10-04-32bit-dos-development-with-open-watcom %}), I'd started talking about the [Open Watcom](https://en.wikipedia.org/wiki/Watcom_C/C%2B%2B_compiler) compiler and its usage in the DOS environment. In today's post, I'm going to walk through writing assembly code inside of your C/C++ that you'll compile with the Watcom compiler.

### Using the DOS api

When it comes to basic interrupt invocation or I/O port work, there's no reason why the API provided by the `dos.h` header won't suffice. It's allows you to write C code, but directly invoke I/O ports and interrupts. Once you want to perform some custom logic, you'll be reaching for planting assembly code directly into your code. 

Here's an example using the `dos.h` api. In this example, we're going to request a key press from the user using keyboard service's [int 16h](https://en.wikipedia.org/wiki/INT_16H) and print out the captured scan and ascii codes:

{% highlight c %}
#include <stdio.h>
#include <dos.h>

int main(int argc, char *argv[]) {

  union REGS r;

  /* ah = 00h, int 16h "read key stroke" */
  r.x.eax = 0x0000;
  int386(0x16, &r, &r);

  /* write the results to stdout */
  printf("scan code = %d\n", r.h.ah);
  printf("ascii     = %d\n", r.h.al);

  return 0;
}
{% endhighlight %}

The call to `int386` takes a register set as inputs and outputs, so that we can see the CPU state after the interrupt was executed.

### Inlined

So, what does that look like inlined?

{% highlight c %}
#include <stdio.h>

int read_key_stroke();
#pragma aux read_key_stroke = \
"int 0x16"                    \
value [eax];

int main(int argc, char *argv[]) {

  int key = read_key_stroke();
 
  /* extract the ascii & scan code */
  int ascii = key & 0xff,
      scan = key >> 8 & 0xff;

  /* write the results to stdout */
  printf("scan code = %d\n", scan);
  printf("ascii     = %d\n", ascii);

  return 0;
}
{% endhighlight %}

Without needing to manage the registers anymore, we've cleaned up a little bit of the code. There is a little bit of alien syntax to deal with though.

### #pragma aux 

The basic structure of an inline assembly function using the `#pragma aux` syntax goes like this:

{% highlight text %}
#pragma aux name_of_your_function =
. . . assembly code in here . . .
modify [ regs ]
value [ reg ]
parm [ regs ]
{% endhighlight %}

You start your function off optionally with a header definition. It's been omitted in this example, but I've added one above for `read_key_stroke`.

The assembly code itself gets quoted and then terminates with three optional instructions.

`modify` allows you to tell the compiler which registers are going to get clobbered when the function runs. This is so it can do the appropriate save management of these registers to the stack.

{% highlight text %}
modify [ eax ebx ecx ]
{% endhighlight %}

This line says that `eax`, `ebx` and `ecx` all get clobbered when this function runs.

`value` allows you to nominate which register has the return value in it.

{% highlight text %}
value [ eax ]
{% endhighlight %}

This line says that the return value is in `eax`. As with `read_key_stroke` above, the value of `eax` is then fed into the `int` return value for the function.

`parm` allows you to nominate registers that will take the values of parameters passed in. 

{% highlight text %}
parm [ eax ] [ ebx ] [ ecx ]
{% endhighlight %}

If we were to implement a function that performs addition, we'd need two arguments to be passed in:

{% highlight text %}
int add_ints(int a, int b);
#pragma aux add_ints =  \
"add  eax, ebx"         \
parm  [ eax ] [ ebx ]   \
value [ eax ];
{% endhighlight %}

Passing parameters is fairly straight forward. You're free to use `EAX`, `EBX`, `ECX`, `EDX`, `EDI` and `ESI` but you are not able to use `EBP`.

