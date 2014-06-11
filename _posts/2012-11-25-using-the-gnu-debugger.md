---
layout: post
title: Using the GNU Debugger
date: 2012-11-25
comments: false
---

Bundled with the GNU toolset is a full featured debugger in GDB. This blog post aims to be a cheatsheet to get yourself around this tool quickly.

### The example program

{% highlight c %}
int main(int argc, char *argv[]) {
  /* some local variables */
  int x = 1, y = 2, z = 0;

  /* something that will go bad */
  int ans = x / z;

  return 0;
}
{% endhighlight %}

### Compile your code with debug info

You need to use the `-g` switch with GCC in order to compile debug symbols. This helps the feedback that GDB will give you a great deal.

{% highlight bash %}
$ gcc -g test.c -o test
{% endhighlight %}

### Loading your program into the debugger

So, in our example above the name of our executable would be "test". So, we just tell gdb to load test by passing it in.

{% highlight bash %}
$ gdb test
{% endhighlight %}

You can even start gdb with a text user interface, so I've just discovered.

{% highlight bash %}
$ gdb test -tui
{% endhighlight %}

It looks like this - not bad, eh?

![GDB Screenshot](http://2.bp.blogspot.com/-6Zx3N1_lUdg/ULHS7XhkRaI/AAAAAAAAAhg/oS9iA9-Jl1c/s1600/Screenshot+-+251112+-+18:11:48.png)

Now we instruct gdb that we'd like to run the application

{% highlight text %}
(gdb) run
Starting program: /home/michael/Development/gdb-tute/test 
Program received signal SIGFPE, Arithmetic exception.
0x00000000004004c3 in main (argc=1, argv=0x7fffffffe4d8) at test.c:9
9    ans = x / z;
{% endhighlight %}

So you can see here that dividing by zero wasn't the best career move for this program. But It nicely let us know that there was an arithmetic exception on line 9!

### Examining data

To get just the call stack information, we issue the "backtrace" instruction

{% highlight text %}
(gdb) backtrace
#0  0x00000000004004c3 in main (argc=1, argv=0x7fffffffe4d8) at test.c:9
{% endhighlight %}

Examining data with print. We can view the value of variables by passing them to the print statement.

{% highlight text %}
(gdb) print x
$1 = 1
(gdb) print y
$2 = 2
(gdb) print z
$3 = 0
{% endhighlight %}

Print also supports printf style specifiers. Examining data at an address. We can view the value of data at a memory location by using the `x` command.

{% highlight text %}
(gdb) print &z
$4 = (int *) 0x7fffffffe3e8
(gdb) x 0x7fffffffe3e8
0x7fffffffe3e8: 0x00000000
{% endhighlight %}

Setting variables while attached is done with `set`. We can stop the error in the program at runtime using this mixed with a breakpoint.

{% highlight text %}
Breakpoint 1, main (argc=1, argv=0x7fffffffe4d8) at test.c:6
6    int x = 1, y = 2, z = 0;
(gdb) next
7    int ans = 0;
(gdb) set (z = 5)
(gdb) print z
$1 = 5
{% endhighlight %}

### Working with breakpoints

To set a breakpoint in gdb, just pass the name of what you'd like to break on to the `break` command.

{% highlight text %}
(gdb) break main
Breakpoint 1 at 0x40049f: file test.c, line 6.
(gdb) run
Starting program: /home/michael/Development/gdb-tute/test 
Breakpoint 1, main (argc=1, argv=0x7fffffffe4d8) at test.c:6
6    int x = 1, y = 2, z = 0;
{% endhighlight %}

You can set a breakpoint at a specific source code line number:

{% highlight text %}
(gdb) break 7
Breakpoint 1 at 0x4004b4: file test.c, line 7.
{% endhighlight %}

Finally, you can make a breakpoint using a combination of source code file name and line location:

{% highlight text %}
(gdb) break test.c:7
Breakpoint 3 at 0x4004b4: file test.c, line 7.
{% endhighlight %}

View your existing breakpoints with `info breakpoints`

{% highlight text %}
(gdb) info breakpoints
Num     Type           Disp Enb Address            What
3       breakpoint     keep y   0x00000000004004b4 in main at test.c:7
{% endhighlight %}

You can clear any set breakpoint with `clear`.

{% highlight text %}
(gdb) clear main
Deleted breakpoint 1
{% endhighlight %}

You can step into code by issuing the `step` command  

{% highlight text %}
(gdb) step
7    int ans = 0;
(gdb) 
9    ans = x / z;
(gdb) 
Program received signal SIGFPE, Arithmetic exception.
0x00000000004004c3 in main (argc=1, argv=0x7fffffffe4d8) at test.c:9
9    ans = x / z;
{% endhighlight %}

You step over code by issuing `next`. Its usage is the same as `step`.

That's all for now. As I come across other useful bits from GDB, I'll certainly post them here. The items above have been lifesavers for me from time to time.