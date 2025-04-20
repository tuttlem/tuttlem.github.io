---
layout: post
title: Fuzz testing C Binaries on Linux
date: 2025-04-20
comments: false
categories: [ "" ]
---

# Introduction

Fuzz testing is the art of breaking your software *on purpose*. By feeding random or malformed input into a program, we 
can uncover crashes, logic errors, or even security vulnerabilities — all without writing specific test cases.

In memory-unsafe languages like C, fuzzing is especially powerful. In just a few lines of shell script, we can hammer a 
binary until it falls over.

This guide shows how to fuzz a tiny C program using just `cat /dev/urandom`, and how to track down and analyze the 
crash with `gdb`.

# The Target

First off we need our test candidate. By design this program is vulnerable through its use of `strcpy`.

{% highlight c %}
#include <stdio.h>
#include <string.h>

void vulnerable(char *input) {
    char buffer[64];
    strcpy(buffer, input);  // Deliberately unsafe
}

int main() {
    char input[1024];
    fread(input, 1, sizeof(input), stdin);
    vulnerable(input);
    return 0;
}
{% endhighlight %}

In `main`, we're reading up to 1kb of data from `stdin`. This pointer is then sent into the `vulnerable` function. A 
buffer is defined in there *well under* the 1kb that could come through the front door.

`strcpy` doesn't care though. It'll try and grab as much data until it encounters a null terminator.

This is our problem.

Let's get this program built with some debugging information:

{% highlight bash %}
gcc -g -o vuln vuln.c
{% endhighlight %}

# Basic "Dumb" Fuzzer

We have plenty of tools at our disposal, directly at the linux console. So we can put together a fuzz tester albeit 
simple, without any extra tools here.

Here's `fuzzer.sh`:

{% highlight bash %}
# allow core dumps
ulimit -c unlimited

# send in some random data
cat /dev/urandom | head -c 100 | ./vuln
{% endhighlight %}

`100` bytes should be enough to trigger some problems internally.

Running the fuzzer, we should see something similar to this:

{% highlight text %}
*** stack smashing detected ***: terminated
[1]    4773 broken pipe                    cat /dev/urandom |
4774 done                                  head -c 100 |
4775 IOT instruction (core dumped)         ./vuln
{% endhighlight %}

We get some immediate feedback in `stack smashing detected`.

# Where's the Core Dump?

On modern Linux systems, core dumps don’t always appear in your working directory. Instead, they may be captured by 
`systemd-coredump` and stored elsewhere.

In order to get a list of core dumps, you can use `coredumpctl`:

{% highlight bash %}
coredumpctl list
{% endhighlight %}

You'll get a big report of all the core dumps that your system has gone through. You can use the PID that crashed to 
reference the dump that is specifically yours. 

{% highlight text %}
TIME                            PID  UID  GID SIG     COREFILE EXE            SIZE
Sun 2025-04-20 11:02:14 AEST   4775 1000 1000 SIGABRT present  /path/to/vuln  19.4K
{% endhighlight %}

# Debugging the dump

We can get our hands on these core dumps in a couple of ways. 

We can launch `gdb` directly via `coredumpctl`, and This will load the crashing binary and the core file into GDB. 

{% highlight bash %}
coredumpctl gdb 4775
{% endhighlight %}

I added the specific failing pid to my command, otherwise this will use the *latest* coredump.

Inside GDB:

{% highlight plain %}
bt              # backtrace
info registers  # cpu state at crash
list            # show source code around crash
{% endhighlight %}

Alternatively, if you want a phyical copy of the dump in your local directory you can get our hands on it with this:

{% highlight bash %}
coredumpctl dump --output=core.vuln
{% endhighlight %}

# AFL

Once you've had your fun with `cat /dev/urandom`, it's worth exploring more sophisticated fuzzers that generate inputs 
intelligently — like AFL (American Fuzzy Lop).

AFL instruments your binary to trace code coverage and then evolves inputs that explore new paths.

## Install

First of all, we need to install afl on our system.

{% highlight bash %}
pacman -S afl
{% endhighlight %}

## Running

Now we can re-compile our executable but this time with AFL's instrumentation:

{% highlight bash %}
afl-cc -g -o vuln-afl vuln.c
{% endhighlight %}

Before we can run our test, we need to create an input corpus. We create a minimal set of valid (or near-valid) inputs. 
AFL will use this input to mutate in other inputs.

{% highlight bash %}
mkdir input
echo "AAAA" > input/seed
{% endhighlight %}

Before we run, there will be some performance settings that you need to push out to the kernel first.

We need to tell the CPU to run at maximum frequency with the following:

{% highlight bash %}
cd /sys/devices/system/cpu
echo performance | tee cpu*/cpufreq/scaling_governor
{% endhighlight %}

For more details about these settings, have a look at the [CPU frequency scaling](https://wiki.archlinux.org/title/CPU_frequency_scaling#Scaling_governors) 
documentation.

Now, we run AFL!

{% highlight bash %}
mkdir output
afl-fuzz -i input -o output ./vuln-afl
{% endhighlight %}

You should now see a live updating dashboard like the following, detailing all of the events that are occuring through 
the many different runs of your application:

{% highlight text %}
american fuzzy lop ++4.31c {default} (./vuln-afl) [explore]          
┌─ process timing ────────────────────────────────────┬─ overall results ────┐
│        run time : 0 days, 0 hrs, 0 min, 47 sec      │  cycles done : 719   │
│   last new find : none yet (odd, check syntax!)     │ corpus count : 1     │
│last saved crash : none seen yet                     │saved crashes : 0     │
│ last saved hang : none seen yet                     │  saved hangs : 0     │
├─ cycle progress ─────────────────────┬─ map coverage┴──────────────────────┤
│  now processing : 0.2159 (0.0%)      │    map density : 12.50% / 12.50%    │
│  runs timed out : 0 (0.00%)          │ count coverage : 449.00 bits/tuple  │
├─ stage progress ─────────────────────┼─ findings in depth ─────────────────┤
│  now trying : havoc                  │ favored items : 1 (100.00%)         │
│ stage execs : 39/100 (39.00%)        │  new edges on : 1 (100.00%)         │
│ total execs : 215k                   │ total crashes : 0 (0 saved)         │
│  exec speed : 4452/sec               │  total tmouts : 0 (0 saved)         │
├─ fuzzing strategy yields ────────────┴─────────────┬─ item geometry ───────┤
│   bit flips : 0/0, 0/0, 0/0                        │    levels : 1         │
│  byte flips : 0/0, 0/0, 0/0                        │   pending : 0         │
│ arithmetics : 0/0, 0/0, 0/0                        │  pend fav : 0         │
│  known ints : 0/0, 0/0, 0/0                        │ own finds : 0         │
│  dictionary : 0/0, 0/0, 0/0, 0/0                   │  imported : 0         │
│havoc/splice : 0/215k, 0/0                          │ stability : 100.00%   │
│py/custom/rq : unused, unused, unused, unused       ├───────────────────────┘
│    trim/eff : 20.00%/1, n/a                        │          [cpu000: 37%]
└─ strategy: explore ────────── state: started :-) ──
{% endhighlight %}

Unlike `/dev/urandom`, AFL:

* Uses feedback to mutate inputs intelligently
* Tracks code coverage
* Detects crashes, hangs, and timeouts
* Can auto-reduce inputs that cause crashes

It’s like the `/dev/urandom` method — but on steroids, with data-driven evolution.

The `/output` folder will hold all the telemetry from the many runs that AFL is currently performing. Any crashes and 
hangs are kept later for your inspection. These are just core dumps that you can use again with gdb.


# Conclusion

Fuzzing is cheap, dumb, and *shockingly effective*. If you’re writing C code, run a fuzzer against your tools. You may
find bugs that formal tests would never hit — and you’ll learn a lot about your program's internals in the process.

If you're interested in going deeper, check out more advanced fuzzers like:

- **[AFL](https://github.com/AFLplusplus/AFLplusplus)** (American Fuzzy Lop): coverage-guided fuzzing via input mutation
- **[LibFuzzer](https://llvm.org/docs/LibFuzzer.html)**: fuzzing entry points directly in code
- **[Honggfuzz](https://github.com/google/honggfuzz)**: another smart fuzzer with sanitizer integration
- **[AddressSanitizer](https://github.com/google/sanitizers/wiki/addresssanitizer)** (ASan): not a fuzzer, but an excellent runtime checker for memory issues

These tools can take you from basic input crashes to deeper vulnerabilities, all without modifying too much of your workflow.

Happy crashing.
