---
layout: post
title: Arbitrary length arithmetic with GMP
date: 2023-11-07
comments: false
categories: [ "math", "gmp", "c" ]
---

### Introduction

[GMP](https://gmplib.org/) is a library that will allow you to perform calculations on numbers that extend past the reach of what your standard data sizes can hold. From their website:

> GMP is a free library for arbitrary precision arithmetic, operating on signed integers, rational numbers, and floating-point numbers. There is no practical limit to the precision except the ones implied by the available memory in the machine GMP runs on. GMP has a rich set of functions, and the functions have a regular interface.

This means you can embed large math into your programs and will only be limited by the amount of memory on your running system.

In today's article, we'll go through a few simple examples on how to use this library.

### Getting setup

Get `gmp` installed locally on your system either by downloading the latest release from their site, or just using your package manager.

{% highlight bash %}
sudo apt-get install libgmp10
{% endhighlight %}

### Building

For any program that will require the `gmp` library, we'll need to add the `-lgmp` switch:

{% highlight bash %}
gcc test.c -o test -lgmp
{% endhighlight %}

Now, we're ready to go.

### Factorial

As an example implementation, we'll write a program to calculate the n'th factorial for us. First of all, we'll implement this using traditional data types offered to us through C, and then we'll swap this out to get greater numbers.

{% highlight c %}
/* test1.c */
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

/** Compute the n'th factorial */
int factorial(int n) {
  int i;
  int x = 1;

  for (i = 1; i <= n; ++i) {
    x = x * i;
  }

  return x;
}

int main(int argc, char *argv[]) {
  int x;

  /* check that a number was specified */
  if (argc <= 1) {
    printf("usage: %s <number>\n", argv[0]);
    return 1;
  }

  /* convert the input to a number */
  x = atoi(argv[1]);
  assert(x >= 0);

  printf("%d\n", factorial(x));

  return 0;
}
{% endhighlight %}

We build this application not needing the `gmp` library:

{% highlight bash %}
gcc test1.c -o test1
{% endhighlight %}

We can then start to test it out.

{% highlight text %}
$ ./test1 2  
2
$ ./test1 4
24
$ ./test1 5
120
$ ./test1 8
40320
$ ./test1 15
2004310016
{% endhighlight %}

The wheels start to fall off once we want to look at numbers higher than `!19`.

{% highlight text %}
$ ./test1 19
109641728
$ ./test1 20
-2102132736
{% endhighlight %}

We overflowed our integer to where it wrapped into negative numbers. `19` is our limit for traditional data types.

### Make the numbers bigger!

Now we can introduce `gmp` to help us break out of these constraints.

We'll rewrite the `factorial` function above to operate on `gmp` types, and we'll also convert the body of our program into a input capture function that will parse text into a `gmp` type for us.

Let's deal with the input first:

{% highlight c %}
void atoi_mpz(char *s, mpz_t res) {
  assert(s);
  
  int parse_result;

  /* allocate our number, and set an initial value of 0 */
  mpz_set_ui(res, 0);

  /* we assume base-10 when parsing */
  parse_result = mpz_set_str(res, s, 10);
  /* check that parsing was successful */
  assert(parse_result == 0);
}
{% endhighlight %}

First, you'll notice that we're not returning anything here. The `mpz_t` type is typed as an array, and as such can't be used as a return. So, we supply it as an output parameter. This pattern will reoccur through these examples.

This function also assumes that `res` has already had `mpz_init` run on it, so it's not magically allocating resources on your behalf.

`mpz_set_ui` sets the initial state of an `mpz_t` with a value from an integer (the real world!). `mpz_set_str` is really doing most of the work for us here, parsing out a string that its given into a `mpz_t`. The base needs to be supplied.

Now the `factorial` function will need to change as you'd expect:

{% highlight c %}
/** Compute the n'th factorial */
void factorial(mpz_t n, mpz_t res) {
  mpz_t i;

  /* initialize x and set it to n */
  mpz_set(res, n);

  /* initialize i to 1 */
  mpz_init(i);
  mpz_set_ui(i, 1);

  while (mpz_cmp(i, n) < 0) {
    mpz_add_ui(i, i, 1);
    mpz_mul(res, res, i);
  }
 
  /* clean up work vars */
  mpz_clear(i);
}
{% endhighlight %}

Again, `res` is an output parameter with no return value. We do have a "work" variable here, so we set it up and destroy it all within the context of our function so we don't have a memory leak.

`mpz_cmp` takes care of the looping for us. We've substituted a `for` loop here for a `while` loop to accommodate.

### Full program!

Now we can use these functions in a program of our own. Here's a full listing of an application that will give you the factorial of an arbitrary length integer.

{% highlight c %}
/* test2.c */
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include <gmp.h>

/** Compute the n'th factorial */
void factorial(mpz_t n, mpz_t res) {
  mpz_t i;

  /* inintialize x and set it to n */
  mpz_set(res, n);

  /* initialize i to 1 */
  mpz_init(i);
  mpz_set_ui(i, 1);

  while (mpz_cmp(i, n) < 0) {
    mpz_add_ui(i, i, 1);
    mpz_mul(res, res, i);
  }
 
  /* clean up work vars */
  mpz_clear(i);
}

void atoi_mpz(char *s, mpz_t res) {
  assert(s);
  
  int parse_result;

  /* allocate our number, and set an initial value of 0 */
  mpz_set_ui(res, 0);

  /* we assume base-10 when parsing */
  parse_result = mpz_set_str(res, s, 10);
  /* check that parsing was successful */
  assert(parse_result == 0);
}

int main(int argc, char *argv[]) {
  mpz_t x, f;

  /* check that a number was specified */
  if (argc <= 1) {
    printf("usage: %s <number>\n", argv[0]);
    return 1;
  }

  mpz_init(f);
  mpz_init(x);

  atoi_mpz(argv[1], x);
  factorial(x, f);

  mpz_out_str(stdout, 10, f);

  mpz_clear(x);
  mpz_clear(f);

  return 0;
{% endhighlight %}

We write the result number here with `mpz_out_str`. This can redirected to any stream of your choice.

Building this program is just adding the `lgmp` switch.

{% highlight bash %}
gcc test2.c -o test2 -lgmp
{% endhighlight %}

And, now you can calculate factorials as high as you like:

{% highlight text %}
$ ./test2 5
600
$ ./test2 50
1520704660085668902180630408303238442218882078448025600000000000000
$ ./test2 508
2600912719299986667129836551161461729376055224067622828051124610945736767140539144806269014448212661926636052457507242531933990603974750482248704223132435196370906188185711501338510076590779372857463616608930177975159159937930181522646905544337180113404654114353667383757400970162882750213805498362693213099688856210937449924868526775622360531973548790490474776119049615868165281085383657990705679209654697390907170752684309098744014502095309729615191639442748317478208592870187172780119819882401997425092893189361279318323502318101157291409635548371757588486399668194699833678157051688803686737440480747043533781525086057498368946374944369614791535224963761053372201191517194843602022799832918767988197763611904441080442538109356964428607751413413409857782381537871839805616626750150075526770316820978596423627176357508773710490144878649275528649481402223674961313005296813402084900146024228278827847133177811762165315793180635312968824483728409835808810724283538893336754191301602850516144797238826325624248081953676797029938259558400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
{% endhighlight %}
