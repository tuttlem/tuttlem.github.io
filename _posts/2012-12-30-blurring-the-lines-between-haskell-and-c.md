---
layout: post
title: Blurring the lines between Haskell and C
date: 2012-12-30
comments: false
---

### Introduction

Being able to use parts of code that you have written in different languages in your environment of choice is a great productivity booster. For those particular problems you can really pick the tool that you need. Haskell and C are no exception to this. The Haskell website has a [great little write-up](http://www.haskell.org/haskellwiki/Calling_Haskell_from_C) on the topic. In today's post, I'm going to run you through the steps that I followed to get this running.

### Write your Haskell

The compilation process depends on your Haskell code being written first as GHC will generate some stub code for you. Here's a very simple and crude prime number tester:

{% highlight haskell %}
{- LANGUAGE ForeignFunctionInterface #-}                      
module Prime where                                            
                                                              
import Foreign.C.Types                                        

-- | Take a boolean and convert it to an integer
bool_to_int :: Bool -> Int                                    
bool_to_int False = 0                                         
bool_to_int True = 1                                          

-- | Brute force division style prime number testing
is_prime :: Int -> Int                                        
is_prime x = bool_to_int ptest                                
   where divisible = [n | n <- [3,5..(x-1)], x `mod` n == 0]  
         ptest     = length (divisible) == 0                  

-- | Interface exposed into C land
is_prime_hs :: CInt -> CInt                                   
is_prime_hs = fromIntegral . is_prime . fromIntegral          
                                                              
-- | Export symbols into C land                                                                                                                            
foreign export ccall is_prime_hs :: CInt -> CInt              
{% endhighlight %}

Ignoring my method of primes testing, you can see some interesting pieces in this Haskell source. On the first line we're enabling a GHC extension for the ForeignFucntionInterface. This allows us to export symbols to other languages. We have our implementation actually in the function `is_prime` with `is_prime_hs` being the callable wrapper from outside, in this case C. The last line actually exports our wrapper as a callable function.

### Haskell compilation

You've got your Haskell module ready for compilation, but it's going to be a little bit different. This source is supporting another application rather than containing a main function of its own.

{% highlight bash %}
$ ghc -c -O prime.hs
{% endhighlight %}

This command will compile our file only `-c` and optimise `-O`. A stub header file is generated for us (thanks to our FFI instructions on the first line of our Haskell file) that we can include in our main program.

### Write your C

It's now time to call our prime function. There is a little bit of administration <em>fluff</em> that we have to go through in order to get there, but it's really not much to worry about. Take note that we're including our stub header file that was generated for us in our Haskell compilation step.

{% highlight c %}
#include <HsFFI.h>                    
#ifdef __GLASGOW_HASKELL__            
#include "Prime_stub.h"               
extern void __stginit_Prime(void);    
#endif                                
                                      
#include <stdio.h>                    
                                      
int main(int argc, char *argv[]) {    
   int i;                             
   
   /* FFI initialization */
   hs_init(&argc, &argv);  

#ifdef __GLASGOW_HASKELL__            
   hs_add_root(__stginit_Prime);      
#endif                                

   /* determine if 13 is prime */
   i = is_prime_hs(13);               
   printf("is 13 prime? %d\n", i);    
  
   /* determine if 21 is prime */
   i = is_prime_hs(21);               
   printf("is 21 prime? %d\n", i);    
                                 
   /* teardown FFI */

   hs_exit();                         
   return 0;                          
}                                     
{% endhighlight %}

So that's pretty straight-forward C code in the end. It's a little awkward at first to look at the #define rigmarole at the top of the file, but you'll soon see straight past it. You can see at the top of the file that we've got an external symbol representing the module, `Primes`. This is used as a secondary initialisation step after we start up FFI (with `hs_init`). The call to `hs_add_root` is the extra initialisation required (per module we import - [I'm led to believe](http://hackage.haskell.org/trac/ghc/ticket/3252)) that we do for GHC's sake. Your C code is written, it's now time to compile, link and execute! Compilation to produce an executable looks like this.

{% highlight bash %}
$ ghc --make -no-hs-main -optc-O call_prime.c Prime -o test
{% endhighlight %}

We're telling ghc that we want to make `--make` our executable `-o test` that doesn't have a main routine in haskell source `-no-hs-main` and optimised by the c compiler `-optc-O`. We should have an executable, ready to go:

{% highlight text %}
$ ./test
is 13 prime? 1
is 21 prime? 0
{% endhighlight %}

It's not fireworks, but it's functional.