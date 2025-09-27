---
layout: post
title: Bloom Filters Made Simple
date: 2025-09-27
comments: false
categories: [ data-structures, algorithms, bloom-filters, python ]
---

# Introduction

A Bloom filter is a tiny, probabilistic memory that answers “Have I seen this before?” in constant time. It never lies 
with a **false negative**—if it says “no”, the item was definitely never added. But to save huge amounts of space versus 
storing all items, it allows false positives—sometimes it will say “probably yes” when collisions happen. 

The trick is simple: keep a row of bits and, for each item, flip a small handful of positions chosen by hash functions. 

Later, check those same positions. Any zero means “definitely not”; all ones means “probably yes.” With just a few 
bytes, Bloom filters help databases skip disk lookups, caches dodge misses, and systems answer membership queries 
blazingly fast.

# Pen & Paper Example

Let’s build the smallest possible Bloom filter: **10 bits**, indexed 0–9.  
We’ll use two playful “hash” functions:

{% highlight plain %}
h1(word) = (sum of letter positions) % 10 (a=1, b=2, …, z=26)
h2(word) = (len(word) * 3)           % 10
{% endhighlight %}

To **insert** a word, flip bits `h1(word)` and `h2(word)` to 1.  
To **query**, compute the same bits:
- If any bit is 0 → **definitely not present**  
- If both are 1 → **probably present**

With these rules in place, we can start to insert some words.

If we were to insert the word "cat":

{% highlight plain %}
h1("cat") = (3+1+20) = 24 %10=4  
h2("cat") = (3*3)    = 9  %10=9  
{% endhighlight %}

Flip bits 4 and 9.

Bits:  

{% highlight plain %}
idx:  0 1 2 3 4 5 6 7 8 9  
bits: 0 0 0 0 1 0 0 0 0 1
{% endhighlight %}

Similarly, we can try inserting the word "dog":

{% highlight plain %}
h1("dog") = (4+15+7) = 26 %10=6  
h2("dog") = (3*3)    = 9  %10=9  
{% endhighlight %}

Flip bits 6 and 9.

Bits:  

{% highlight plain %}
idx:  0 1 2 3 4 5 6 7 8 9  
bits: 0 0 0 0 1 0 1 0 0 1
{% endhighlight %}

We can now start to try a few queries. If we look for "cat", we can see that bits 4 and 9 are both 1, which indicates
that "cat" is **probably present**.

If we look for something that we haven't added yet, like "cow":

{% highlight plain %}
h1("cow") = (3+15+23) = 41 %10=1  
h2("cow") = (3*3)     = 9  %10=9  
{% endhighlight %}

Bit 1 is still 0 so that tells us that it's **definitely not present**.

# Python Demo (matching the example)

This code reproduces the steps above:

{% highlight python %}
from string import ascii_lowercase

# Map a..z -> 1..26
ABC = {ch: i+1 for i, ch in enumerate(ascii_lowercase)}

def h1(word: str, m: int) -> int:
    return sum(ABC.get(ch, 0) for ch in word.lower()) % m

def h2(word: str, m: int) -> int:
    return (len(word) * 3) % m

class Bloom:
    def __init__(self, m=10):
        self.m = m
        self.bits = [0]*m

    def _idxs(self, word):
        return [h1(word, self.m), h2(word, self.m)]

    def add(self, word):
        for i in self._idxs(word):
            self.bits[i] = 1

    def might_contain(self, word) -> bool:
        return all(self.bits[i] == 1 for i in self._idxs(word))

    def show(self):
        idx = "idx:  " + " ".join(f"{i}" for i in range(self.m))
        bts = "bits: " + " ".join(str(b) for b in self.bits)
        print(idx)
        print(bts)
        print()

bf = Bloom(m=10)

print("Insert 'cat'")
bf.add("cat")
bf.show()

print("Insert 'dog'")
bf.add("dog")
bf.show()

def check(w):
    print(f"Query '{w}':", "probably present" if bf.might_contain(w) else "definitely not present")

check("cat")
check("cow")
for w in ["dot", "cot", "tag", "god"]:
    check(w)
{% endhighlight %}

Run this and you’ll see the exact same evolution of bits as the “pen & paper” example.

{% highlight plain %}
Insert 'cat'
idx:  0 1 2 3 4 5 6 7 8 9
bits: 0 0 0 0 1 0 0 0 0 1

Insert 'dog'
idx:  0 1 2 3 4 5 6 7 8 9
bits: 0 0 0 0 1 0 1 0 0 1

Query 'cat': probably present
Query 'cow': definitely not present
Query 'dot': probably present
Query 'cot': definitely not present
Query 'tag': definitely not present
Query 'god': probably present
{% endhighlight %}

# Toward Real Bloom Filters

Our toy version used silly hash rules. Real implementations use cryptographic hashes and multiple derived functions. 
Here’s a slightly more realistic snippet using **double hashing** from SHA-256 and MD5:

{% highlight python %}
import hashlib

class RealishBloom:
    def __init__(self, m=128, k=4):
        self.m = m
        self.k = k
        self.bits = [0]*m

    def _hashes(self, word: str):
        w = word.encode()
        h = int.from_bytes(hashlib.sha256(w).digest(), "big")
        g = int.from_bytes(hashlib.md5(w).digest(), "big")
        for i in range(self.k):
            yield (h + i*g) % self.m

    def add(self, word: str):
        for i in self._hashes(word):
            self.bits[i] = 1

    def might_contain(self, word: str) -> bool:
        return all(self.bits[i] == 1 for i in self._hashes(word))
{% endhighlight %}

This implementation will allow you to filter much more complex (and longer) content. The wider your bit field is, and the 
more complex your hashing algorithms are, the better bit distribution you will get. This gives you a lower 
chance of false positives, improving the overall performance of the data structure.

# Conclusion

Bloom filters are elegant because of their simplicity: flip a few bits when adding, check those bits when querying. 
They trade absolute certainty for massive savings in memory and time. They’re everywhere—from browsers to databases to 
networking—and now, thanks to a handful of cat, dog, and cow, you know how they work.
