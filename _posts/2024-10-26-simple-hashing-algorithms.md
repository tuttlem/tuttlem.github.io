---
layout: post
title: Simple Hashing Algorithms
date: 2024-10-26
comments: false
categories: [ "hash" ]
---

# Introduction

Hash functions are essential in computer science, widely used in data structures, cryptography, and applications like 
file integrity checks and digital signatures. This post will explore a few well-known hash functions: **djb2**, 
**Jenkins**, **Murmur3**, and **FNV-1a**. We'll discuss each function's approach and use cases, then dive into sample C 
implementations for each.

# What is a Hash Function?

In brief, a hash function takes input data (e.g., a string) and returns a fixed-size integer, or "hash," that represents 
the original data. Ideal hash functions distribute data uniformly, minimizing collisions (when different inputs generate 
the same hash value). While cryptographic hash functions like SHA-256 prioritize security, our focus here will be on 
non-cryptographic hashes for tasks such as data lookup and unique identifier generation.

## djb2

The **djb2** hash function, designed by Daniel J. Bernstein, is a simple yet effective algorithm for hashing strings. 
Its operation is lightweight, using bit shifts and additions, making it fast and suitable for many non-cryptographic 
purposes. The main advantage of `djb2` lies in its simplicity, which is also why it is commonly used in hash table 
implementations.

### Code

{% highlight c %}
/**
 * @brief Hashes a string using the djb2 algorithm
 * @param str The string to hash
 * @return The hash of the string
 */
uint64_t ced_hash_djb2(const void *key, size_t len) {
    uint64_t hash = 5381;
    const unsigned char *str = key;

    while (len--) {
        hash = ((hash << 5) + hash) + *str++;
    }

    return hash;
}
{% endhighlight %}

### Explanation

In `djb2`, we initialize the hash with `5381` and iterate over each character of the string. The main hashing logic is 
`hash = ((hash << 5) + hash) + *str++`, which essentially combines shifts and additions for a computationally light 
transformation.

## Jenkins Hash

The **Jenkins** hash function, created by Bob Jenkins, is popular for its performance and quality of distribution. 
Jenkins functions are commonly used for hash tables and are generally effective at handling common hashing requirements 
without high computational overhead.

### Code

{% highlight c %}
/**
 * @brief Hashes a string using the Jenkins algorithm
 * @param key The key to hash
 * @param length The length of the key
 * @return The hash of the string
 */
uint32_t ced_hash_jenkins(const void *key, size_t length) {
    uint32_t hash = 0;
    const uint8_t *data = (const uint8_t *)key;

    for (size_t i = 0; i < length; ++i) {
        hash += data[i];
        hash += (hash << 10);
        hash ^= (hash >> 6);
    }

    hash += (hash << 3);
    hash ^= (hash >> 11);
    hash += (hash << 15);

    return hash;
}
{% endhighlight %}

### Explanation

In this implementation, each byte of the input affects the entire hash state via a series of shifts and XORs. These 
bitwise operations mix the bits thoroughly, helping reduce the chances of hash collisions, especially for small or 
repetitive data inputs.

## Murmur3

**Murmur3** is part of a family of hash functions known for their speed and good distribution characteristics. Designed 
by Austin Appleby, Murmur3 performs exceptionally well on large datasets and is commonly used in database indexing, 
distributed systems, and other applications where hash quality and performance are paramount.

### Code

{% highlight c %}
/**
 * @brief Hashes a string using the Murmur3 algorithm
 * @param key The key to hash
 * @param length The length of the key
 * @param seed The seed value for the hash
 * @return The hash of the string
 */
uint32_t ced_hash_murmur(const void *key, size_t length, uint32_t seed) {
    const uint32_t m = 0x5bd1e995;
    const int r = 24;

    uint32_t hash = seed ^ length;
    const uint8_t *data = (const uint8_t *)key;

    while (length >= 4) {
        uint32_t k = *(uint32_t *)data;

        k *= m;
        k ^= k >> r;
        k *= m;

        hash *= m;
        hash ^= k;

        data += 4;
        length -= 4;
    }

    switch (length) {
        case 3: hash ^= data[2] << 16;
        case 2: hash ^= data[1] << 8;
        case 1: hash ^= data[0];
            hash *= m;
    }

    hash ^= hash >> 13;
    hash *= m;
    hash ^= hash >> 15;

    return hash;
}
{% endhighlight %}

### Explanation

Murmur3 processes input in 4-byte chunks, applying a seed-based hashing operation with bit shifts to achieve randomness. 
This function is optimized for speed and provides excellent performance, particularly in scenarios where uniform hash 
distribution is critical.

## FNV-1a

The **FNV-1a** hash is another widely used, fast, non-cryptographic hash function. It is well-known for its simplicity 
and reasonable distribution properties. FNV-1a is often used for smaller data structures like hash tables and is 
compatible with both small and large datasets.

{% highlight c %}
/**
 * @brief Hashes a string using the FNV1a algorithm
 * @param key The key to hash
 * @param length The length of the key
 * @return The hash of the string
 */
uint32_t ced_hash_fnv1a(const void *key, size_t length) {
    const uint32_t offset_basis = 2166136261;
    const uint32_t prime = 16777619;

    uint32_t hash = offset_basis;
    const uint8_t *data = (const uint8_t *)key;

    for (size_t i = 0; i < length; ++i) {
        hash ^= data[i];
        hash *= prime;
    }

    return hash;
}
{% endhighlight %}

### Explanation

FNV-1a initializes a hash value with an offset basis and iterates over each byte, XORing it with the hash and then 
multiplying by a prime number. This operation sequence yields a well-distributed hash while maintaining simplicity.

# Summary

Each of the four hash functions reviewed here has distinct characteristics:

* djb2: Simple and efficient, suitable for smaller data and hash tables.
* Jenkins: Offers good distribution with minimal computation, ideal for hash tables.
* Murmur3: Fast and optimized for larger data, making it ideal for database indexing and distributed applications.
* FNV-1a: Simple and widely used, especially in situations where lightweight and straightforward hash computation is required.

Choosing the right hash function depends on the specific requirements of your application, particularly the trade-offs 
between speed, distribution quality, and memory usage. The implementations shared here provide a starting point for 
integrating efficient hashing techniques in C.