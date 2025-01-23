---
layout: post
title: Implementing an LRU Cache in Rust
date: 2025-01-23
comments: false
categories: [ "cache", "lru", "rust" ]
---

# Introduction

When building high-performance software, caches often play a vital role in optimizing performance by reducing redundant 
computations or avoiding repeated I/O operations. One such common caching strategy is the Least Recently Used (LRU) 
cache, which ensures that the most recently accessed data stays available while evicting the least accessed items when 
space runs out.

## What Is an LRU Cache?

At its core, an LRU cache stores a limited number of key-value pairs. When you access or insert an item:

* If the item exists, it is marked as "recently used."
* If the item doesn't exist and the cache is full, the least recently used item is evicted to make space for the new one.

LRU caches are particularly useful in scenarios where access patterns favor recently used data, such as:

* **Web page caching** in browsers.
* **Database query caching** for repeated queries.
* **API response caching** to reduce repeated external requests.

In this post, we'll build a simple and functional implementation of an LRU cache in Rust. Instead of diving into 
complex data structures like custom linked lists, we'll leverage Rust's standard library collections 
(`HashMap` and `VecDeque`) to achieve:

* Constant-time access and updates using `HashMap`.
* Efficient tracking of usage order with `VecDeque`.

* This straightforward approach is easy to follow and demonstrates Rust's powerful ownership model and memory safety.

# `LRUCache` Structure

We’ll begin with a struct that defines the cache:

{% highlight rust %}
pub struct LRUCache<K, V> {
    capacity: usize,                 // Maximum number of items the cache can hold
    map: HashMap<K, V>,              // Key-value store
    order: VecDeque<K>,              // Tracks the order of key usage
}
{% endhighlight %}

This structure holds:

1. `capacity`: The maximum number of items the cache can store.
2. `map`: The main storage for key-value pairs.
3. `order`: A queue to maintain the usage order of keys.

# Implementation

Our implementation of `LRUCache` includes some constraints on the generic types `K` (key) and `V` (value). Specifically, 
the `K` type requires the following traits:

{% highlight rust %}
impl<K: Clone + Eq + std::hash::Hash + PartialEq, V> LRUCache<K, V> {
}
{% endhighlight %}

The `Clone` trait allows us to create a copy of the key when needed (via `.clone()`). `Eq` is a trait that ensure that
keys can be compared for equality and are either strictly equal or not. The `Hash` trait enables us to hash the keys 
which is a requirement for using `HashMap`, and finally the `PartialEq` trait allows for equality comparisons between 
two keys.

Technically `Eq` should already imply `PartialEq` but we explicity include it here for clarity.

## Create the Cache

To initialize the cache, we add a `new` method:

{% highlight rust %}
pub fn new(capacity: usize) -> Self {
    LRUCache {
        capacity,
        map: HashMap::with_capacity(capacity),
        order: VecDeque::with_capacity(capacity),
    }
}
{% endhighlight %}

* `HashMap::with_capacity`: Preallocates space for the HashMap to avoid repeated resizing.
* `VecDeque::with_capacity`: Allocates space for tracking key usage.

## Value access via `get`

The `get` method retrieves a value by key and updates its usage order:

{% highlight rust %}
pub fn get(&mut self, key: &K) -> Option<&V> {
    if self.map.contains_key(key) {
        // Move the key to the back of the order queue
        self.order.retain(|k| k != key);
        self.order.push_back(key.clone());
        self.map.get(key)
    } else {
        None
    }
}
{% endhighlight %}

* Check if the key exists via `contains_key`
* Remove the key from its old position in `order` and push it to the back
* Return the vlaue from the `HashMap`

In cases where a value never existed or has been evicted, this function sends `None` back to the caller.

## Value insertion via `put`

The `put` method adds a new key-value pair or updates an existing one:

{% highlight rust %}
pub fn put(&mut self, key: K, value: V) {
    if self.map.contains_key(&key) {
        // Update existing key's value and mark it as most recently used
        self.map.insert(key.clone(), value);
        self.order.retain(|k| k != &key);
        self.order.push_back(key);
    } else {
        if self.map.len() == self.capacity {
            // Evict the least recently used item
            if let Some(lru_key) = self.order.pop_front() {
                self.map.remove(&lru_key);
            }
        }
        self.map.insert(key.clone(), value);
        self.order.push_back(key);
    }
}
{% endhighlight %}

* If the key exists
  * The value is updated in `map`
  * The key is moved to the back of `order`

* If the cache is full
  * Remove the least recently used key (which will be the front of `order`) from `map`

* Insert the new key-value pair and mark it as recently used

## Size

Finally, we add a helper method to get the current size of the cache:

{% highlight rust %}
pub fn len(&self) -> usize {
    self.map.len()
}
{% endhighlight %}

# Testing

Now we can test our cache:

{% highlight rust %}
fn main() {
    let mut cache = LRUCache::new(3);

    cache.put("a", 1);
    cache.put("b", 2);
    cache.put("c", 3);

    println!("{:?}", cache.get(&"a")); // Some(1)
    cache.put("d", 4); // Evicts "b"
    println!("{:?}", cache.get(&"b")); // None
    println!("{:?}", cache.get(&"c")); // Some(3)
    println!("{:?}", cache.get(&"d")); // Some(4)
}
{% endhighlight %}

Running this code, we see the following:

{% highlight text %}
Some(1)
None
Some(3)
Some(4)
{% endhighlight %}

# Conclusion

In this post, we built a simple yet functional LRU cache in Rust. A full implementation can be found as a gist [here](https://gist.github.com/tuttlem/e9da56a2693a550f3ffc3a075fd4925e).

While this implementation is perfect for  understanding the basic principles, it can be extended further with:

* **Thread safety** using synchronization primitives like Mutex or RwLock.
* **Custom linked structures** for more efficient eviction and insertion.
* **Diagnostics and monitoring** to observe cache performance in real-world scenarios.

If you're looking for a robust cache for production, libraries like [lru](https://crates.io/crates/lru) offer feature-rich implementations. But for 
learning purposes, rolling your own cache is an excellent way to dive deep into Rust’s collections and ownership model.
