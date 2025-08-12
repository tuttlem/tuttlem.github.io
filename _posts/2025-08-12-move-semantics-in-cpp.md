---
layout: post
title: Move Semantics in C++
date: 2025-08-12
tags: [c++, language, performance, semantics]
---

> TL;DR: `std::move` doesn’t move anything by itself. It’s a cast that **permits** moving. Real moves happen in your 
> type’s move constructor/assignment. Use them to trade deep copies for cheap pointer swaps and to unlock container 
> performance—*provided* you mark them `noexcept`.

## The motivating example 

We’ll anchor everything on a tiny heap-owning type. It’s intentionally "unsafe" (raw `new[]`/`delete[]`) so the 
ownership transfer is easy to see in logs.

{% highlight cpp %}
#include <iostream>
#include <utility> // for std::move

struct my_object {
    int* data;
    size_t size;

    // Constructor
    my_object(size_t n) : data(new int[n]), size(n) {
        std::cout << "Constructed (" << this << ") size=" << size 
                  << " data=" << data << "\n";
    }

    // Copy constructor
    my_object(const my_object& other) 
        : data(new int[other.size]), size(other.size) {
        std::copy(other.data, other.data + size, data);
        std::cout << "Copied from (" << &other << ") to (" << this << ")"
                  << " data=" << data << "\n";
    }

    // Move constructor
    my_object(my_object&& other) noexcept 
        : data(other.data), size(other.size) {
        other.data = nullptr;
        other.size = 0;
        std::cout << "Moved from (" << &other << ") to (" << this << ")"
                  << " data=" << data << "\n";
    }

    // Destructor
    ~my_object() {
        std::cout << "Destroying (" << this << ") data=" << data << "\n";
        delete[] data;
    }
};

int main() {
    std::cout << "--- Create obj1 ---\n";
    my_object obj1(5);

    std::cout << "\n--- Copy obj1 into obj2 ---\n";
    my_object obj2 = obj1; // Calls copy constructor

    std::cout << "\n--- Move obj1 into obj3 ---\n";
    my_object obj3 = std::move(obj1); // Calls move constructor

    std::cout << "\n--- End of main ---\n";
}
{% endhighlight %}

When you run this you’ll see:

* One deep allocation
* One deep copy (new buffer), and 
* One *move* (no allocation; just pointer steal). 

The destructor logs reveal that ownership was transferred and that the moved-from object was neutered.

**Try it**: `clang++ -std=c++20 -O0 -g move_demo.cpp && ./a.out`

Having a brief look at the output (from my machine, at least):

{% highlight text %}
--- Create obj1 ---
Constructed (0x7ffd8c960858) size=5 data=0x5616824336c0

--- Copy obj1 into obj2 ---
Copied from (0x7ffd8c960858) to (0x7ffd8c960848) data=0x5616824336e0

--- Move obj1 into obj3 ---
Moved from (0x7ffd8c960858) to (0x7ffd8c960838) data=0x5616824336c0

--- End of main ---
Destroying (0x7ffd8c960838) data=0x5616824336c0
Destroying (0x7ffd8c960848) data=0x5616824336e0
Destroying (0x7ffd8c960858) data=0
{% endhighlight %}

* Constructed: `obj1` allocates a buffer at `0x5616824336c0`.
* Copied: `obj2` gets its own buffer (`0x5616824336e0`) and the contents are duplicated from `obj1`.
At this point, both `obj1` and `obj2` own separate allocations.
* Moved: `obj3` simply takes ownership of `obj1`’s buffer (`0x5616824336c0`) without allocating.
`obj1`’s data pointer is nulled out (`data=0`), leaving it valid but empty.
* Destruction order: `obj3` frees `obj1`’s original buffer, `obj2` frees its own copy, and finally `obj1` frees nothing (because it’s been neutered by the move).

The contrasting addresses make it easy to see:

* Copies produce **different** data pointers.
* Moves result in **pointer reuse**.

## What problem do move semantics solve?

Before C++11, passing/returning big objects often meant **deep copies** or awkward workarounds. Containers like 
`std::vector<T>` also had a problem: on reallocation they could only copy elements. If copying `T` was expensive or 
forbidden, performance cratered.

**Move semantics (C++11)** let a type say: *“If you no longer need the source object, I can steal its resources 
instead of allocating/copying them.”* This unlocks:

- Returning large objects by value efficiently.
- Growing containers without copying payloads.
- Expressing one-time ownership transfers cleanly.

# Conclusion

In this small example we only wrote a move constructor, but real-world resource-owning classes often need both move 
and copy operations, plus move assignment. The full “rule of five” ensures your type behaves correctly in all 
situations — and marking moves `noexcept` can make a big difference in container performance.

Move semantics solves a big problem especially when your class encapsulates a lot of data. It's an elegant solution that
C++ provides you for performance, ownership, and safety.