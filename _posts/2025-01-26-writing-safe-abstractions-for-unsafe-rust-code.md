---
layout: post
title: Writing Safe Abstractions for Unsafe Rust Code
date: 2025-01-26
comments: false
categories: [ "rust", "unsafe" ]
---

# Introduction

Rust is celebrated for its emphasis on safety and performance, largely thanks to its robust compile-time checks. 
However, there are situations where you need to bypass these checks to perform low-level operations—this is where 
Rust's `unsafe` keyword comes in. While `unsafe` opens the door to powerful features, it also comes with significant 
risks. 

The solution? 

Encapsulating unsafe code in safe abstractions. 

This post explores what that means, why it’s important, and how to do it effectively.

## Understanding unsafe in Rust

Rust enforces strict memory safety guarantees by default. However, some operations are inherently unsafe and require 
explicit acknowledgment from the programmer. These include:

* **Raw pointer manipulation**: Directly accessing memory without bounds or validity checks.
* **Foreign Function Interface (FFI)**: Interacting with non-Rust code (e.g., calling C functions).
* **Manual memory management**: Allocating and freeing memory without Rust’s usual safeguards.
* **Concurrency primitives**: Implementing data structures that require custom synchronization logic.

When you write `unsafe` code, you’re essentially telling the compiler, _“I know what I’m doing; trust me.”_ 

While this is sometimes necessary, it’s critical to minimize the potential for misuse by others.

## Why Wrap Unsafe Code in Safe Abstractions?

Using `unsafe` is a trade-off. It gives you access to low-level features and optimizations but requires you to 
manually uphold the invariants that Rust would otherwise enforce. Safe abstractions address this challenge by:

* **Avoiding Undefined Behavior**: Preventing common pitfalls like null pointer dereferences, data races, or buffer overflows.
* **Improving Maintainability**: Reducing the scattering of `unsafe` blocks across the codebase makes it easier to audit and debug.
* **Providing Ease of Use**: Enabling most developers to rely on Rust’s safety guarantees without needing to understand the intricacies of the underlying `unsafe` implementation.

# What is a Safe Abstraction?

A safe abstraction is an API or module where the internal implementation may use unsafe code, but the external 
interface ensures that incorrect usage is either impossible or extremely difficult. 

Let’s look at how to create one.

## Example: Safe Wrapping of Unsafe Memory Allocation

Here’s a simplified example of wrapping unsafe memory management into a safe abstraction:

{% highlight rust %}
pub struct SafeAllocator {
    // Internal raw pointer or other unsafe constructs
    ptr: *mut u8,
    size: usize,
}

impl SafeAllocator {
    pub fn new(size: usize) -> Self {
        let ptr = unsafe { libc::malloc(size) as *mut u8 };
        if ptr.is_null() {
            panic!("Failed to allocate memory");
        }
        Self { ptr, size }
    }

    pub fn allocate(&self, offset: usize, len: usize) -> &[u8] {
        if offset + len > self.size {
            panic!("Out of bounds access");
        }
        unsafe {
            std::slice::from_raw_parts(self.ptr.add(offset), len)
        }
    }

    pub fn deallocate(self) {
        unsafe {
            libc::free(self.ptr as *mut libc::c_void);
        }
    }
}

impl Drop for SafeAllocator {
    fn drop(&mut self) {
        unsafe {
            libc::free(self.ptr as *mut libc::c_void);
        }
    }
}
{% endhighlight %}

In this example:

* `unsafe` is confined to specific, well-defined sections of the code.
* The API ensures that users cannot misuse the allocator (e.g., by accessing out-of-bounds memory).
* `Drop` ensures memory is automatically freed when the allocator goes out of scope.

## Example Usage of `SafeAllocator`

Here’s how you might use the `SafeAllocator` in practice:

{% highlight rust %}
fn main() {
    // Create a new SafeAllocator with 1024 bytes of memory
    let allocator = SafeAllocator::new(1024);

    // Allocate a slice of 128 bytes starting from offset 0
    let slice = allocator.allocate(0, 128);
    println!("Allocated slice of length: {}", slice.len());

    // The allocator will automatically deallocate memory when it goes out of scope
}
{% endhighlight %}

This usage demonstrates:

* How to create and interact with the SafeAllocator API.
* That memory is automatically managed via Rust’s Drop trait, preventing leaks.

# Leveraging Rust’s Type System

Rust’s type system is another powerful tool for enforcing invariants. For example, you can use:

* **Lifetimes**: To ensure references don’t outlive the data they point to.
* **PhantomData**: To associate types or lifetimes with otherwise untyped data.
* **Ownership and Borrowing Rules**: To enforce safe access patterns at compile time.

# Documentation of Safety Contracts

Any `unsafe` code should include clear documentation of the invariants it relies on. For example:

{% highlight rust %}
// Safety:
// - `ptr` must be non-null and point to a valid memory region.
// - `len` must not exceed the bounds of the allocated memory.
unsafe {
    std::slice::from_raw_parts(ptr, len)
}
{% endhighlight %}

This makes it easier for future maintainers to understand and verify the correctness of the code.

# Real-World Examples of Safe Abstractions

Many Rust libraries provide excellent examples of safe abstractions over `unsafe` code:

* `std::sync::Mutex`: Internally uses unsafe for thread synchronization but exposes a safe API for locking and unlocking.
* `Vec`: The Rust standard library’s `Vec` type uses `unsafe` for raw memory allocation and resizing but ensures bounds checks and proper memory management externally.
* `crossbeam`: Provides safe concurrency primitives built on low-level atomic operations.

# Costs and Benefits

While writing safe abstractions requires extra effort and careful thought, the benefits outweigh the costs:

## Benefits:

* **Reduced Risk of Bugs**: Encapsulating unsafe code minimizes the chance of introducing undefined behavior.
* **Improved Developer Experience**: Safe APIs make it easier for others to use your code without worrying about low-level details.
* **Easier Auditing**: With unsafe code isolated, it’s easier to review and verify its correctness.

## Costs:

* **Initial Effort**: Designing a robust safe abstraction takes time and expertise.
* **Performance Overhead**: In rare cases, adding safety layers may incur slight overhead (though usually negligible in well-designed abstractions).

# Conclusion

Writing safe abstractions for unsafe Rust code is both an art and a science. It involves understanding the invariants 
of your `unsafe` code, leveraging Rust’s type system to enforce safety, and documenting your assumptions clearly. By 
doing so, you can harness the power of `unsafe` while maintaining Rust’s guarantees of memory safety and concurrency 
correctness—the best of both worlds.

