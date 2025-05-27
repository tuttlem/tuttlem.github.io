---
layout: post
title: Tagged Unions
date: 2025-05-27
comments: false
categories: [ c, memory, types, systems, lowlevel ]
---

# Introduction

In [a previous post]({% post_url 2024-10-16-make-your-own-variant-data-type %}), we built a dynamic variant type in C 
that could safely hold integers, floats, strings, and arrays — all while tracking its current type.

That implementation used a simple, effective trick from the C toolbox: the **tagged union**.

But what *is* a tagged union, really? What does it look like in memory? How does it behave across different languages 
like Rust or Swift? And how can understanding this help you write better low-level code?

In this article, we’ll go deep into the mechanics of tagged unions — demystifying how they work, how they’re laid out 
in memory, and how other languages leverage this idea at a higher level.

# What is a Tagged Union?

A **tagged union** is a structure that lets you store different types of data in the *same* memory space — but only 
*one* of them at a time.

It consists of two parts:

1. A **tag** (also called a discriminator or type id) — this tells you what kind of value the union currently holds.
2. A **union** — this is a single memory region that can represent multiple types, but only one is valid at any given time.

Together, they form a **tagged union**, also known as:
- Variant type (C++)
- Discriminated union (TypeScript)
- Sum type (Haskell, Rust)
- `enum` with payloads (Rust, Swift, F#)

# A Simple C Implementation

Let’s take a minimal version of the `ced_var_t` type we saw earlier:

{% highlight c %}
typedef enum {
    TYPE_INT32,
    TYPE_FLOAT,
    TYPE_STRING,
} value_type_t;

typedef struct {
    value_type_t type;

    union {
        int32_t   i32;
        float     f;
        char*     str;
    } data;
} tagged_value_t;
{% endhighlight %}

Here:
- `type` is the tag.
- `data` is the payload.

When you store a value, you must set both correctly:

{% highlight c %}
tagged_value_t v;
v.type = TYPE_INT32;
v.data.i32 = 42;
{% endhighlight %}

When you want to read the value, you must check the tag first.

# What Does This Look Like in Memory?

Let’s visualize what’s in memory for the above:

{% highlight text %}
+--------+---------+------------------+
| tag    | padding | data             |
| (4B)   | (4B)    | (8B - union)     |
+--------+---------+------------------+
{% endhighlight %}

Because `str` is a `char*`, which is typically 8 bytes on 64-bit systems, the entire union needs to reserve 
**8 bytes**. So the full struct is:
- 4 bytes for `type` (as "tag")
- 4 bytes padding (for alignment)
- 8 bytes for union

**Total: 16 bytes**

Even if you’re only storing a small `int32_t`, the full memory block remains the same.

{% include callout.html type="info" title="Keep in mind!" text="This is a classic example of how low-level memory alignment and padding rules affect real-world struct layout — and why tools like sizeof() sometimes surprise you." %}


# Why Is This Useful?

Tagged unions give you flexibility:
- Store different types dynamically
- Inspect and branch on their type at runtime
- Compact representation (compared to `void* + metadata` hacks)
- Zero-cost abstraction in low-level code

They’re great for interpreters, configuration systems, scripting languages, or anything where types can vary 
dynamically.

# But It’s Also Dangerous

C won’t stop you from misusing the union:

{% highlight c %}
v.type = TYPE_FLOAT;
v.data.i32 = 1234;  // Invalid — tag and payload don't match!
{% endhighlight %}

Accessing a union member that doesn't match the tag is **undefined behavior**. It might work. Or it might crash. Or 
silently corrupt your data.

That’s why **you must manage the tag manually**, and treat it as the single source of truth.

# How Rust Does It Better

In Rust, you’d write this as:

{% highlight rust %}
enum Value {
    Int32(i32),
    Float(f32),
    Str(String),
}
{% endhighlight %}

Rust does three major things better:
1. **Enforces valid construction** — you can’t create an invalid state.
2. **Requires match exhaustiveness** — you have to handle all cases.
3. **Uses niche optimization** — it can sometimes omit the tag entirely.

For example, `Option<&T>` takes no extra space. Rust uses the null pointer as the tag for `None`.

# Comparison with Other Languages

| Language     | Feature                     | Example                                 |
|--------------|-----------------------------|-----------------------------------------|
| C            | Tagged union (manual)       | `struct + union + enum`                 |
| Rust         | Sum type with safety        | `enum Value { Int(i32), ... }`          |
| Swift        | `enum` with associated data | `enum Value { case int(Int), ... }`     |
| TypeScript   | Discriminated union         | `{ kind: "int", value: number }`        |
| Haskell/OCaml| Algebraic data type         | `data Value = Int Int | Float Float`    |

# Performance Considerations

Tagged unions are generally compact, but there are trade-offs:

- The **size of the union** is dictated by the **largest** member.
- On 64-bit platforms, alignment often causes padding (e.g., 4-byte tag + 4-byte pad + 8-byte payload).
- If a variant holds something heap-allocated (like strings), you may incur pointer-chasing and memory fragmentation.

You can use tools like `sizeof()`, `offsetof()`, or `pahole` to understand the exact layout.

# Debugging Tips

If you’re building or debugging a tagged union system in C:

- Use `assert()` to guard tag-to-union access:
  {% highlight c %}
  assert(v->type == TYPE_INT32);
  {% endhighlight %}

- Use `valgrind` to catch misuses or memory leaks.
- Inspect raw memory using `gdb`, `hexdump`, or print helpers.
- Consider printing the tag as a string to make logs easier to read.

# Further Reading

- [Rust enum layout documentation](https://doc.rust-lang.org/reference/type-layout.html)
- [Valgrind](https://valgrind.org/)
- [pahole: show data structure layout](https://linux.die.net/man/1/pahole)
