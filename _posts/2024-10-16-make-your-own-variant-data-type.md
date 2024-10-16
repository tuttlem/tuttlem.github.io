---
layout: post
title: Make Your Own Variant Data Type
date: 2024-10-16
comments: false
categories: [ "variant", "c", "programming" ]
---

# Introduction

In software development, we often encounter scenarios where we need to store or manipulate data of varying types—integers,
strings, floating points, and more. Typically, each data type is handled separately, but what if you could encapsulate
different types within a single structure? This is where a **variant data type** comes in.

A variant is a type-safe container that can hold any type of value, while keeping track of what type it currently holds.
This makes variants incredibly useful in situations where your data structure needs to handle multiple data types
dynamically, such as in scripting languages, serialization systems, or general-purpose containers.

In this article, we’ll walk through how to implement your own variant data type in C. We'll start by defining the types
that our variant can handle, move on to constructing the variant itself, and finish with operations like cloning,
converting, and freeing variants. The goal is to provide you with a reusable component that can serve as a foundation
for more complex systems, such as interpreters, data structures, or even custom languages.

# Defining a Variant Data Type

The first step in implementing a variant data type is to define what types the variant can hold. In C, we can use an
`enum` to list all the possible types we want to support. For our variant, we'll handle everything from basic types
like integers and floats to more complex types like strings and arrays.

We’ll also define a union within our variant structure. The union allows us to store different data types in the same
memory space while ensuring that we only ever use one at a time, depending on the type of the variant.

Here’s the `enum` and the `union` for our variant type:

{% highlight c %}
typedef enum {
ced_var_type_null = 0,
ced_var_type_int8 = 1,
ced_var_type_uint8 = 2,
ced_var_type_int16 = 3,
ced_var_type_uint16 = 4,
ced_var_type_int32 = 5,
ced_var_type_uint32 = 6,
ced_var_type_int64 = 7,
ced_var_type_uint64 = 8,
ced_var_type_bool = 9,
ced_var_type_float = 10,
ced_var_type_double = 11,
ced_var_type_string = 12,
ced_var_type_pointer = 13,
ced_var_type_array = 14,
ced_var_type_dict = 15,
} ced_var_type_t;

typedef struct ced_var_t {
ced_var_type_t type;
size_t size;

    union {
        int8_t _int8;
        uint8_t _uint8;
        int16_t _int16;
        uint16_t _uint16;
        int32_t _int32;
        uint32_t _uint32;
        int64_t _int64;
        uint64_t _uint64;
        int _bool;
        float _float;
        double _double;
        char* _string;
        void* _pointer;
        struct ced_var_t **_array;
    } data;
} ced_var_t, *ced_var_p;
{% endhighlight %}

## Type Enumeration
The `enum` defines constants for each supported type, allowing us to track the type of data that the variant currently
holds. By assigning each type a unique value, we can ensure that the variant correctly interprets the data in its union.

For example:
- `ced_var_type_int8` corresponds to an 8-bit signed integer.
- `ced_var_type_string` corresponds to a string pointer.

These constants will be key when handling conversions or operations that depend on the data type.

## Union for Data Storage
At the heart of the variant structure is a **union**. The union allows us to store multiple data types in the same
memory space, but only one at a time. By combining this union with the `type` field from the `enum`, we always know
which type the variant currently holds.

Here’s what the union includes:
- Integer types like `int8_t`, `int16_t`, and so on.
- Floating-point types like `float` and `double`.
- Complex types like `char*` for strings and `void*` for pointers.
- Arrays of variants (for holding lists or other complex data).

The union ensures that the variant is memory-efficient, as only one of these types will occupy the memory at any given
time.

## Memory and Size Tracking
The `size` field allows us to track the size of the data that the variant is holding. This is especially important for
types like strings or arrays, where the size of the content can vary.

For basic types like `int32_t`, the size is fixed and known in advance, but for strings or arrays, this field gives us
the ability to manage memory dynamically. As we handle more complex data types, this size tracking becomes crucial to
avoid memory leaks and ensure proper memory management.

# Usage

Now that we've defined the variant data type, let's look at how to create and manage these variants. This section will
walk through constructing and tearing down a variant to ensure proper memory management and usage.

## Construction
Creating a variant is straightforward. We provide helper functions that allow us to construct variants for different
types. These functions allocate memory for the variant and initialize it with the appropriate data.

For example, here's how you would create a variant that holds an 8-bit integer:

{% highlight c %}
ced_var_p my_int8_var = ced_var_new_int8(42);
{% endhighlight %}

This function creates a variant with the type `ced_var_type_int8`, sets its value to `42`, and returns a pointer to the
new variant. Similarly, we can construct variants for other types like strings, booleans, and floating points:

{% highlight c %}
ced_var_p my_string_var = ced_var_new_string("Hello, Variant!");
ced_var_p my_bool_var = ced_var_new_bool(1);
ced_var_p my_float_var = ced_var_new_float(3.14f);
{% endhighlight %}

Each of these functions ensures that the correct type is assigned and memory is allocated to store the value.

## Creating Arrays
You can also create more complex variants, such as arrays of variants. The `ced_var_new_array` function allows you to
pass an array of variants and the number of elements, constructing a variant that holds an array:

{% highlight c %}
ced_var_p array_items[3];
array_items[0] = ced_var_new_int32(10);
array_items[1] = ced_var_new_string("Array Element");
array_items[2] = ced_var_new_bool(0);

ced_var_p my_array_var = ced_var_new_array(array_items, 3);
{% endhighlight %}

In this example, the array variant will hold three different elements: an integer, a string, and a boolean.

## Tear Down

As with any dynamically allocated memory in C, it’s important to free the memory when you’re done using a variant.
Failing to do so will result in memory leaks. Each variant, whether it’s a basic type or an array, must be freed using
the `ced_var_free` function:

{% highlight c %}
ced_var_free(my_int8_var);
ced_var_free(my_string_var);
ced_var_free(my_bool_var);
ced_var_free(my_float_var);
{% endhighlight %}

When dealing with arrays or more complex structures like dictionaries, `ced_var_free` will recursively free all elements
within the array or dictionary, ensuring that all memory is properly cleaned up:

{% highlight c %}
ced_var_free(my_array_var);
{% endhighlight %}

In this case, the function will free each element within the array before freeing the array itself.

## Important Notes on Memory Management
- **Strings**: Strings are dynamically allocated when a variant is created, so make sure to free the variant holding the string when you're done with it.
- **Arrays**: Arrays of variants can grow large, and freeing them requires freeing each individual variant inside the array. The `ced_var_free` function handles this for you, but it's good practice to be aware of the potential overhead.

By ensuring that every variant is constructed properly and freed once it's no longer needed, you can manage dynamic
types safely and efficiently in your applications.

# Back to the Real World

Now that we've built our variant data type and explored how to construct and tear it down, let's bring it into a
real-world scenario. A variant data type is most useful when you need to handle dynamic types interchangeably without
knowing in advance what type of data you're working with. Let’s see how we can use variants in practical applications
and seamlessly interchange them with native C data types.

## Working with Native Data Types

One key feature of our variant type is that it allows us to work with various data types dynamically and convert
between them when needed. Let’s take a look at some common examples of interchanging variant types with native C data
types.

### Example 1: Converting Variants to Native Types

Suppose you have a variant containing an integer, and you want to use this integer in a C function that expects a
native `int32_t`. Using the `ced_var_as_int32` function, we can safely convert the variant to its corresponding native
type:

{% highlight c %}
ced_var_p my_variant = ced_var_new_int32(100);

int32_t native_int = ced_var_as_int32(my_variant)->data._int32;
printf("Native int value: %d\n", native_int);
{% endhighlight %}

In this case, the variant holds a 32-bit integer. We retrieve it using `ced_var_as_int32` and extract the native
integer value from the `data` field. Now, we can use it as we would any regular `int32_t`.

### Example 2: Converting Between Types

Sometimes, you might want to convert from one type to another. For example, you have a floating-point value stored in a
variant, and you need to convert it to an integer for use in some part of your application:

{% highlight c %}
ced_var_p my_float_variant = ced_var_new_float(3.14159f);

// Convert the variant to an int32
ced_var_p int_variant = ced_var_as_int32(my_float_variant);

// Extract the integer value
int32_t native_int = int_variant->data._int32;
printf("Converted int value: %d\n", native_int);
{% endhighlight %}

Here, the `ced_var_as_int32` function attempts to convert the float to an integer. This example illustrates how
variants make dynamic type handling seamless, allowing you to move between types without much friction.

### Example 3: Working with Complex Types

Beyond simple data types, our variant can handle more complex types like strings and arrays. Suppose we want to extract
a string from a variant and use it as a native C string:

{% highlight c %}
ced_var_p my_string_variant = ced_var_new_string("Hello, Variants!");

// Extract the string from the variant
const char* native_string = ced_var_as_string(my_string_variant)->data._string;
printf("Native string value: %s\n", native_string);
{% endhighlight %}

In this case, `ced_var_as_string` gives us the native C string pointer, which can then be passed around and used in the
same way as any other `char*` in C.

### Example 4: Handling Arrays

Finally, let’s demonstrate handling an array of mixed types. We can create a variant array, add different data types to
it, and extract the native values from each element:

{% highlight c %}
ced_var_p array_items[2];
array_items[0] = ced_var_new_int32(42);
array_items[1] = ced_var_new_string("Variant in an Array");

ced_var_p my_array_variant = ced_var_new_array(array_items, 2);

// Extract and print the integer from the first element
int32_t array_int = ced_var_as_int32(ced_var_array_get(my_array_variant, 0))->data._int32;
printf("Array int value: %d\n", array_int);

// Extract and print the string from the second element
const char* array_string = ced_var_as_string(ced_var_array_get(my_array_variant, 1))->data._string;
printf("Array string value: %s\n", array_string);
{% endhighlight %}

In this example, we see how a variant array can hold multiple types, and we extract and use each native value as needed.

# Conclusion

With our variant data type, we've created a powerful tool that allows us to work dynamically with multiple data types
in C, interchanging them seamlessly. Whether you're working with integers, floating points, strings, or even arrays,
the variant provides a flexible and type-safe way to manage data without requiring explicit type knowledge at
compile-time.

This flexibility can be especially useful in systems where data types are not known in advance, such as scripting
engines, serialization systems, or general-purpose data structures. By interchanging variants with native data types,
we unlock a wide range of possibilities for dynamic and flexible programming in C.

A full implementation of this variant data type [can be found](https://github.com/tuttlem/ced/blob/master/src/var/variant.h)
in my [ced](https://github.com/tuttlem/ced) library up on GitHub.
