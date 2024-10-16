---
layout: post
title: Unit testing your C code
date: 2024-10-16
comments: false
categories: [ "c", "unit testing" ]
---

# Introduction

Writing reliable and maintainable code is a fundamental part of software
development, and unit testing is one of the most effective ways to ensure your
code works as expected. Unit tests help catch bugs early, ensure that changes to
the codebase don't introduce new issues, and serve as a form of documentation
for your code's expected behavior.

In this article, we'll explore how to set up and use Google Test (also known as
googletest), a popular C++ testing framework, to test your C and C++ code. We'll
walk through the installation process, demonstrate basic assertions, and then
dive into testing a more complex feature—the variant data type.

---

# Installation and Setup

Google Test makes it easy to write and run unit tests. It integrates well with
build systems like CMake, making it straightforward to include in your project.
Let's go step-by-step through the process of installing and setting up Google Test
in a CMake-based project.

## Step 1: Add Google Test to Your Project

First, we need to download and include Google Test in the project. One of the easiest
ways to do this is by adding Google Test as a subdirectory in your project’s
source code. You can download the source directly from the [Google Test GitHub repository](https://github.com/google/googletest).

Once you have the source, place it in a `lib/` directory within your project.
Your directory structure should look something like this:

{% highlight text %}
my_project/
├── lib/
│   └── googletest-1.14.0/
├── src/
│   └── [source files...]
├── CMakeLists.txt
{% endhighlight %}


## Step 2: Modify the CMake File

Now that you have Google Test in your project, let's modify your `CMakeLists.txt`
file to integrate it. Below is an example of a CMake configuration that sets up
Google Test and links it to your test suite:

{% highlight text %}
project(tests)

# Add Google Test as a subdirectory
add_subdirectory(lib/googletest-1.14.0)

# Include directories for Google Test and your source files
include_directories(${gtest_SOURCE_DIR}/include ${gtest_SOURCE_DIR})
include_directories(../src)

# Add the executable that will run the tests
add_executable(Google_Tests_run
src/tests1.cpp
src/tests2.cpp
src/tests3.cpp)

# Link the Google Test library and any other necessary libraries
target_link_libraries(Google_Tests_run gtest gtest_main myproject)
{% endhighlight %}

This CMake setup includes Google Test in your project by adding it as a
subdirectory, and it links your test suite to the `gtest` and `gtest_main`
libraries. Now you’re ready to write and run unit tests!

## Step 3: Build and Run the Tests

To compile the tests, simply run the following commands from the root of your project directory:

{% highlight bash %}
mkdir build
cd build
cmake ..
make
{% endhighlight %}

Once the build is complete, you can run your tests with:

{% highlight bash %}
./Google_Tests_run
{% endhighlight %}

This command will execute all the tests defined in your test files. Now that the
environment is set up, let's move on to writing some unit tests using Google Test.

# Basic Assertions with Google Test

Before diving into testing our variant data type, let’s explore some of the basic
assertions provided by Google Test. Assertions are used to check that a
particular condition holds true during test execution. If the condition is false,
the test fails.

## Common Assertions

Here are some of the most commonly used assertions:

- **`EXPECT_EQ(val1, val2)`**: Checks that `val1` is equal to `val2`.
- **`EXPECT_NE(val1, val2)`**: Checks that `val1` is not equal to `val2`.
- **`EXPECT_TRUE(condition)`**: Checks that the condition is `true`.
- **`EXPECT_FALSE(condition)`**: Checks that the condition is `false`.
- **`ASSERT_EQ(val1, val2)`**: Like `EXPECT_EQ`, but if the assertion fails, it aborts the current function.

Let's look at a simple example that tests basic operations:

{% highlight c %}
#include "gtest/gtest.h"

TEST(SimpleTests, BasicAssertions) {
// Expect equality
EXPECT_EQ(1 + 1, 2);

    // Expect inequality
    EXPECT_NE(1 + 1, 3);
    
    // Expect true condition
    EXPECT_TRUE(4 > 2);
    
    // Expect false condition
    EXPECT_FALSE(2 > 4);
}
{% endhighlight %}

When you run this test, Google Test will evaluate each assertion and output the
result. If any assertion fails, it will print a detailed message showing the
expected and actual values.

Now that you’ve seen how to use basic assertions, let's move on to testing a more
complex feature: the variant data type.

## Testing the Variant Data Type

In a [previous post]({% post_url 2024-10-16-make-your-own-variant-data-type %}) we explored creating our own variant
data type. This piece of library code should provide us with some good examples on how to apply unit tests.

With the variant being able to hold multiple types (integers, floats,
strings, etc.), we need to test that each type is correctly handled by the variant
and behaves as expected.

Here’s an example test that checks if the `ced_var_new_int8` function correctly
creates an 8-bit integer variant:

{% highlight c %}
#include "gtest/gtest.h"
#include "ced.h"

namespace {
TEST(VariantTests, ConstructInt8) {
ced_var_p var = ced_var_new_int8(1);

        EXPECT_EQ(var->__info.type, reflect_type_variant);
        EXPECT_EQ(var->type, ced_var_type_int8);
        EXPECT_EQ(var->data._int8, 1);
    
        ced_var_free(var);
    }
}
{% endhighlight %}

This test ensures that:
1. The variant type is correctly set to `ced_var_type_int8`.
2. The integer value stored in the variant is `1`.

You can follow this pattern to test other data types supported by the variant,
ensuring that each type is correctly initialized and behaves as expected.

In the next section, we'll walk through more examples of testing different variant
types and introduce more complex tests for arrays and type conversions.

# More Tests!

Now that we've covered the basics of using Google Test, let’s look at some examples of how to apply these concepts to
test our variant data type. We won’t go through every single test, but we’ll highlight a few that demonstrate different
key behaviors—constructing basic types, handling arrays, and type conversion.

## Constructing Basic Types

One of the simplest tests you can write is to verify that a variant can be properly constructed with a specific data
type. This ensures that the `ced_var_new_*` functions correctly initialize the variant.

For example, here’s a test that checks if we can create an 8-bit integer variant:

{% highlight c %}
TEST(VariantTests, ConstructInt8) {
ced_var_p var = ced_var_new_int8(1);

    EXPECT_EQ(var->__info.type, reflect_type_variant);  // Check the variant type
    EXPECT_EQ(var->type, ced_var_type_int8);            // Ensure it's an int8 variant
    EXPECT_EQ(var->data._int8, 1);                      // Check the stored value

    ced_var_free(var);  // Don't forget to free the variant!
}
{% endhighlight %}

This test checks the following:
1. The variant’s type is correctly set to `ced_var_type_int8`.
2. The data inside the variant is the expected integer value.
3. The variant is freed properly at the end to avoid memory leaks.

## Handling Arrays of Variants

Another important feature of the variant data type is its ability to hold arrays of other variants. Testing this
involves creating an array, verifying its size, and ensuring each element in the array holds the correct value.

Here’s an example that constructs an array of variants and tests its contents:

{% highlight c %}
TEST(VariantTests, ConstructArray) {
    ced_var_p arr[] = {
        ced_var_new_int8(10),
        ced_var_new_int16(500),
        ced_var_new_int32(100000),
        ced_var_new_int64(10000000000),
        ced_var_new_string("Howdy!")
    };

    ced_var_p var = ced_var_new_array(arr, 5);

    EXPECT_EQ(var->type, ced_var_type_array);  // Check it's an array
    EXPECT_EQ(var->size, 5);                   // Check the size of the array

    // Clean up memory for both the array and its contents
    for (int i = 0; i < 5; ++i) {
        ced_var_free(arr[i]);
    }
    ced_var_free(var);
}
{% endhighlight %}

In this test, we:
1. Create an array of variants, each holding different types (integers and a string).
2. Verify that the variant we created is indeed an array.
3. Check that the size of the array is correct.
4. Clean up the memory for each individual variant and the array as a whole.

## Type Conversion and Safety

Variants allow us to convert between different types, but not all conversions are valid. We should ensure that the type
conversion logic works correctly and fails gracefully when an invalid conversion is attempted.

Let’s look at a test that checks a valid conversion, and another that ensures a failed conversion returns `NULL`:

### Successful Type Conversion
{% highlight c %}
TEST(VariantTests, AsType) {
ced_var_p var = ced_var_new_int8(1);
ced_var_p new_var = ced_var_as_type(var, ced_var_type_int16);

    EXPECT_EQ(new_var->type, ced_var_type_int16);  // Ensure it's now int16
    EXPECT_EQ(new_var->data._int16, 1);            // Check the value after conversion

    ced_var_free(var);
    ced_var_free(new_var);
}
{% endhighlight %}

This test checks that:
1. The original 8-bit integer is correctly converted into a 16-bit integer.
2. The value remains unchanged after conversion.

### Failed Type Conversion
{% highlight c %}
TEST(VariantTests, AsTypeFail) {
ced_var_p var = ced_var_new_int64(1);
ced_var_p new_var = ced_var_as_type(var, ced_var_type_int8);

    EXPECT_EQ(new_var == NULL, true);  // Check that the conversion failed

    ced_var_free(var);
}
{% endhighlight %}

In this test:
1. We attempt to convert a 64-bit integer into an 8-bit integer, which is not possible.
2. The conversion returns `NULL`, indicating the failure, and we verify this with `EXPECT_EQ`.

These are just a few examples of the types of unit tests you can write for your variant data type. By covering basic
type construction, handling arrays, and ensuring type conversion behaves as expected, we’ve demonstrated how to use
Google Test to validate the functionality of complex C code.

# Conclusion

Unit testing is a critical part of ensuring the reliability and correctness of your code. By integrating Google Test
into your C/C++ projects, you can create a robust testing suite that not only catches bugs early but also provides
confidence in the stability of your codebase.

With the ability to handle various types, arrays, and even type conversions, our variant data type is a powerful tool,
and Google Test helps ensure it works exactly as intended. Whether you're dealing with basic types or more complex
features, writing clear, concise unit tests like the ones shown here will go a long way in maintaining high-quality code.

