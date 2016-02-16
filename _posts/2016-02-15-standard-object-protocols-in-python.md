---
layout: post
title: Standard object protocols in Python
date: 2016-02-15
comments: false
categories: [ "python", "object", "attribute" ]
---

To give your objects a more *baked-in* feel, you can use python's standard object protocol functions so that native operators start to operate on your object. 

By implementing the following items on your custom objects, infix operators start to work executing your custom code as per defined.

### General

By overriding `__bool__` in your objects, you can define how your object will respond in conditional scenarios. `__bool__` effectively allows you to use your object as a condition in an `if` or `while` statement.

{% highlight python %}
class Boolable:
  def __init__(self, n):
    self.num = n

  def __bool__(self):
    return self.num % 2 == 0    
{% endhighlight %}

The method `__call__` will allow your object to openly accept function calls:

{% highlight python %}
class Callable:
  def __call__(self, *args, *kwargs):
    # implementation here

c = Callable()
c()
{% endhighlight %}

### Array

The following overrides allow you to make your objects appear like containers (arrays, etc.):

| Operation      | Method          | Description                                        |
|----------------|-----------------|----------------------------------------------------|
| Length         | `__len__`       | Allows the `len` function to operate on the object | 
| Iterator       | `__iter__`      | Gets an object to start the iteration process      |
| Next           | `__next__`      | Gets an object to continue the iteration process   |
| Reverse        | `__reversed__`  | Reverses the internal sequence                     |

### Dictionary

The following overrides allow you to make your object respond like a `dict`:

| Operation      | Method          | Description                                        |
|----------------|-----------------|----------------------------------------------------|
| Set item       | `__setitem__`   | Sets an item in the dictionary                     |
| Get item       | `__getitem__`   | Retrieves an item from the dictionary              |
| Delete item    | `__delitem__`   | Removes an item from the dictionary                |

### Mathematic

The following table lists out all of the methods that you can override on a class that will give you access to arithmetic operators.

| Operation            | Operator | LHS             | RHS               | Inline            |
|----------------------|----------|-----------------|-------------------|-------------------|
| Addition             | `+`      | `__add__`       | `__radd__`        | `__iadd__`        |
| Subtraction          | `-`      | `__sub__`       | `__rsub__`        | `__isub__`        |
| Multiplication       | `*`      | `__mul__`       | `__rmul__`        | `__imul__`        |
| True Division        | `/`      | `__truediv__`   | `__rtruediv__`    | `__itruediv__`    |
| Floor Division       | `//`     | `__floordiv__`  | `__rfloordiv__`   | `__ifloordiv__`   |
| Modulo               | `%`      | `__mod__`       | `__rmod__`        | `__imod__`        |
| Division and Modulo  | `divmod` | `__divmod__`    | `__rdivmod__`     |                   |
| Exponentiation       | `**`     | `__pow__`       | `__rpow__`        | `__ipow__`        |
| Shift left           | `<<`     | `__lshift__`    | `__rlshift__`     | `__ilshift__`     |
| Shift right          | `>>`     | `__rshift__`    | `__rrshift__`     | `__irshift__`     |
| Bitwise AND          | `&`      | `__and__`       | `__rand__`        | `__iand__`        |
| Bitwise OR           | `|`      | `__or__`        | `__ror__`         | `__ior__`         |
| Bitwise XOR          | `^`      | `__xor__`       | `__rxor__`        | `__ixor__`        |
| Bitwise NOT          | `~`      | `__invert__`    |                   |                   |

| Function | Method       |
|----------|--------------|
| Floor    | `__floor__`  |
| Ceil     | `__ceil__`   |
| Round    | `__round__`  |
| Negate   | `__neg__`    |
| Positive | `__pos__`    |
| Absolute | `__abs__`    |

### Comparison 

The following table lists all of the comparison operators

| Operation                | Operator | Method            |
|--------------------------|----------|-------------------|
| Equals                   | `==`     | `__eq__`          |
| Not Equal                | `!=`     | `__ne__`          |
| Greater than, equal to   | `>=`     | `__gte__`         |
| Greater than             | `>`      | `__gt__`          |
| Lesser than, equal to    | `<=`     | `__lte__`         |
| Lesser than              | `<`      | `__lt__`          |

### Type conversions

| Type      | Method       | Description            |
|-----------|--------------|------------------------|
| int       | `__int__`    |                        |
| float     | `__float__`  |                        |
| complex   | `__complex__`|                        |
| index     | `__index__`  | Allows python to use your object as an array index |

### Context

The following override allow your objects to measure contexts:

| Operation      | Method          | Description                                        |
|----------------|-----------------|----------------------------------------------------|
| Enter          | `__enter__`     | Measures when a context enters                     |
| Exit           | `__exit__`      | Measures when a context exits                      |

These functions are useful when your object is supplied to a `with` statement.

{% highlight python %}
class ContextMeasurement:
  def __enter__(self):
    print("Entering context")
      
  def __exit__(self, exc_class, exc_instance, traceback):
    print("Exiting context")
        
with ContextMeasurement():
  print("Inside the context right now")
{% endhighlight %}

