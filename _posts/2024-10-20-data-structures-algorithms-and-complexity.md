---
layout: post
title: Data Structures, Algorithms, and Complexity
date: 2024-10-20
comments: false
categories: [ "python", "data structures", "algorithms" ]
---

# Introduction

In this post, we’ll walk through fundamental data structures and sorting algorithms, using Python to demonstrate key 
concepts and code implementations. We'll also discuss the algorithmic complexity of various operations like searching, 
inserting, and deleting, as well as the best, average, and worst-case complexities of popular sorting algorithms.

# Algorithmic Complexity

When working with data structures and algorithms, it's crucial to consider how efficiently they perform under different 
conditions. This is where **algorithmic complexity** comes into play. It helps us measure how the time or space an 
algorithm uses grows as the input size increases.

## Time Complexity

**Time complexity** refers to the amount of time an algorithm takes to complete, usually expressed as a function of the 
size of the input, $$ n $$. We typically use **Big-O notation** to describe the worst-case scenario. The goal is to 
approximate how the time increases as the input size grows.

### Common Time Complexities:
- $$ O(1) $$ **(Constant Time)**: The runtime does not depend on the size of the input. For example, accessing an element in an array by index takes the same amount of time regardless of the array’s size.
- $$ O(n) $$ **(Linear Time)**: The runtime grows proportionally with the size of the input. For example, searching for an element in an unsorted list takes $$ O(n) $$ time because, in the worst case, you have to check each element.
- $$ O(n^2) $$ **(Quadratic Time)**: The runtime grows quadratically with the input size. Sorting algorithms like **Bubble Sort** and **Selection Sort** exhibit $$ O(n^2) $$ time complexity because they involve nested loops.
- $$ O(\log n) $$ **(Logarithmic Time)**: The runtime grows logarithmically as the input size increases, often seen in algorithms that reduce the problem size with each step, like binary search.
- $$ O(n \log n) $$: This complexity appears in efficient sorting algorithms like **Merge Sort** and **Quick Sort**, combining the linear and logarithmic growth patterns.

## Space Complexity

**Space complexity** refers to the amount of memory an algorithm uses relative to the size of the input. This is also 
expressed in Big-O notation. For instance, sorting an array **in-place** (i.e., modifying the input array) requires 
$$ O(1) $$ auxiliary space, whereas **Merge Sort** requires $$ O(n) $$ additional space to store the temporary arrays 
created during the merge process.

## Why Algorithmic Complexity Matters

Understanding the time and space complexity of algorithms is crucial because it helps you:
- **Predict Performance**: You can estimate how an algorithm will perform on large inputs, avoiding slowdowns that may arise with inefficient algorithms.
- **Choose the Right Tool**: For example, you might choose a hash table (with $$ O(1) $$ lookup) over a binary search tree (with $$ O(\log n) $$ lookup) when you need fast access times.
- **Optimize Code**: Knowing the time complexity helps identify bottlenecks and guides you in writing more efficient code.

# Data Structures

## Lists

Python lists are dynamic arrays that support random access. They are versatile and frequently used due to their built-in 
functionality.

{% highlight python %}
# Python List Example
my_list = [1, 2, 3, 4]
my_list.append(5)    # O(1) - Insertion at the end
my_list.pop()        # O(1) - Deletion at the end
print(my_list[0])    # O(1) - Access
{% endhighlight %}

**Complexity**

* Access: $$ O(1) $$
* Search: $$ O(n) $$
* Insertion (at end): $$ O(1) $$
* Deletion (at end): $$ O(1) $$

## Arrays

Arrays are fixed-size collections that store elements of the same data type. While Python lists are dynamic, we can use 
the `array` module to simulate arrays.

{% highlight python %}
import array
my_array = array.array('i', [1, 2, 3])
my_array.append(4)
print(my_array)
{% endhighlight %}

**Complexity**

* Access: $$ O(1) $$
* Search: $$ O(n) $$
* Insertion/Deletion: $$ O(n) $$

## Stacks

A Stack is a Last-In-First-Out (LIFO) structure. Python lists can be used to simulate stack operations with `append()` 
and `pop()`.

{% highlight python %}
class Stack:
    def __init__(self):
        self.stack = []

    def push(self, value):
        self.stack.append(value)

    def pop(self):
        return self.stack.pop()

my_stack = Stack()
my_stack.push(1)
my_stack.push(2)
print(my_stack.pop())  # O(1)
{% endhighlight %}

**Complexity**

* Push/Pop: $$ O(1) $$
* Peek: $$ O(1) $$
* Search: $$ O(n) $$

## Queues

A Queue is a First-In-First-Out (FIFO) structure. Python's `collections.deque` is efficient for this purpose.

{% highlight python %}
from collections import deque
queue = deque()
queue.append(1)  # Enqueue
queue.append(2)
print(queue.popleft())  # Dequeue O(1)
{% endhighlight %}

**Complexity**

* Enqueue/Dequeue: $$ O(1) $$
* Search: $$ O(n) $$

## Sets

Sets are unordered collections with no duplicates. Python's `set` is implemented as a hash table.

{% highlight python %}
my_set = set()
my_set.add(1)  # O(1)
my_set.remove(1)  # O(1)
print(1 in my_set)  # O(1)
{% endhighlight %}

**Complexity**

* Add: $$ O(1) $$
* Remove: $$ O(1) $$
* Search: $$ O(1) $$

## Maps (Dictionaries)

Dictionaries store key-value pairs and are implemented as hash tables in Python.

{% highlight python %}
my_dict = {}
my_dict['key'] = 'value'  # O(1)
del my_dict['key']  # O(1)
print('key' in my_dict)  # O(1)
{% endhighlight %}

**Complexity**

* Insertion/Lookup/Deletion: $$ O(1) $$

## Trees

Trees are hierarchical data structures that allow efficient searching and sorting. A Binary Search Tree (BST) is one 
common example.

{% highlight python %}
class Node:
    def __init__(self, key):
        self.left = None
        self.right = None
        self.value = key

    def insert(root, key):
        if root is None:
            return Node(key)
        if key < root.value:
            root.left = insert(root.left, key)
        else:
            root.right = insert(root.right, key)
        return root

root = None
root = insert(root, 10)
root = insert(root, 20)
{% endhighlight %}

**Complexity**

* Insertion/Search/Deletion: $$ O(\log n) $$ for balanced trees, $$ O(n) $$ for unbalanced trees.

## Heaps

Heaps are specialized tree-based structures where the parent node is always greater (max-heap) or smaller (min-heap) 
than its children.

{% highlight python %}
import heapq
heap = []
heapq.heappush(heap, 10)  # O(log n)
heapq.heappush(heap, 5)
print(heapq.heappop(heap))  # O(log n)
{% endhighlight %}

**Complexity**

* Insertion/Deletion: $$ O(\log n) $$
* Peek: $$ O(1) $$

# Sorting Algorithms

Now we'll talk about some very common sorting algorithms and understand their complexity to better equip ourselves to 
make choices about what types of searches we need to do and when.

## Bubble Sort

Repeatedly swap adjacent elements if they are in the wrong order.

{% highlight python %}
def bubble_sort(arr):
    for i in range(len(arr)):
        for j in range(0, len(arr) - i - 1):
            if arr[j] > arr[j + 1]:
                arr[j], arr[j + 1] = arr[j + 1], arr[j]

arr = [5, 2, 9, 1, 5, 6]
bubble_sort(arr)
print(arr)
{% endhighlight %}

**Complexity**

* Best: $$ O(n) $$
* Average/Worst: $$ O(n^2) $$
* Space: $$ O(1) $$

## Selection Sort

Select the smallest element and swap it with the current element.

{% highlight python %}
def selection_sort(arr):
    for i in range(len(arr)):
        min_idx = i
        for j in range(i + 1, len(arr)):
            if arr[j] < arr[min_idx]:
                min_idx = j
        arr[i], arr[min_idx] = arr[min_idx], arr[i]

arr = [64, 25, 12, 22, 11]
selection_sort(arr)
print(arr)
{% endhighlight %}

**Complexity**

* Best/Average/Worst: $$ O(n^2) $$
* Space: $$ O(1) $$

## Insertion Sort

Insert each element into its correct position in the sorted portion of the array.

{% highlight python %}
def insertion_sort(arr):
    for i in range(1, len(arr)):
        key = arr[i]
        j = i - 1
        while j >= 0 and key < arr[j]:
            arr[j + 1] = arr[j]
            j -= 1
        arr[j + 1] = key

arr = [12, 11, 13, 5, 6]
insertion_sort(arr)
print(arr)
{% endhighlight %}

**Complexity**

* Best: $$ O(n) $$
* Average/Worst: $$ O(n^2) $$
* Space: $$ O(1) $$

## Merge Sort

Divide and conquer algorithm that splits the array and merges them back in sorted order.

{% highlight python %}
def merge_sort(arr):
    if len(arr) > 1:
        mid = len(arr) // 2
        left = arr[:mid]
        right = arr[mid:]

        merge_sort(left)
        merge_sort(right)

        i = j = k = 0
        while i < len(left) and j < len(right):
            if left[i] < right[j]:
                arr[k] = left[i]
                i += 1
            else:
                arr[k] = right[j]
                j += 1
            k += 1

        while i < len(left):
            arr[k] = left[i]
            i += 1
            k += 1

        while j < len(right):
            arr[k] = right[j]
            j += 1
            k += 1

arr = [12, 11, 13, 5, 6, 7]
merge_sort(arr)
print(arr)
{% endhighlight %}

**Complexity**

* Best/Average/Worst: $$ O(n \log n) $$
* Space: $$ O(n) $$

## Quick Sort

Picks a pivot and partitions the array around the pivot.

{% highlight python %}
def partition(arr, low, high):
    pivot = arr[high]
    i = low - 1
    for j in range(low, high):
        if arr[j] <= pivot:
            i += 1
            arr[i], arr[j] = arr[j], arr[i]
    arr[i + 1], arr[high] = arr[high], arr[i + 1]
    return i + 1

def quick_sort(arr, low, high):
    if low < high:
        pi = partition(arr, low, high)
        quick_sort(arr, low, pi - 1)
        quick_sort(arr, pi + 1, high)

arr = [10, 7, 8, 9, 1, 5]
quick_sort(arr, 0, len(arr) - 1)
print(arr)
{% endhighlight %}

**Complexity**

* Best/Average: $$ O(n \log n) $$
* Worst: $$ O(n^2) $$
* Space: $$ O(\log n) $$

## Heap Sort

Uses a heap data structure to find the maximum or minimum element.

{% highlight python %}
def heapify(arr, n, i):
    largest = i
    l = 2 * i + 1
    r = 2 * i + 2

    if l < n and arr[l] > arr[largest]:
        largest = l

    if r < n and arr[r] > arr[largest]:
        largest = r

    if largest != i:
        arr[i], arr[largest] = arr[largest], arr[i]
        heapify(arr, n, largest)

def heap_sort(arr):
    n = len(arr)
    for i in range(n // 2 - 1, -1, -1):
        heapify(arr, n, i)
    for i in range(n - 1, 0, -1):
        arr[i], arr[0] = arr[0], arr[i]
        heapify(arr, i, 0)

arr = [12, 11, 13, 5, 6, 7]
heap_sort(arr)
print(arr)
{% endhighlight %}

**Complexity**

* Best/Average/Worst: $$ O(n \log n) $$
* Space: $$ O(1) $$

## Bucket Sort

Distributes elements into buckets and sorts them individually.

{% highlight python %}
def bucket_sort(arr):
    bucket = []
    for i in range(len(arr)):
        bucket.append([])

    for j in arr:
        index_b = int(10 * j)
        bucket[index_b].append(j)

    for i in range(len(arr)):
        bucket[i] = sorted(bucket[i])

    k = 0
    for i in range(len(arr)):
        for j in range(len(bucket[i])):
            arr[k] = bucket[i][j]
            k += 1

arr = [0.897, 0.565, 0.656, 0.1234, 0.665]
bucket_sort(arr)
print(arr)
{% endhighlight %}

**Complexity**

* Best: $$ O(n+k) $$
* Average/Worst: $$ O(n^2) $$
* Space: $$ O(n+k) $$

## Radix Sort

Sorts numbers digit by digit starting from the least significant digit.

{% highlight python %}
def counting_sort(arr, exp):
    n = len(arr)
    output = [0] * n
    count = [0] * 10

    for i in range(n):
        index = arr[i] // exp
        count[index % 10] += 1

    for i in range(1, 10):
        count[i] += count[i - 1]

    i = n - 1
    while i >= 0:
        index = arr[i] // exp
        output[count[index % 10] - 1] = arr[i]
        count[index % 10] -= 1
        i -= 1

    for i in range(len(arr)):
        arr[i] = output[i]

def radix_sort(arr):
    max_val = max(arr)
    exp = 1
    while max_val // exp > 0:
        counting_sort(arr, exp)
        exp *= 10

arr = [170, 45, 75, 90, 802, 24, 2, 66]
radix_sort(arr)
print(arr)
{% endhighlight %}

**Complexity**

* Best/Average/Worst: $$ O(nk) $$
* Space: $$ O(n+k) $$

# Conclusion

We've explored a wide range of data structures and sorting algorithms, discussing their Python implementations, and 
breaking down their time and space complexities. These foundational concepts are essential for any software developer to 
understand, and mastering them will improve your ability to choose the right tools and algorithms for a given problem.

Below is a table outlining these complexities about the data structures:

| Data Structure | Access Time | Search Time | Insertion Time | Deletion Time | Space Complexity |
|----------------|-------------|-------------|----------------|---------------|------------------|
| List (Array)   | $$ O(1) $$  | $$ O(n) $$  | $$ O(n) $$     | $$ O(n) $$    | $$ O(n) $$       |
| Stack          | $$ O(n) $$  | $$ O(n) $$  | $$ O(1) $$     | $$ O(1) $$    | $$ O(n) $$       |
| Queue          | $$ O(n) $$  | $$ O(n) $$  | $$ O(1) $$     | $$ O(1) $$    | $$ O(n) $$       |
| Set            | N/A         | $$ O(1) $$  | $$ O(1) $$     | $$ O(1) $$    | $$ O(n) $$       |
| Dictionary     | N/A         | $$ O(1) $$  | $$ O(1) $$     | $$ O(1) $$    | $$ O(n) $$       |
| Binary Tree (BST) | $$ O(\log n) $$ | $$ O(\log n) $$ | $$ O(\log n) $$ | $$ O(\log n) $$ | $$ O(n) $$       |
| Heap (Binary)  | $$ O(n) $$  | $$ O(n) $$  | $$ O(\log n) $$| $$ O(\log n) $$| $$ O(n) $$      |


Below is a quick summary of the time complexities of the sorting algorithms we covered:

| Algorithm      | Best Time Complexity | Average Time Complexity | Worst Time Complexity | Auxiliary Space |
|----------------|----------------------|-------------------------|-----------------------|-----------------|
| Bubble Sort    | $$ O(n) $$           | $$ O(n^2) $$             | $$ O(n^2) $$           | $$ O(1) $$       |
| Selection Sort | $$ O(n^2) $$         | $$ O(n^2) $$             | $$ O(n^2) $$           | $$ O(1) $$       |
| Insertion Sort | $$ O(n) $$           | $$ O(n^2) $$             | $$ O(n^2) $$           | $$ O(1) $$       |
| Merge Sort     | $$ O(n \log n) $$    | $$ O(n \log n) $$        | $$ O(n \log n) $$      | $$ O(n) $$       |
| Quick Sort     | $$ O(n \log n) $$    | $$ O(n \log n) $$        | $$ O(n^2) $$           | $$ O(\log n) $$  |
| Heap Sort      | $$ O(n \log n) $$    | $$ O(n \log n) $$        | $$ O(n \log n) $$      | $$ O(1) $$       |
| Bucket Sort    | $$ O(n + k) $$       | $$ O(n + k) $$           | $$ O(n^2) $$           | $$ O(n + k) $$   |
| Radix Sort     | $$ O(nk) $$          | $$ O(nk) $$              | $$ O(nk) $$            | $$ O(n + k) $$   |

Keep this table handy as a reference for making decisions on the appropriate sorting algorithm based on time and space 
constraints.

Happy coding!