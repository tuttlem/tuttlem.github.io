---
layout: post
title: High Performance Linux IO with IO_URING
date: 2024-12-23
comments: false
categories: [ "" ]
---

# Introduction

IO_URING is an advanced asynchronous I/O interface introduced in the Linux kernel (version 5.1). It's designed to 
provide significant performance improvements for I/O-bound applications, particularly those requiring high throughput 
and low latency.

It's well worth taking a look in the linux man pages for [io_uring](https://man7.org/linux/man-pages/man7/io_uring.7.html) 
and having a read through the function interface.

In today's article we'll discuss IO_URING in depth and follow with some examples to see it in practice.

# What is IO_URING

**IO_URING** is a high-performance asynchronous I/O interface introduced in Linux kernel version 5.1. It was developed 
to address the limitations of traditional Linux I/O mechanisms like `epoll`, `select`, and `aio`. These earlier 
approaches often suffered from high overhead due to system calls, context switches, or inefficient batching, which 
limited their scalability in handling modern high-throughput and low-latency workloads.

At its core, IO_URING provides a ring-buffer-based mechanism for submitting I/O requests and receiving their 
completions, eliminating many inefficiencies in older methods. This allows applications to perform non-blocking, 
asynchronous I/O with minimal kernel involvement, making it particularly suited for applications such as databases, web 
servers, and file systems.

# How does IO_URING work?

IO_URING's architecture revolves around two primary shared memory ring buffers between user space and the kernel:

1. **Submission Queue (SQ):**
    - The SQ is a ring buffer where applications enqueue I/O requests.
    - User-space applications write requests directly to the buffer without needing to call into the kernel for each operation.
    - The requests describe the type of I/O operation to be performed (e.g., read, write, send, receive).
2. **Completion Queue (CQ):**
    - The CQ is another ring buffer where the kernel places the results of completed I/O operations.
    - Applications read from the CQ to retrieve the status of their submitted requests.

The interaction between user space and the kernel is simplified:

- The user-space application adds entries to the **Submission Queue** and notifies the kernel when ready (via a single syscall like `io_uring_enter`).
- The kernel processes these requests and posts results to the **Completion Queue**, which the application can read without additional syscalls.

# Key Features

1. **Batching Requests:**
    - Multiple I/O operations can be submitted in a single system call, significantly reducing syscall overhead.
2. **Zero-copy I/O:**
    - Certain operations (like reads and writes) can leverage fixed buffers, avoiding unnecessary data copying between kernel and user space.
3. **Kernel Offloading:**
    - The kernel can process requests in the background, allowing the application to continue without waiting.
4. **Efficient Polling:**
    - Supports event-driven programming with low-latency polling mechanisms, reducing idle time in high-performance applications.
5. **Flexibility:**
    - IO_URING supports a wide range of I/O operations, including file I/O, network I/O, and event notifications.

# Code

Let's get some code examples going to see exactly what we're dealing with. 

First of all, check to see that your kernel supports IO_URING. It should. It's been available since 51.

{% highlight bash %}
uname -r
{% endhighlight %}

You'll also need `liburing` avaliable to you in order to compile these examples.

## Library setup

In this first example, we won't perform any actions; but we'll setup the library so that we can use these operations. 
All of our other examples will use this as a base.

We'll need some basic I/O headers as well as `liburing.h`.

{% highlight c %}
#include <liburing.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
{% endhighlight %}

We initialize our uring queue using `io_uring_queue_init`:

{% highlight c %}
struct io_uring ring;
int ret;

// initialize IO_URING
if (io_uring_queue_init(8, &ring, 0) < 0) {
    perror("io_uring_queue_init");
    exit(1);
}
{% endhighlight %}

When we're finished with the ring, we cleanup with `io_uring_queue_exit`.

{% highlight c %}
io_uring_queue_exit(&ring);
{% endhighlight %}

## Simple Write

In this example, we'll queue up a write of a string out to a file and that's it.

First, we need to open the file like usual:

{% highlight c %}
int fd = open(FILENAME, O_WRONLY | O_CREAT | O_TRUNC, 0644);
if (fd < 0) {
    perror("open");
    io_uring_queue_exit(&ring);
    exit(1);
}
{% endhighlight %}

Now, we setup the write job to happen.

{% highlight c %}
struct io_uring_sqe *sqe = io_uring_get_sqe(&ring);
if (!sqe) {
    fprintf(stderr, "io_uring_get_sqe failed\n");
    close(fd);
    io_uring_queue_exit(&ring);
    exit(1);
}

const char *message = MESSAGE;
struct iovec iov = {
    .iov_base = (void *)message,
    .iov_len = strlen(message)
};

io_uring_prep_writev(sqe, fd, &iov, 1, 0);
{% endhighlight %}

The `io_uring_get_sqe` function will get us the next available submission queue entry from the job queue. Once we have 
secured one of these, we then fill a vector I/O structure (a `iovec`) with the details of our data. Here it's just the 
data pointer, and length.

Finally, we prepare a vector write request using `io_uring_prep_writev`.

We submit the job off to be processed now with `io_uring_submit`:

{% highlight c %}
ret = io_uring_submit(&ring);
if (ret < 0) {
    perror("io_uring_submit");
    close(fd);
    io_uring_queue_exit(&ring);
    exit(1);
}
{% endhighlight %}

We can wait for the execution to complete; even more powerful though is we can be off doing other things if we'd like!

In order to wait for the job to finish, we use `io_uring_wait_cqe`:

{% highlight c %}
struct io_uring_cqe *cqe;
ret = io_uring_wait_cqe(&ring, &cqe);
if (ret < 0) {
    perror("io_uring_wait_cqe");
    close(fd);
    io_uring_queue_exit(&ring);
    exit(1);
}
{% endhighlight %}

We check the result of the job through the `io_uring_cqe` structure filled by the `io_uring_wait_cqe` call:

{% highlight c %}
if (cqe->res < 0) {
    fprintf(stderr, "Write failed: %s\n", strerror(-cqe->res));
} else {
    printf("Write completed successfully!\n");
}
{% endhighlight %}

Finally, we mark the uring event as consumed and close the file.

{% highlight c %}
io_uring_cqe_seen(&ring, cqe);
close(fd);
{% endhighlight %}

The full example of this can be found [here](https://gist.github.com/tuttlem/902e24fac5f4e5c35880b403416aa7e4).

## Multiple Operations

We can start to see some of the power of this system in this next example. We'll submit multiple jobs for processing.

We've opened a source file for reading int `src_fd` and a destination file for writing in `dest_fd`.

{% highlight c %}
// prepare a read operation
sqe = io_uring_get_sqe(&ring);
io_uring_prep_read(sqe, src_fd, buffer, BUF_SIZE, 0);

// submit the read request
io_uring_submit(&ring);
io_uring_wait_cqe(&ring, &cqe);

if (cqe->res < 0) {
    fprintf(stderr, "Read failed: %s\n", strerror(-cqe->res));
    io_uring_cqe_seen(&ring, cqe);
    goto cleanup;
}
io_uring_cqe_seen(&ring, cqe);

// prepare a write operation
sqe = io_uring_get_sqe(&ring);
io_uring_prep_write(sqe, dest_fd, buffer, cqe->res, 0);

// submit the write request
io_uring_submit(&ring);
io_uring_wait_cqe(&ring, &cqe);

if (cqe->res < 0) {
    fprintf(stderr, "Write failed: %s\n", strerror(-cqe->res));
} else {
    printf("Copy completed successfully!\n");
}
io_uring_cqe_seen(&ring, cqe);
{% endhighlight %}

So, this is just sequentially executing multiple operations.

The full example of this can be found [here](https://gist.github.com/tuttlem/8b918743e2432ace917ef8cd2b7178de).

## Asynchronous operations

Finally, we'll write an example that will process multiple operations in parallel.

The following for loop sets up 3 read jobs:

{% highlight c %}
for (int i = 0; i < FILE_COUNT; i++) {
    int fd = open(files[i], O_RDONLY);
    if (fd < 0) {
        perror("open");
        io_uring_queue_exit(&ring);
        exit(1);
    }

    // Allocate a buffer for the read operation
    char *buffer = malloc(BUF_SIZE);
    if (!buffer) {
        perror("malloc");
        close(fd);
        io_uring_queue_exit(&ring);
        exit(1);
    }

    requests[i].fd = fd;
    requests[i].buffer = buffer;

    // Get an SQE (Submission Queue Entry)
    struct io_uring_sqe *sqe = io_uring_get_sqe(&ring);
    if (!sqe) {
        fprintf(stderr, "Failed to get SQE\n");
        close(fd);
        free(buffer);
        io_uring_queue_exit(&ring);
        exit(1);
    }

    // Prepare a read operation
    io_uring_prep_read(sqe, fd, buffer, BUF_SIZE, 0);
    io_uring_sqe_set_data(sqe, &requests[i]);
}
{% endhighlight %}

All of the requests now get submitted for processing:

{% highlight c %}
// Submit all requests
ret = io_uring_submit(&ring);
if (ret < 0) {
    perror("io_uring_submit");
    io_uring_queue_exit(&ring);
    exit(1);
}
{% endhighlight %}

Finally, we wait on each of the jobs to finish. The important thing to note here, is that we could be busy off doing 
otherthings rather than just waiting for these jobs to finish.

{% highlight c %}
// wait for completions
for (int i = 0; i < FILE_COUNT; i++) {
    struct io_uring_cqe *cqe;
    ret = io_uring_wait_cqe(&ring, &cqe);
    if (ret < 0) {
        perror("io_uring_wait_cqe");
        io_uring_queue_exit(&ring);
        exit(1);
    }

    // Process the completed request
    struct io_request *req = io_uring_cqe_get_data(cqe);
    if (cqe->res < 0) {
        fprintf(stderr, "Read failed for file %d: %s\n", req->fd, strerror(-cqe->res));
    } else {
        printf("Read %d bytes from file descriptor %d:\n%s\n", cqe->res, req->fd, req->buffer);
    }

    // Mark the CQE as seen
    io_uring_cqe_seen(&ring, cqe);

    // Clean up
    close(req->fd);
    free(req->buffer);
}
{% endhighlight %}

The entire example of this one can be found [here](https://gist.github.com/tuttlem/71cf8bacb52a2c69d0d7efa3167fd860).

# Conclusion

**IO_URING** represents a transformative step in Linux asynchronous I/O, providing unparalleled performance and flexibility 
for modern applications. By minimizing syscall overhead, enabling zero-copy I/O, and allowing concurrent and batched 
operations, it has become a vital tool for developers working on high-performance systems.

Through the examples we've covered, you can see the practical power of IO_URING, from simple write operations to complex 
asynchronous processing. Its design not only simplifies high-throughput I/O operations but also opens up opportunities 
to optimize and innovate in areas like database systems, networking, and file handling.
