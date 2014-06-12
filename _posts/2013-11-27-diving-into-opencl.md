---
layout: post
title: Diving into OpenCL
date: 2013-11-27
comments: false
---

### Introduction

In a [previous article]({ post_url 2013-11-25-setup-debian-for-opencl-development }) I'd put together a walk through on how to get your development environment ready to write some [OpenCL](http://www.khronos.org/opencl/) code. This article by itself isn't of much use unless you can write some code already. Today's post will be a walk through on writing your first OpenCL program. This example, much like a lot of the other entry-level OpenCL development tutorials will focus on performing addition between two lists of floating point numbers.

### Lots to learn

Unfortunately, OpenCL is a topic that brings a very steep learning curve. In order to understand even the most simple of programs you need to read a fair bit of code and hopefully be aware of what it's doing. Before we dive into any implementation, I'll take you on a brief tour of terms, types and definitions that will help you to understanding the code as it's presented.

A `cl_platform_id` is obtained using [clGetPlatformIDs](http://www.khronos.org/registry/cl/sdk/1.0/docs/man/xhtml/clGetPlatformIDs.html). A platform in OpenCL refers to the host execution environment and any attached devices. Platforms are what allow OpenCL to share resources and execute programs.

A `cl_device_id` is obtained using [clGetDeviceIDs](http://www.khronos.org/registry/cl/sdk/1.0/docs/man/xhtml/clGetDeviceIDs.html). It's how your program will refer to the devices that your code will run on. A device is how OpenCL refers to "something" that will execute code (CPU, GPU, etc).

A `cl_context` is obtained using [clCreateContext](http://www.khronos.org/registry/cl/sdk/1.0/docs/man/xhtml/clCreateContext.html). A context is established across OpenCL devices. It's what OpenCL will use to manage command-queues, memory, program and kernel objects. It provides the ability to execute a kernel across many devices.

A `cl_program` is created from actual (string) source-code at runtime using [clCreateProgramWithSource](http://www.khronos.org/registry/cl/sdk/1.0/docs/man/xhtml/clCreateProgramWithSource.html). They're created in conjunction with your context so that program creation is aware of where it'll be expected to run. After the `cl_program` reference is successfully established, the host program would typically call [clBuildProgram](http://www.khronos.org/registry/cl/sdk/1.0/docs/man/xhtml/clBuildProgram.html) to take the program from its source code (string) state into an executable (binary) state.

A `cl_command_queue` is established using [clCreateCommandQueue](http://www.khronos.org/registry/cl/sdk/1.0/docs/man/xhtml/clCreateCommandQueue.html). A command queue is how work is scheduled to a device for execution. 

A `cl_kernel` is created using [clCreateKernel](http://www.khronos.org/registry/cl/sdk/1.0/docs/man/xhtml/clCreateKernel.html). A kernel is a function contained within a compiled `cl_program` object. It's identified within the source code with a `__kernel` qualifier. You set the argument list for a `cl_kernel` object using [clSetKernelArg](http://www.khronos.org/registry/cl/sdk/1.0/docs/man/xhtml/clSetKernelArg.html). To glue it all together, you use [clEnqueueNDRangeKernel](http://www.khronos.org/registry/cl/sdk/1.0/docs/man/xhtml/clEnqueueNDRangeKernel.html) or [clEnqueueTask](http://www.khronos.org/registry/cl/sdk/1.0/docs/man/xhtml/clEnqueueTask.html) to enqueue a task on the command queue to execute a kernel.

A side note here is that you can use [clEnqueueNativeKernel](http://www.khronos.org/registry/cl/sdk/1.0/docs/man/xhtml/clEnqueueNativeKernel.html) to execute native C/C++ code that isn't compiled by OpenCL.

At least if you can identify some form of meaning when you come across these type names, you won't be totally in the dark. Next up, we'll create a host program and OpenCL routine - compile, build and run!

### The Host

The host application is responsible for engaging with the OpenCL api to setup all of the objects described above. It's also responsible for locating the OpenCL source code and making it available for compilation at run time. In this first snippet of code, we use the OpenCL api to establish the management platform and devices that are available to execute our code. Majority of the OpenCL api standardises itself around returning error codes from all of the functions.

{% highlight c %}
cl_platform_id platform;
cl_device_id device;
int err;

/* find any available platform */
err = clGetPlatformIDs(1, &platform, NULL);

/* determine if we failed */
if (err < 0) {
  perror("Unable to identify a platform");
  exit(1);
}

/* try to acquire the GPU device */
err = clGetDeviceIDs(platform, CL_DEVICE_TYPE_GPU, 1, &device, NULL);

/* if no GPU was found, fail-back to the CPU */
if (err == CL_DEVICE_NOT_FOUND) {
  err = clGetDeviceIDs(platform, CL_DEVICE_TYPE_CPU, 1, &device, NULL);
}

/* check that we did acquire a device */
if (err < 0) {
  perror("Unable to acquire any devices");
  exit(1);
}
{% endhighlight %}

At this point, we have "platform" which will (<em>hopefully</em>) contain a platform ID identifying our management platform and "device" should either refer to the GPU or CPU (failing to find a GPU). The next step is to create a context and your OpenCL program from source.

{% highlight c %}
/* we'll get to this soon. for the time being, imagine that your
   OpenCL source code is located by the following variable */
char *program_source = ". . .";
size_t program_size = strlen(program_source);

cl_context context;
cl_program program;

/* try to establish a context with the device(s) that we've found */
context = clCreateContext(NULL, 1, &device, NULL, NULL, &err);

/* check that context creation was successful */
if (err < 0) {
  perror("Unable to create a context");
  exit(1);
}

/* using the context, create the OpenCL program from source */
program = clCreateProgramWithSource(context, 1, (const char **)&program_source, &program_size, &err);

/* chech that we could create the program */
if (err < 0) {
  perror("Unable to create the program");
  exit(1);
}
{% endhighlight %}

We've done just that here, but the program isn't quite yet ready for execution. Before we can start using this, we need to build the program. The build process is very much a compilation & linking process that involves its own set of log message outputs, etc. You can make this part of your program as elaborate as you'd like. Here's an example compilation process.

{% highlight c %}

size_t log_size;
char *program_log;

/* build the program */
err = clBuildProgram(program, 0, NULL, NULL, NULL, NULL);

/* determine if the build failed */
if (err < 0) {

  /* the build has failed here, so we'll now ask OpenCL for the build
     information, so that we can display the errors back to the user */
     
  /* pull back the size of the log */
  clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG, 0, NULL, &log_size);

  /* allocate a buffer and draw the log into a string */
  program_log = (char *)malloc(log_size + 1);
  program_log[log_size] = '\0';
  
  /* pull back the actual log text */
  clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG, log_size + 1, program_log, NULL);
  
  /* send the log out to the console */
  printf("%s\n", program_log);
  
  free(program_log);
  exit(1);
}
{% endhighlight %}

We've got a platform, device, context and program that's been built. We now need to shift contexts from the host program to the actual OpenCL code that we'll execute for the purposes of this example. We need to understand what the inputs, outputs, used resources, etc. of the OpenCL code is before we can continue to write the rest of our host. 

### The OpenCL Code

The main purpose of OpenCL code is really to operate arithmetically on arrays (or strings) of data. The example that I'm suggesting for the purposes of this article takes in two source arrays and produces another array which are the sum of each index. i.e.

{% highlight text %}
c[0] = a[0] + b[0]
c[1] = a[1] + b[1]
. . .
. . .
{% endhighlight %}

As above, the source arrays are `a` and `b`. The result array (holding the sum of each source array at each index) is `c`. Here's the (rather simple) OpenCL code to achieve this.

{% highlight c %}
__kernel void add_numbers(__global const int* a, 
                          __global const int* b, 
                          __global int* c) {
                          
  // get the array index we're going to work on
  size_t idx = get_global_id(0);
  
  // sum the values
  c[idx] = a[idx] + b[idx];
  
}
{% endhighlight %}

That's all there is to it. Things to note are, any function that is to be called in the OpenCL context is called a "kernel". Kernel's must be decorated with the `__kernel` modifier. In this example, the parameters that are passed in are decorated with the `__global` modifier. This tells OpenCL that these are objects allocated from the global memory pool. You can read up more about these modifiers [here](http://www.khronos.org/registry/cl/sdk/1.0/docs/man/xhtml/global.html). The final thing to note is the use of [get_global_id](http://www.khronos.org/registry/cl/sdk/1.0/docs/man/xhtml/get_global_id.html). It's what gives us the particular index to process in our array here. The parameter that's supplies allows you to work with 1, 2 or 3 dimensional arrays. Anything over this, the arrays need to be broken down to use a smaller dimension count.

### Back to the Host

Back in context of the host, we'll create the command queue and kernel objects. The command queue allows us to send commands to OpenCL like reading & writing to buffers or executing kernel code. The following code shows the creation of the command queue and kernel.

{% highlight c %}

cl_command_queue queue;
cl_kernel kernel;

/* create the command queue */
queue = clCreateCommandQueue(context, device, 0, &err);

/* check that we create the queue */
if (err < 0) {
  perror("Unable to create a command queue");
  exit(1);
}

/* create the kernel */
kernel = clCreateKernel(program, "add_numbers", &err);

/* check that we created the kernel */
if (err < 0) {
  perror("Unable to create kernel");
  exit(1);
}
{% endhighlight %}

Notice that we mentioned the kernel by name here. A kernel object refers to the function! Now that we have a function to execute (or kernel) we now need to be able to pass data to the function. We also need to be able to read the result once processing has finished. Our first job is allocating buffers that OpenCL will be aware of to handle these arrays.

{% highlight c %}
/* here are our source data arrays. "ARRAY_SIZE" is defined elsewhere to
   give these arrays common bounds */
int a_data[ARRAY_SIZE], b_data[ARRAY_SIZE], c_data[ARRAY_SIZE];

/* these are the handles that OpenCL will refer to these memory chunks */
cl_mem a_buffer, b_buffer, c_buffer;

/* initialize a_data & b_data to integers that we want to add */
for (i = 0; i < ARRAY_SIZE; i ++) {
  a_data[i] = random() % 1000;
  b_data[i] = random() % 1000;
}

/* create the memory buffer objects */
a_buffer = clCreateBuffer(context, CL_MEM_READ_ONLY, ARRAY_SIZE * sizeof(int), NULL, &err);
b_buffer = clCreateBuffer(context, CL_MEM_READ_ONLY, ARRAY_SIZE * sizeof(int), NULL, &err);
c_buffer = clCreateBuffer(context, CL_MEM_READ_WRITE, ARRAY_SIZE * sizeof(int), NULL, &err);
{% endhighlight %}

In the above snippet, we've defined the source arrays and we've also created buffers that will hold the information (for use in our OpenCL code). Now all we need to do is to feed the source arrays into the buffers and supply all of the buffers as arguments to our kernel.

{% highlight c %}
/* fill the source array buffers */
err = clEnqueueWriteBuffer(queue, a_buffer, CL_TRUE, 0, 
  ARRAY_SIZE * sizeof(int), a_data, 0, NULL, NULL);
  
err = clEnqueueWriteBuffer(queue, b_buffer, CL_TRUE, 0,
  ARRAY_SIZE * sizeof(int), b_data, 0, NULL, NULL);
  
/* supply these arguments to the kernel */
err = clSetKernelArg(kernel, 0, sizeof(cl_mem), &a_buffer);
err = clSetKernelArg(kernel, 1, sizeof(cl_mem), &b_buffer);
err = clSetKernelArg(kernel, 2, sizeof(cl_mem), &c_buffer);
{% endhighlight %}

Now we invoke OpenCL to do the work. In doing this, we need to supply the invocation with a global size and local size. Global size is used to specify the total number of work items being processed. In our case, this is `ARRAY_SIZE`. Local size is used as the number of work items in each local group. Local size needs to be a divisor of global size. For simplicity, I've set these both to `ARRAY_SIZE`.

{% highlight c %}

int global_size = ARRAY_SIZE,
    local_size = ARRAY_SIZE;
    
/* send the work request to the queue */
err = clEnqueueNDRangeKernel(queue, kernel, 1, NULL, 
  &global_size, &local_size, 0, NULL, NULL);
 
/* check that we could queue the work */ 
if (err < 0) {
  perror("Unable to queue work");
  exit(1);
}

/* wait for the work to complete */
clFinish(queue);
{% endhighlight %}

After all of the work is completed, we really want to take a look at the result. We'll send another request to the command queue to read that result array back into local storage. From there, we'll be able to print the results to screen.

{% highlight c %}

/* read out the result array */
err = clEnqueueReadBuffer(queue, c_buffer, CL_TRUE, 0
  ARRAY_SIZE * sizeof(int), c_data, 0, NULL, NULL);
  
/* check that the read was ok */
if (err < 0) {
  perror("Unable to read result array");
  exit(1);
}

/* print out the result */
for (i = 0; i < ARRAY_SIZE; i ++) {
  printf("%d + %d = %d\n", a_data[i], b_data[i], c_data[i]);
}
{% endhighlight %}

Fantastic. Everything's on screen now, we can see the results. All we'll do from here is clean up our mess and get out.

{% highlight c %}
clReleaseKernel(kernel);
clReleaseMemObject(a_buffer);
clReleaseMemObject(b_buffer);
clReleaseMemObject(c_buffer);
clReleaseCommandQueue(queue);
clReleaseProgram(program);
clReleaseContext(context);
{% endhighlight %}

What a marathon! Hopefully you've learnt something from this post. It's a lot to take in to get a "Hello, World" level application up and running.