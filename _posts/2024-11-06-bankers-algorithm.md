---
layout: post
title: Banker's Algorithm
date: 2024-11-06
comments: false
categories: [ "rust" ]
---

# Introduction

The **Banker’s Algorithm** is a classic algorithm used in operating systems to manage resource allocation and avoid 
deadlock, especially when dealing with multiple processes competing for limited resources. This problem provides an 
opportunity to work with data structures and logic that ensure safe, deadlock-free allocation.

In this implementation, we’ll use Rust to simulate the Banker’s Algorithm. Here’s what we’ll cover:

* **Introduction to the Banker’s Algorithm**: Understanding the problem and algorithm.
* **Setting Up the System State**: Define resources, allocation, maximum requirements, and available resources.
* **Implementing the Safety Check**: Ensure that allocations leave the system in a safe state.
* **Requesting and Releasing Resources**: Manage resources safely to prevent deadlock.

# Banker's Algorithm

The Banker’s Algorithm operates in a system where each process can request and release resources multiple times. The 
algorithm maintains a "safe state" by only granting resource requests if they don't lead to a deadlock. This is done by 
simulating allocations and checking if the system can still fulfill all processes' maximum demands without running out 
of resources.

Key components in the Banker’s Algorithm:

* **Available**: The total number of each type of resource available in the system.
* **Maximum**: The maximum demand of each process for each resource.
* **Allocation**: The amount of each resource currently allocated to each process.
* **Need**: The remaining resources each process needs to fulfill its maximum demand, calculated as `Need = Maximum - Allocation`.

A system is considered in a "safe state" if there exists an order in which all processes can finish without deadlock. 
The Banker’s Algorithm uses this condition to determine if a resource request can be granted.

# Implementation

We can now break this algorithm down and present it using rust.

## Setting Up the System State 

Let’s start by defining the structures to represent the system’s resources, maximum requirements, current allocation, 
and needs.

{% highlight rust %}
#[derive(Debug)]
struct System {
    available: Vec<i32>,
    maximum: Vec<Vec<i32>>,
    allocation: Vec<Vec<i32>>,
    need: Vec<Vec<i32>>,
}

impl System {
    fn new(available: Vec<i32>, maximum: Vec<Vec<i32>>, allocation: Vec<Vec<i32>>) -> Self {
        let need = maximum.iter()
            .zip(&allocation)
            .map(|(max, alloc)| max.iter().zip(alloc).map(|(m, a)| m - a).collect())
            .collect();
        
        System {
            available,
            maximum,
            allocation,
            need,
        }
    }
}
{% endhighlight %}

In this structure:

* `available` represents the system’s total available resources for each resource type.
* `maximum` is a matrix where each row represents a process, and each column represents the maximum number of each resource type the process might request.
* `allocation` is a matrix indicating the currently allocated resources to each process.
* `need` is derived from maximum - allocation and represents each process's remaining resource requirements.

### Need breakdown

Taking the following piece of code, we can do a pen-and-paper walkthrough:

{% highlight rust %}
let need = maximum.iter()
    .zip(&allocation)
    .map(|(max, alloc)| max.iter().zip(alloc).map(|(m, a)| m - a).collect())
    .collect();
{% endhighlight %}

Suppose:

* `maximum = [[7, 5, 3], [3, 2, 2], [9, 0, 2]]`
* `allocation = [[0, 1, 0], [2, 0, 0], [3, 0, 2]]`

Using the above code:

1. `maximum.iter().zip(&allocation)` will produce pairs:

   * `([7, 5, 3], [0, 1, 0])`
   * `([3, 2, 2], [2, 0, 0])`
   * `([9, 0, 2], [3, 0, 2])`

2. For each pair, the inner map and collect will compute need:

   * For `[7, 5, 3]` and `[0, 1, 0]`: `[7 - 0, 5 - 1, 3 - 0] = [7, 4, 3]`
   * For `[3, 2, 2]` and `[2, 0, 0]`: `[3 - 2, 2 - 0, 2 - 0] = [1, 2, 2]`
   * For `[9, 0, 2]` and `[3, 0, 2]`: `[9 - 3, 0 - 0, 2 - 2] = [6, 0, 0]`

3. The outer `collect` gathers these rows, producing:

   * `need = [[7, 4, 3], [1, 2, 2], [6, 0, 0]]`

So, `need` is the remaining resource requirements for each process. This line of code efficiently computes it by 
iterating and performing calculations on corresponding elements in maximum and allocation.

## Implementing the Safety Check

The safety check function will ensure that, after a hypothetical resource allocation, the system remains in a safe 
state.

Here’s the function to check if the system is in a safe state:

{% highlight rust %}
impl System {
    fn is_safe(&self) -> bool {
        let mut work = self.available.clone();
        let mut finish = vec![false; self.need.len()];
        
        loop {
            let mut progress = false;
            for (i, (f, n)) in finish.iter_mut().zip(&self.need).enumerate() {
                if !*f && n.iter().zip(&work).all(|(need, avail)| *need <= *avail) {
                    work.iter_mut().zip(&self.allocation[i]).for_each(|(w, &alloc)| *w += alloc);
                    *f = true;
                    progress = true;
                }
            }
            if !progress {
                break;
            }
        }
        
        finish.iter().all(|&f| f)
    }
}
{% endhighlight %}

Explanation:

* **Work Vector**: work represents the available resources at each step.
* **Finish Vector**: finish keeps track of whether each process can complete with the current work allocation.
* We loop through each process, and if the process’s need can be satisfied by work, we simulate finishing the process by adding its allocated resources back to work.
* This continues until no further progress can be made. If all processes are marked finish, the system is in a safe state.

## Requesting Resources

The `request_resources` function simulates a process requesting resources. The function will:

1. Check if the request is within the need of the process.
2. Temporarily allocate the requested resources and check if the system remains in a safe state.
3. If the system is safe, the request is granted; otherwise, it is denied.

{% highlight rust %}
impl System {
    fn request_resources(&mut self, process_id: usize, request: Vec<i32>) -> bool {
        if request.iter().zip(&self.need[process_id]).any(|(req, need)| *req > *need) {
            println!("Error: Process requested more than its need.");
            return false;
        }

        if request.iter().zip(&self.available).any(|(req, avail)| *req > *avail) {
            println!("Error: Process requested more than available resources.");
            return false;
        }

        // Pretend to allocate resources
        for i in 0..request.len() {
            self.available[i] -= request[i];
            self.allocation[process_id][i] += request[i];
            self.need[process_id][i] -= request[i];
        }

        // Check if the system is safe
        let safe = self.is_safe();

        if safe {
            println!("Request granted for process {}", process_id);
        } else {
            // Roll back if not safe
            for i in 0..request.len() {
                self.available[i] += request[i];
                self.allocation[process_id][i] -= request[i];
                self.need[process_id][i] += request[i];
            }
            println!("Request denied for process {}: Unsafe state.", process_id);
        }

        safe
    }
}
{% endhighlight %}

Explanation:

* The function checks if the request exceeds the `need` or `available` resources.
* If the request can be granted, it temporarily allocates the resources, then calls `is_safe` to check if the new state is safe.
* If the system remains in a safe state, the request is granted; otherwise, it rolls back the allocation.

## Releasing Resources

Processes can release resources they no longer need. This function adds the released resources back to available and 
reduces the process's allocation.

{% highlight rust %}
impl System {
    fn release_resources(&mut self, process_id: usize, release: Vec<i32>) {
        for i in 0..release.len() {
            self.available[i] += release[i];
            self.allocation[process_id][i] -= release[i];
            self.need[process_id][i] += release[i];
        }
        println!("Process {} released resources: {:?}", process_id, release);
    }
}
{% endhighlight %}

## Example Usage

Here’s how you might set up and use the system:

{% highlight rust %}
fn main() {
    let available = vec![10, 5, 7];
    let maximum = vec![
        vec![7, 5, 3],
        vec![3, 2, 2],
        vec![9, 0, 2],
        vec![2, 2, 2],
    ];
    let allocation = vec![
        vec![0, 1, 0],
        vec![2, 0, 0],
        vec![3, 0, 2],
        vec![2, 1, 1],
    ];

    let mut system = System::new(available, maximum, allocation);

    println!("Initial system state: {:?}", system);

    // Process 1 requests resources
    system.request_resources(1, vec![1, 0, 2]);

    // Process 2 releases resources
    system.release_resources(2, vec![1, 0, 0]);

    // Check the system state
    println!("Final system state: {:?}", system);
}
{% endhighlight %}

This setup demonstrates the core of the Banker’s Algorithm: managing safe resource allocation in a multi-process 
environment. By using Rust’s safety guarantees, we’ve built a resource manager that can prevent deadlock.

# Going Multithreaded

The Banker’s Algorithm, as traditionally described, is often presented in a sequential way to focus on the 
resource-allocation logic. However, implementing a multi-threaded version makes it more realistic and challenging, as 
you can simulate processes concurrently requesting and releasing resources.

Let’s extend this code to add a multi-threaded component. Here’s what we’ll do:

* **Simulate Processes as Threads**: Each process will run in its own thread, randomly making requests for resources or releasing them.
* **Synchronize Access**: Since multiple threads will access shared data (i.e., `available`, `maximum`, `allocation`, and `need`), we’ll need to use `Arc` and `Mutex` to make the data accessible and safe across threads.

## Refactor the `System` Structure for Thread Safety

To allow multiple threads to safely access and modify the shared System data, we’ll use `Arc<Mutex<System>>` to wrap the 
entire System. This approach ensures that only one thread can modify the system’s state at any time.

Let’s update our code to add some dependencies:

{% highlight rust %}
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;
use rand::Rng;

{% endhighlight %}

Now, we’ll use `Arc<Mutex<System>>` to safely share this `System` across multiple threads.

## Implement Multi-Threaded Processes

Each process (thread) will:

1. Attempt to request resources at random intervals.
2. Either succeed or get denied based on the system’s safe state.
3. Occasionally release resources to simulate task completion.

Here’s how we might set this up:

{% highlight rust %}
fn main() {
    let available = vec![10, 5, 7];
    let maximum = vec![
        vec![7, 5, 3],
        vec![3, 2, 2],
        vec![9, 0, 2],
        vec![2, 2, 2],
    ];
    let allocation = vec![
        vec![0, 1, 0],
        vec![2, 0, 0],
        vec![3, 0, 2],
        vec![2, 1, 1],
    ];

    // Wrap the system in Arc<Mutex> for safe shared access
    let system = Arc::new(Mutex::new(System::new(available, maximum, allocation)));

    // Create threads for each process
    let mut handles = vec![];
    for process_id in 0..4 {
        let system = Arc::clone(&system);
        let handle = thread::spawn(move || {
            let mut rng = rand::thread_rng();
            loop {
                // Generate a random request with non-negative values within a reasonable range
                let request = vec![
                    rng.gen_range(0..=3),
                    rng.gen_range(0..=2),
                    rng.gen_range(0..=2),
                ];

                // Attempt to request resources
                {
                    let mut sys = system.lock().unwrap();
                    println!("Process {} requesting {:?}", process_id, request);
                    if sys.request_resources(process_id, request.clone()) {
                        println!("Process {} granted {:?}", process_id, request);
                    } else {
                        println!("Process {} denied {:?}", process_id, request);
                    }
                }

                thread::sleep(Duration::from_secs(1));

                // Occasionally release resources, ensuring non-negative values
                let release = vec![
                    rng.gen_range(0..=2),
                    rng.gen_range(0..=1),
                    rng.gen_range(0..=1),
                ];

                {
                    let mut sys = system.lock().unwrap();
                    sys.release_resources(process_id, release.clone());
                    println!("Process {} released {:?}", process_id, release);
                }

                thread::sleep(Duration::from_secs(2));
            }
        });
        handles.push(handle);
    }

    // Wait for all threads to finish (they won't in this infinite example)
    for handle in handles {
        handle.join().unwrap();
    }
}
{% endhighlight %}

## Explanation of the Multi-Threaded Implementation

1. **Random Resource Requests and Releases**:

    * Each process generates a random `request` vector simulating the resources it wants to acquire.
    * It then locks the `system` to call `request_resources`, either granting or denying the request based on the system's safety check. 
    * After a short wait, each process may release some resources (also randomly determined).

2. **Concurrency Management with `Arc<Mutex<System>>`**:

   * Each process clones the `Arc<Mutex<System>>` handle, ensuring shared access to the system.
   * Before each `request_resources` or `release_resources` operation, each process locks the `Mutex` on `System`. This ensures that only one thread modifies the system at any given time, preventing race conditions.
   
3. **Thread Loop:**

    * Each thread runs in an infinite loop, continuously requesting and releasing resources. This simulates real-world processes that may continuously request and release resources over time.



# Conclusion

The Banker’s Algorithm is a powerful way to manage resources safely, and Rust’s type system and memory safety features 
make it well-suited for implementing such algorithms. By simulating requests, releases, and safety checks, you can 
ensure the system remains deadlock-free. This algorithm is especially useful in operating systems, databases, and 
network management scenarios.

By adding multi-threading to the Banker’s Algorithm, we’ve made the simulation more realistic, reflecting how processes 
in a real system might concurrently request and release resources. Rust’s Arc and Mutex constructs ensure safe shared 
access, aligning with Rust’s memory safety guarantees.

This multi-threaded implementation of the Banker’s Algorithm provides:

* **Deadlock Avoidance**: Requests are only granted if they leave the system in a safe state.
* **Resource Allocation Simulation**: Processes continually request and release resources, emulating a dynamic resource allocation environment.

