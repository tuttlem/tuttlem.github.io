---
layout: post
title: Simulating a Scheduler
date: 2025-11-11
comments: false
categories: [ os, osdev, scheduling, processes ]
---

# Introduction

Most operating systems books talk about _schedulers_ as if they’re mysterious forces that magically decide which program 
runs next. You’ll usually see phrases like _context switching_, _round-robin_, _priority queues_, and 
_tasks yielding the CPU_—but the moment you try to understand how that works mechanically, the examples jump straight 
into kernel code, hardware timers, and interrupt controllers.

That’s... a lot.

Before touching real hardware, it’s far easier to learn the concept in a sandbox: a tiny world that behaves like an 
operating system, but without all the baggage. That’s what we’re going to build.

In this post, we’ll **simulate a scheduler**.

We’ll:

* Define a small set of opcodes—like a micro-instruction set for “programs.”
* Write a mini assembler that converts human-readable instructions to bytecodes.
* Represent each program as a task inside a virtual machine.
* Implement a scheduler that runs these tasks, switching between them to simulate concurrency.

The final result:

* multiple tiny “programs”
* all appearing to run at the same time
* under the control of a scheduler you wrote

No threads. No OS dependencies. No unsafe code. Just pure, understandable logic.

By the time we reach the end, you’ll not only understand what a scheduler does—you’ll understand _why it needs to exist_, 
and the mental model will stay with you when you look at real operating systems later.

Let’s build a scheduler from scratch.

# Programs

Before scheduling, we need programs. We need a way to define, write, and store programs so that we can load these into 
our computer.

## Instruction Set

First up, we're going to define a set of instructions that our "computer" will execute. We'll define these as a well 
known common set of instructions as there's a few components that will need to understand these. The virtual machine 
that we'll get to will use these to interpret and perform action, but our assembler will have the job of converting 
those instructions into bytes on disk.

Here's the instructions:

{% highlight rust %}
#[repr(u8)]
#[derive(Copy, Clone, Debug)]
pub enum Instruction {
    NOP = 0x00,
    PUSH = 0x01,
    LOAD = 0x02,
    ADD = 0x03,
    SUB = 0x04,
    PRINT = 0x05,
    SLEEP = 0x06,
    YIELD = 0x07,
    HALT = 0x08,
    WORK = 0x09,
    SETPRIO = 0x0A,
}
{% endhighlight %}

As you can see, our computer won't do a lot. Its functionality isn't the subject here though; we want to see 
how these instructions get scheduled.

We use `repr(u8)` to make casting to `u8` a much smoother process; that will help our assembler later on change our 
defined instructions into programs on disk.

Conversely, we need to make the bytes-on-disk to instructions-in-memory translation easier.

{% highlight rust %}
impl Instruction {
    pub fn from_byte(byte: u8) -> Option<Self> {
        match byte {
            0x00 => Some(Instruction::NOP),
            0x01 => Some(Instruction::PUSH),
            0x02 => Some(Instruction::LOAD),
            0x03 => Some(Instruction::ADD),
            0x04 => Some(Instruction::SUB),
            0x05 => Some(Instruction::PRINT),
            0x06 => Some(Instruction::SLEEP),
            0x07 => Some(Instruction::YIELD),
            0x08 => Some(Instruction::HALT),
            0x09 => Some(Instruction::WORK),
            0x0A => Some(Instruction::SETPRIO),
            _ => None,
        }
    }
}
{% endhighlight %}

## Assembling Programs

With a solid set of instructions defined, we can now write some code to take a list of instrucitons and write them to 
disk. We'd call this a binary.

{% highlight rust %}
pub fn translate_instructions_to_bytes(instructions: &Program) -> Vec<u8> {
    instructions
        .iter()
        .map(|i| *i as u8)
        .collect::<Vec<u8>>()
}

pub fn assemble_to_disk(instructions: &Program, path: &str) -> std::io::Result<()> {
    let bytes = translate_instructions_to_bytes(instructions);
    std::fs::write(path, bytes)
}
{% endhighlight %}

The `Program` type is simply a synonym for `Vec<Instruction>`.

We can use this now to assemble some programs:

{% highlight rust %}
let p1 = vec![
    Instruction::PUSH,
    Instruction::ADD,
    Instruction::SUB,
    Instruction::LOAD,
    Instruction::HALT,
];

assemble_to_disk(&p1, "./bins/p1.bin").expect("couldn't write binary");
{% endhighlight %}

Outside the context of our rust program, we can verify the content of the binary with `hexdump`:

{% highlight plain %}
$ hexdump -C bins/p1.bin

00000000  01 03 04 02 08       |.....|
{% endhighlight %}

Those byte values marry up to what our program defines. You can see it's important that our [ISA](https://en.wikipedia.org/wiki/Instruction_set_architecture) 
doesn't change now, otherwise our programs will have completely different meanings!

Of course, if it does change - we simply re-assemble our programs.

## Tasks

Now we define our `Task` abstraction. A task is how our virtual machine will store the state of a program that's been 
loaded off of disk and is executing / has executed. We only need some basics to define the state of our task.

{% highlight rust %}
pub struct Task {
    /// The path to the task
    pub path: String,
    
    /// Program code
    pub code: Program,
    
    /// Current program counter (instruction pointer register)
    pub pc: usize,
    
    /// Stack state
    pub stack: Vec<i32>,
    
    /// Determines if this task is still running
    pub done: bool,
    
    /// Priority of this task
    pub priority: usize,
}
{% endhighlight %}

We can load programs off of disk and make tasks out of them ready to execute.

{% highlight rust %}
impl Task {
    pub fn load(path: &str) -> std::io::Result<Task> {
        let raw_code = std::fs::read(path).expect("Failed to read file");
        let instructions = raw_code
            .iter()
            .filter_map(|&b| Instruction::from_byte(b)) /* filter bytes out that are invalid */
            .collect::<Vec<Instruction>>();
        
        let task = Task {
            path: path.to_string(),
            code: instructions,
            pc: 0,
            stack: [].to_vec(),
            done: false,
            priority: 0
        };

        return Ok(task);
    }
}
{% endhighlight %}

# Virtual Machine

Now that we have tasks loaded from disk, we need something that can *run* them.

Enter the **virtual machine**.

This is the component responsible for:

* tracking and storing tasks,
* executing one instruction at a time,
* and cooperatively multitasking between tasks (our scheduler).

Let’s look at the core VM structure:

{% highlight rust %}
pub struct VM {
    pub tasks: Vec<Task>,
}

impl VM {
    pub fn new() -> Self {
        Self { tasks: Vec::new() }
    }

    pub fn add_task(&mut self, task: Task) {
        self.tasks.push(task);
    }

    fn has_runnables(&self) -> bool {
        return self.tasks.len() > 0 && self.tasks.iter().any(|t| !t.done);
    }

    fn execute(&self, task: &Task) {
        let instruction = task.code[task.pc];
        println!("[{}]: {:?}", task.path, instruction);
    }
}
{% endhighlight %}

The virtual machine stores a collection of `Task`s, and has three responsibilities:

1. `add_task` – load a task into the VM.
2. `has_runnables` – check if any task still needs CPU time.
3. `execute` – run *one* instruction for a given task.

Note: `execute` receives a reference to a task (`&Task`). We don’t take ownership of the task, because the scheduler must be able to revisit it later and resume execution.

## Scheduling — Round Robin

Now that we have tasks stored in the VM, we need to *schedule* them.

We're going to implement the simplest scheduling algorithm: **round robin**.

Round robin is:

* fair (every task gets a turn),
* predictable (runs tasks in order),
* and conceptually simple (loop through the task list over and over).

Here is the scheduler loop:

{% highlight rust %}
pub fn run_round_robin(&mut self) {
    while self.has_runnables() {
        // loop each task once per round
        for idx in (0..self.tasks.len()) {
            {
                let task_ref: &Task = &self.tasks[idx];

                if task_ref.done {
                    continue;
                }

                // print the instruction that's about to execute
                self.execute(task_ref);
            }

            // now we advance the task forward one instruction
            let task = &mut self.tasks[idx];

            if !task.done {
                task.step();

                if task.pc >= task.code.len() {
                    task.done = true;
                }
            }
        }

        // cleanup (remove finished tasks from the VM)
        self.tasks.retain(|t| !t.done);
    }
}
{% endhighlight %}

A few important points:

* Only one instruction executes per task per loop — simulating time slices.
* Each iteration of the outer `while` loop represents one *round* of CPU scheduling.
* `retain` removes finished tasks so the VM doesn't waste time checking them.

That’s cooperative multitasking: the VM does just enough work per task, then moves on.

## Test Programs

We need some test programs to feed into this system. Here's the code (using our assembler) that creates three
programs that we can use called `p1`, `p2`, and `p3`.

{% highlight rust %}
let p1 = vec![
    Instruction::PUSH,
    Instruction::ADD,
    Instruction::SUB,
    Instruction::LOAD,
    Instruction::HALT,
];

let p2 = vec![
    Instruction::SLEEP,
    Instruction::SLEEP,
    Instruction::SLEEP,
    Instruction::NOP,
    Instruction::HALT,
];

let p3 = vec![
    Instruction::PUSH,
    Instruction::LOAD,
    Instruction::HALT,
];

assemble_to_disk(&p1, "./bins/p1.bin").expect("couldn't write binary");
assemble_to_disk(&p2, "./bins/p2.bin").expect("couldn't write binary");
assemble_to_disk(&p3, "./bins/p3.bin").expect("couldn't write binary");
{% endhighlight %}

With these programs, you'll now be able to see how they "animate" through the scheduler as it chooses which to execute.

## Running It

Let's put it all together.

We'll load three programs — each assembled into a `.bin` file — and execute them through our VM:

{% highlight rust %}
let mut vm = VM::new();

vm.add_task(Task::load("./bins/p1.bin").expect("couldn't read binary"));
vm.add_task(Task::load("./bins/p2.bin").expect("couldn't read binary"));
vm.add_task(Task::load("./bins/p3.bin").expect("couldn't read binary"));

vm.run_round_robin();
{% endhighlight %}

Running this produces output like the following:

{% highlight plain %}
[./bins/p1.bin]: PUSH
[./bins/p2.bin]: SLEEP
[./bins/p3.bin]: PUSH
[./bins/p1.bin]: ADD
[./bins/p2.bin]: SLEEP
[./bins/p3.bin]: LOAD
[./bins/p1.bin]: SUB
[./bins/p2.bin]: SLEEP
[./bins/p3.bin]: HALT
[./bins/p1.bin]: LOAD
[./bins/p2.bin]: NOP
[./bins/p1.bin]: HALT
[./bins/p2.bin]: HALT
{% endhighlight %}

Each task gets one instruction at a time.  
They *appear* to multitask, but really, we are just **interleaving execution**.

This is exactly how early cooperative schedulers worked.

## Different Algorithms

We implemented Round Robin and looked at Priority Scheduling, but operating systems use a variety of scheduling strategies.
Each one optimizes something different — fairness, throughput, responsiveness, or predictability.

Here’s a breakdown of the most common ones you’ll see in real operating systems:

### **First-Come, First-Served (FCFS)**

The simplest possible scheduler.

* Tasks are run in the order they arrive.
* No preemption — once a task starts running, it keeps the CPU until completion.

**Pros:**
* Very predictable.
* Easy to implement.

**Cons:**
* Terrible response time — one long task can block all others (the “convoy effect”).

Used in: batch systems, print queues, embedded devices.

### **Shortest Job First (SJF) / Shortest Remaining Time First (SRTF)**

Runs the shortest task first.

* SJF — non-preemptive (once a task starts, it finishes).
* SRTF — preemptive (if a new shorter task arrives, preempt the current one).

**Pros:**
* Great throughput (lowest total completion time).

**Cons:**
* Requires knowing how long tasks will run (hard in general).
* Small tasks can starve large tasks.

Used in: job schedulers, long-running batch systems, HPC.

### **Round Robin (RR)** *(what we implemented)*

Each task gets a fixed unit of time (called a *quantum*), and then moves to the back of the queue.

**Pros:**
* Very fair.
* Great for interactive workloads (UI responsiveness).

**Cons:**
* If quantum is too short: too much overhead (context switching).
* If quantum too long: behaves like FCFS.

Used in: timesharing OS kernels, early Unix schedulers.

### **Priority Scheduling**

Each task has a priority number.

* Always selects the runnable task with the highest priority.
* Can be preemptive or non-preemptive.

**Pros:**
* High-importance tasks get CPU time first.

**Cons:**
* Starvation — low priority tasks may never run.

Used in: realtime systems, audio/video processing, embedded control software.

### **Multi-Level Feedback Queue (MLFQ)** *(how modern OS schedulers work)*

Combines **priority** and **round robin**.

* Multiple queues (high priority to low priority)
* Round Robin within each queue
* Tasks that use a lot of CPU get *demoted* to lower priority queues
* Tasks that frequently yield or sleep get *promoted* (interactive = fast)

**Pros:**
* Gives priority to interactive tasks
* Penalizes CPU-bound tasks
* Adapts automatically over time (no tuning per process)

**Cons:**
* Harder to implement
* Requires tracking task behavior (history)

Used in: Windows, macOS, Linux (CFS is conceptually similar).

## Comparison

| Algorithm                  | Preemptive | Goal / Optimization                 | Fairness | Weaknesses                         |
|----------------------------|------------|-------------------------------------|-----|------------------------------------|
| FCFS                       | N          | Simplicity                          | Low | One long task blocks everything   |
| SJF / SRTF                 | Y/N        | Lowest total completion time        | Low | Starvation of long tasks          |
| Round Robin                | Y          | Responsiveness / interactivity      | High | Requires good quantum tuning      |
| Priority Scheduling        | Y/N        | Importance / latency sensitivity    | Low | Starvation of low priorities      |
| Multi-Level Feedback Queue | Y          | Realistic, adaptive fairness        | High | More complex to implement         |

### TL;DR

Different schedulers optimize for different outcomes:

* **Fairness?** → Round Robin  
* **Highest priority first?** → Priority Scheduling  
* **Best throughput?** → Shortest Job First / SRTF  
* **Real OS behavior?** → Multi-Level Feedback Queue  

Schedulers are tradeoffs — once you understand *what they optimize*, you understand why real operating systems don’t use just one mechanism.

# Conclusion

We started with nothing but a tiny instruction set and finished with:

* a program format,
* an assembler,
* a task abstraction,
* a virtual machine,
* and a functioning scheduler.

The magic was never "multitasking" — it was **switching to the next task at the right moment**.

Schedulers are simple at their heart:

> Run something for a bit. Save its state. Move on.

Now that we’ve built a round-robin scheduler, it's easy to extend:

* task priorities (`SETPRIO`),
* `YIELD` instruction support,
* blocking + sleeping tasks,
* time-based preemption (tick interrupts).

But those are chapters for another day.