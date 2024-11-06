---
layout: post
title: State Machines
date: 2024-11-06
comments: false
categories: [ "rust" ]
---

# Introduction

State machines are essential in software for managing systems with multiple possible states and well-defined 
transitions. Often used in networking protocols, command processing, or user interfaces, state machines help ensure 
correct behavior by enforcing rules on how a program can transition from one state to another based on specific inputs 
or events.

In Rust, enums and pattern matching make it straightforward to create robust state machines. Rust’s type system enforces 
that only valid transitions happen, reducing errors that can arise in more loosely typed languages. In this article, 
we’ll explore how to design a state machine in Rust that’s both expressive and type-safe, with a concrete example of a 
networking protocol.

# Setting Up the State Machine in Rust

The first step is to define the various states. Using Rust’s `enum`, we can represent each possible state within our 
state machine. For this example, let’s imagine we’re modeling a simple connection lifecycle for a network protocol.

Here’s our ConnectionState enum:

{% highlight rust %}
enum ConnectionState {
    Disconnected,
    Connecting,
    Connected,
    Error,
}
{% endhighlight %}

Each variant represents a specific state that our connection could be in. In a real-world application, you could add 
more states or include additional information within each state, but for simplicity, we’ll focus on these four.

# Defining Transitions

Next, let’s define a `transition` function. This function will dictate the rules for how each state can move to another 
based on events. We’ll introduce another enum, `Event`, to represent the various triggers that cause state transitions:

{% highlight rust %}
enum Event {
    StartConnection,
    ConnectionSuccessful,
    ConnectionFailed,
    Disconnect,
}
{% endhighlight %}

Our `transition` function will take in the current state and an event, then use pattern matching to determine the next 
state.

{% highlight rust %}
impl ConnectionState {
    fn transition(self, event: Event) -> ConnectionState {
        match (self, event) {
            (ConnectionState::Disconnected, Event::StartConnection) => ConnectionState::Connecting,
            (ConnectionState::Connecting, Event::ConnectionSuccessful) => ConnectionState::Connected,
            (ConnectionState::Connecting, Event::ConnectionFailed) => ConnectionState::Error,
            (ConnectionState::Connected, Event::Disconnect) => ConnectionState::Disconnected,
            (ConnectionState::Error, Event::Disconnect) => ConnectionState::Disconnected,
            // No transition possible, remain in the current state
            (state, _) => state,
        }
    }
}
{% endhighlight %}

This function defines the valid state transitions:

* If we’re `Disconnected` and receive a `StartConnection` event, we transition to `Connecting`.
* If we’re `Connecting` and successfully connect, we move to `Connected`.
* If a connection attempt fails, we transition to `Error`.
* If we’re `Connected` or in an `Error` state and receive a `Disconnect` event, we return to `Disconnected`.

Any invalid state-event pair defaults to remaining in the current state.

# Implementing Transitions and Handling Events

To make the state machine operate, let’s add a `Connection` struct that holds the current state and handles the 
transitions based on incoming events.

{% highlight rust %}
struct Connection {
    state: ConnectionState,
}

impl Connection {
    fn new() -> Self {
        Connection {
            state: ConnectionState::Disconnected,
        }
    }

    fn handle_event(&mut self, event: Event) {
        self.state = self.state.transition(event);
    }
}
{% endhighlight %}

Now, we can initialize a connection and handle events:

{% highlight rust %}
fn main() {
    let mut connection = Connection::new();

    connection.handle_event(Event::StartConnection);
    println!("Current state: {:?}", connection.state); // Should be Connecting

    connection.handle_event(Event::ConnectionSuccessful);
    println!("Current state: {:?}", connection.state); // Should be Connected

    connection.handle_event(Event::Disconnect);
    println!("Current state: {:?}", connection.state); // Should be Disconnected
}
{% endhighlight %}

With this setup, we have a fully functional state machine that moves through a predictable set of states based on 
events. Rust’s pattern matching and type-checking ensure that only valid transitions are possible.

# Other Usage

While our connection example is simple, state machines are invaluable for more complex flows, like command processing in 
a CLI or a network protocol. Imagine a scenario where we have commands that can only run under certain conditions.

Let’s say we have a simple command processing machine that recognizes two commands: `Init` and `Process`. The machine 
can only start processing after initialization. Here’s what the implementation might look like:

{% highlight rust %}
enum CommandState {
    Idle,
    Initialized,
    Processing,
}

enum CommandEvent {
    Initialize,
    StartProcessing,
    FinishProcessing,
}

impl CommandState {
    fn transition(self, event: CommandEvent) -> CommandState {
        match (self, event) {
            (CommandState::Idle, CommandEvent::Initialize) => CommandState::Initialized,
            (CommandState::Initialized, CommandEvent::StartProcessing) => CommandState::Processing,
            (CommandState::Processing, CommandEvent::FinishProcessing) => CommandState::Initialized,
            (state, _) => state, // Remain in the current state if transition is invalid
        }
    }
}
{% endhighlight %}

With the same `transition` approach, we could build an interface to handle user commands, enforcing the correct order 
for initializing and processing. This could be extended to handle error states or additional command flows as needed.

# Advantages of Using Rust for State Machines

Rust’s enums and pattern matching provide an efficient, type-safe way to create state machines. The Rust compiler helps 
prevent invalid transitions, as each match pattern must account for all possible states and events. Additionally:

* **Ownership and Lifetimes**: Rust’s strict ownership model ensures that state transitions do not create unexpected side effects.
* **Pattern Matching**: Pattern matching allows concise and readable code, making state transitions easy to follow.
* **Enums with Data**: Rust enums can hold additional data for each state, providing more flexibility in complex state machines.

Rust’s approach to handling state machines is both expressive and ensures that your code remains safe and predictable. 
This makes Rust particularly suited for applications that require strict state management, such as networking or 
command-processing applications.

# Conclusion

State machines are a powerful tool for managing structured transitions between states. Rust’s enums and pattern matching 
make implementing these machines straightforward, with added safety and performance benefits. By taking advantage of 
Rust’s type system, we can create state machines that are both readable and resistant to invalid transitions.