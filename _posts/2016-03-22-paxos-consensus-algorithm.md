---
layout: post
title: Paxos consensus algorithm
date: 2016-03-22
comments: false
categories: [ "computer science", "distributed", "consensus", "paxos" ]
---

[Paxos]() is a set of protocols designed to solve consensus problems when implementing fault tolerant distributed systems. When many actors in a system can propose values for a system to use, a controlling process (or protocol) needs to do the job of selecting the value and propagating that value's selection to other actors in the system. 

### What are we trying to solve?

As in the introduction, distributed systems have many participants or actors that can propose values. A controlling node or consensus needs to be established on which of these values to actually use. This seems quite trivial in a local sense, but once this problem is distributed amongst separate nodes; the system becomes exponentially more complex. The complexity arises in a distributed setting as messages carrying these values can be lost, delayed or destroyed.

The common issues that you always see in distributed systems really boil down to *reliability* and *consistency*. Network communications and computers in general fail making for a less-than-reliable model. When these things fail, vectors open up allowing inconsistencies to creep into our system's view of the world.

The whole idea of the consensus algorithm is to select a value when they're on offer. Paxos will only select from values that have been offered; it will only select on of those values and the selection isn't announced to other actors unless it's actually made.

### Assumptions

There are a few assumptions of all systems that need to be made clear; this is important as the protocols are designed around abstract concepts that are modeled to reality. The following lists have re-produced from the [Paxos Wikipedia Article](https://en.wikipedia.org/wiki/Paxos_(computer_science))

* Processors
  * Processors operate at arbitrary speed.
  * Processors may experience failures.
  * Processors with stable storage may re-join the protocol after failures 
  * Processors do not collude, lie, or otherwise attempt to subvert the protocol. 

* Network
  * Processors can send messages to any other processor.
  * Messages are sent asynchronously and may take arbitrarily long to deliver.
  * Messages may be lost, reordered, or duplicated.
  * Messages are delivered without corruption. 

### Roles

With those assumptions in place, we can introduce the main roles in the Paxos system. It's important to note at this point that physical computing entities can perform one or many of these roles.

#### Client

The *client* is the creation and termination point for the system. It's a request from a *client* that start the Paxos system in motion. The protocol finished its work with a response being sent back to the *client*. The *client* isn't a part of the system itself, it's just be protocol's main protagonist.

#### Proposer

The *proposers* job takes on requests from the *client*. It's the responsibility of the *proposers* to get a valued agreed upon.

#### Voter

It's the job of the *voter* to accept values from a *proposer*. It's not until the quorum (or majority of voters) agree on a value that a state change happens.

#### Learner

The *learner* propagates its information back to the client once it's notified by the quorum (through a primary acceptor) that a proposal has been accepted.

### Workflow

There's a particular workflow that the Paxos system can follow. There are simple and complex flavors of the workflow which all aim at focusing different parts of the consensus flow. The following steps outline the most basic flavor of the protocol:

#### Prepare & Promise

A *proposer* will send a *prepare request* to as many or every *acceptor* that it can. If the *acceptor* has not yet seen another proposal, a *prepare response* is sent back which promises to not accept another proposal. Any *prepare request* now received by an *acceptor* is ignored if the proposal number is lower than the previous request seen.

This is key, because if an *acceptor* now sees a message with a higher number, it'll process that *prepare request*.

#### Accept

When the quorum send *prepare resonses* back to the proposer (and this ends up as the majority: by definition of quorum), the *proposer* can now start to issue *accept requests*.  If an *acceptor* is sent an *accept request* that has a number equal or above to the highest *proposal* that it has seen, it will accept that request.

This information is now propagated down to all of the learners for them to see that a particular proposal has been accepted.

#### Accepted

The system will now choose a value once learners discover that a majority of nodes have accepted the same value.

### Implementation

A simple implementation of this protocol can be found in [this](https://github.com/cocagne/paxos/) GitHub repository. It walks through a few of the different message flows in nicely laid out Python code. 

