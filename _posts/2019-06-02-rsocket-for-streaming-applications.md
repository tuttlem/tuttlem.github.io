---
layout: post
title: RSocket for streaming applications
date: 2019-06-02
comments: false
categories: [ "" ]
---

### Introduction

In a world of changing software requirements, and more demand for results and analytics in realtime, framework and system creators have needed to become smarter with the way that they reason about information ingestion. Streaming frameworks have started to make good ground in establishing themselves as accessible software platforms for many developers.

In today's article, we'll explore [RSocket](http://rsocket.io/) very briefly. [RSocket](http://rsocket.io/)'s place is to provide an application protocol that is directly designed for reactive streaming applications.

### Core Feature

The core features of implementing this specification are as follows:

* Metadata and Payload frames
* All 4 interaction models : Fire-and-forget, request/response, requestStream, requestChannel
* Request-N frame : application level flow control
* Fragmentation/Reassembly : Drivers are assumed to fully encode/decode the expected user data type
* Keep-Alive frame : A responder receiving a keep-alive frame must reply a keep-alive frame
* Error Frame : in order to fully support connection lifecycle
* Handling the unexpected : If Resumption, Leasing or an extension is not supported, rejected error frames must be used


### Example

The following are two code snippets take from the [RSocket](http://rsocket.io/) website.

The sample server:

{% highlight java %}
RSocketFactory.receive()
    .frameDecoder(Frame::retain)
    .acceptor(new PingHandler())
    .transport(TcpServerTransport.create(7878))
    .start()
    .block()
    .onClose();
{% endhighlight %}

The sample client:

{% highlight java %}
Mono<RSocket> client =
    RSocketFactory.connect()
        .frameDecoder(Frame::retain)
        .transport(TcpClientTransport.create(7878))
        .start();

PingClient pingClient = new PingClient(client);

Recorder recorder = pingClient.startTracker(Duration.ofSeconds(1));

int count = 1_000;

pingClient
    .startPingPong(count, recorder)
    .doOnTerminate(() -> System.out.println("Sent " + count + " messages."))
    .blockLast();
{% endhighlight %}


