---
layout: post
title: Learning Rust Part 13 - Networking and Protocols
date: 2024-10-30
comments: false
categories: [ "rust" ]
---

# Introduction

Rust’s networking capabilities are both powerful and versatile, supporting everything from low-level socket programming 
to high-level protocols. Whether you’re working with standard protocols like HTTP and MQTT or crafting custom protocols, 
Rust’s libraries offer the tools needed for high-performance and reliable network communication.

# Socket Programming (TCP/UDP)

Socket programming is fundamental to network communication. Rust’s `std::net` module provides basic support for TCP and 
UDP sockets, suitable for low-level client-server applications.

## TCP Sockets

TCP (Transmission Control Protocol) is connection-oriented, ensuring reliable data transmission. Rust’s `TcpListener` 
and `TcpStream` make it easy to listen for and send TCP data.

### Simple TCP Server

{% highlight rust %}
use std::net::{TcpListener, TcpStream};
use std::io::{Read, Write};

fn handle_client(mut stream: TcpStream) {
    let mut buffer = [0; 512];
    stream.read(&mut buffer).unwrap();
    stream.write(&buffer).unwrap();
}

fn main() {
    let listener = TcpListener::bind("127.0.0.1:7878").unwrap();
    for stream in listener.incoming() {
        let stream = stream.unwrap();
        handle_client(stream);
    }
}
{% endhighlight %}

## UDP Sockets

UDP (User Datagram Protocol) is connectionless and best suited for fast, unreliable message delivery. Rust’s 
`UdpSocket` allows for easy creation of UDP clients and servers.

### Simple UDP Client and Server

{% highlight rust %}
use std::net::UdpSocket;

fn main() -> std::io::Result<()> {
    let socket = UdpSocket::bind("127.0.0.1:7878")?;
    socket.send_to(b"Hello, UDP!", "127.0.0.1:7879")?;

    let mut buffer = [0; 512];
    let (amt, src) = socket.recv_from(&mut buffer)?;
    println!("Received {} bytes from {}: {:?}", amt, src, &buffer[..amt]);
    Ok(())
}
{% endhighlight %}

# Low-Level Network Access with `tokio` and `async-std`

For non-blocking network applications, Rust offers asynchronous libraries like `tokio` and `async-std`, which enable 
high-performance I/O without blocking the main thread—ideal for servers handling numerous concurrent connections.

## TCP with `tokio`

`tokio` is Rust’s most popular async runtime, commonly used in web servers and microservices. Here’s a basic 
asynchronous TCP server using `tokio`.

{% highlight rust %}
use tokio::net::TcpListener;
use tokio::io::{self, AsyncReadExt, AsyncWriteExt};

#[tokio::main]
async fn main() -> io::Result<()> {
    let listener = TcpListener::bind("127.0.0.1:7878").await?;
    loop {
        let (mut socket, _) = listener.accept().await?;
        tokio::spawn(async move {
            let mut buffer = [0; 512];
            let _ = socket.read(&mut buffer).await;
            let _ = socket.write_all(&buffer).await;
        });
    }
}
{% endhighlight %}

## TCP with `async-std`

`async-std` is an alternative async library similar to `tokio`, providing asynchronous versions of Rust’s standard 
library functions.

{% highlight rust %}
use async_std::net::TcpListener;
use async_std::prelude::*;
use async_std::task;

fn main() -> std::io::Result<()> {
    task::block_on(async {
        let listener = TcpListener::bind("127.0.0.1:7878").await?;
        while let Ok((mut socket, _)) = listener.accept().await {
            task::spawn(async move {
                let mut buffer = vec![0; 512];
                let _ = socket.read(&mut buffer).await;
                let _ = socket.write_all(&buffer).await;
            });
        }
        Ok(())
    })
}
{% endhighlight %}

# Protocols (HTTP, MQTT, gRPC)

Rust has libraries for common application-layer protocols like HTTP, MQTT, and gRPC, which are widely used in web 
development, IoT, and microservices.

## HTTP with `reqwest` and `hyper`

For HTTP clients, `reqwest` provides an easy-to-use API, while `hyper` is a low-level HTTP library for both clients and 
servers.

{% highlight rust %}
use reqwest::Error;

#[tokio::main]
async fn main() -> Result<(), Error> {
    let response = reqwest::get("https://httpbin.org/get").await?;
    println!("Status: {}", response.status());
    Ok(())
}
{% endhighlight %}

## MQTT with `rumqttc`

MQTT (Message Queuing Telemetry Transport) is a lightweight messaging protocol often used in IoT applications. 
The `rumqttc` library is popular for MQTT in Rust.

{% highlight rust %}
use rumqttc::{MqttOptions, Client, QoS};

fn main() {
    let mut mqttoptions = MqttOptions::new("client_id", "broker.hivemq.com", 1883);
    let (mut client, mut connection) = Client::new(mqttoptions, 10);
    client.subscribe("hello/world", QoS::AtLeastOnce).unwrap();

    for notification in connection.iter() {
        println!("{:?}", notification);
    }
}
{% endhighlight %}

## gRPC with `tonic`

gRPC is an RPC framework based on HTTP/2, ideal for high-performance microservices. `tonic` provides async support for 
gRPC in Rust.

{% highlight rust %}
use tonic::{transport::Server, Request, Response, Status};
use hello_world::greeter_server::{Greeter, GreeterServer};
use hello_world::HelloReply;

mod hello_world {
    tonic::include_proto!("helloworld");
}

#[derive(Default)]
pub struct MyGreeter;

#[tonic::async_trait]
impl Greeter for MyGreeter {
    async fn say_hello(
        &self,
        request: Request<hello_world::HelloRequest>,
    ) -> Result<Response<HelloReply>, Status> {
        let reply = hello_world::HelloReply {
            message: format!("Hello {}", request.into_inner().name),
        };
        Ok(Response::new(reply))
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let addr = "127.0.0.1:50051".parse().unwrap();
    let greeter = MyGreeter::default();

    Server::builder()
        .add_service(GreeterServer::new(greeter))
        .serve(addr)
        .await?;
    Ok(())
}
{% endhighlight %}

# Custom Protocols with Rust

Rust’s type safety and low-level control make it ideal for creating custom network protocols. Using `tokio` or 
`async-std`, you can manage connections, implement unique message structures, and handle various communication patterns.

## Defining a Simple Custom Protocol

Suppose you need a custom protocol where messages start with a fixed header followed by a payload. Here’s how to define 
this structure and handle parsing.

{% highlight rust %}
use tokio::io::{self, AsyncReadExt, AsyncWriteExt};
use tokio::net::TcpStream;

async fn send_message(mut stream: TcpStream, message: &[u8]) -> io::Result<()> {
    let header = (message.len() as u16).to_be_bytes(); // Message length header
    stream.write_all(&header).await?;
    stream.write_all(message).await?;
    Ok(())
}

async fn receive_message(mut stream: TcpStream) -> io::Result<Vec<u8>> {
    let mut header = [0; 2];
    stream.read_exact(&mut header).await?;
    let length = u16::from_be_bytes(header) as usize;
    let mut buffer = vec![0; length];
    stream.read_exact(&mut buffer).await?;
    Ok(buffer)
}
{% endhighlight %}

# Serializing/Deserializing Network Messages

Rust’s serialization libraries, like `serde`, simplify encoding and decoding network messages. Using `serde`, you can 
define structured data and serialize it to JSON, MessagePack, or other formats.

## Using `serde` with JSON

The `serde_json` crate makes it easy to serialize and deserialize Rust types to JSON, which is suitable for APIs or 
custom protocols.

{% highlight rust %}
use serde::{Serialize, Deserialize};
use serde_json;

#[derive(Serialize, Deserialize, Debug)]
struct Message {
    id: u32,
    content: String,
}

fn main() -> serde_json::Result<()> {
    let msg = Message { id: 1, content: "Hello, Rust!".to_string() };
    let json = serde_json::to_string(&msg)?;
    println!("Serialized: {}", json);

    let deserialized: Message = serde_json::from_str(&json)?;
    println!("Deserialized: {:?}", deserialized);
    Ok(())
}
{% endhighlight %}

# Summary

Rust’s networking capabilities support a wide range of applications, from low-level socket programming to high-level 
protocol handling. With libraries like `tokio`, `async-std`, and `serde`, Rust enables both synchronous and asynchronous 
communication, making it a great choice for building robust networked applications.
