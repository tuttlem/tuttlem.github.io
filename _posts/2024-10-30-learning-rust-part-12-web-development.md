---
layout: post
title: Learning Rust Part 12 - Web Development
date: 2024-10-30
comments: false
categories: [ "" ]
---

# Introduction

Rust has gained significant traction in web development thanks to its speed, safety, and a growing ecosystem of web 
frameworks and libraries. From high-performance APIs to cross-platform applications with WebAssembly, Rust provides 
numerous tools for both backend and frontend development. This post explores popular tools in Rust’s web development 
toolkit, covering HTTP clients, REST API frameworks, asynchronous web frameworks, WebAssembly, frontend libraries, 
and cross-platform solutions like Tauri.

## HTTP Clients and Servers

Rust provides several libraries for making HTTP requests and building HTTP servers.

## reqwest - HTTP Client

`reqwest` is a user-friendly HTTP client built on top of `hyper`, offering an easy interface for making asynchronous 
requests.

{% highlight rust %}
use reqwest::Error;

#[tokio::main]
async fn main() -> Result<(), Error> {
    let response = reqwest::get("https://api.github.com").await?;
    println!("Status: {}", response.status());
    Ok(())
}
{% endhighlight %}

## hyper - Low-Level HTTP Client and Server

`hyper` is a low-level HTTP library suitable for building HTTP servers and clients where you need fine-grained control.

{% highlight rust %}
use hyper::{Body, Request, Response, Server};
use hyper::service::{make_service_fn, service_fn};

async fn handle_request(_: Request<Body>) -> Result<Response<Body>, hyper::Error> {
    Ok(Response::new(Body::from("Hello, World!")))
}

#[tokio::main]
async fn main() {
    let make_svc = make_service_fn(|_| async { Ok::<_, hyper::Error>(service_fn(handle_request)) });
    let addr = ([127, 0, 0, 1], 3000).into();

    let server = Server::bind(&addr).serve(make_svc);
    println!("Listening on http://{}", addr);

    if let Err(e) = server.await {
        eprintln!("Server error: {}", e);
    }
}
{% endhighlight %}

## actix-web - Full-Featured Web Framework

`actix-web` is a high-performance web framework suitable for building complex applications and REST APIs. Based on the 
`actix` actor framework, it offers excellent concurrency.

{% highlight rust %}
use actix_web::{get, App, HttpServer, Responder};

#[get("/")]
async fn hello() -> impl Responder {
    "Hello, Actix-web!"
}

#[actix_rt::main]
async fn main() -> std::io::Result<()> {
    HttpServer::new(|| App::new().service(hello))
        .bind("127.0.0.1:8080")?
        .run()
        .await
}
{% endhighlight %}

# REST API Development

Rust’s ecosystem supports building robust REST APIs with frameworks like `warp` and `rocket`, in addition to 
`actix-web`.

## Building REST APIs with warp

`warp` is a lightweight, flexible, and composable web framework that’s asynchronous by default, ideal for creating 
RESTful APIs with minimal boilerplate.

{% highlight rust %}
use warp::Filter;

#[tokio::main]
async fn main() {
    let hello = warp::path::end().map(|| warp::reply::json(&"Hello, Warp!"));

    warp::serve(hello)
        .run(([127, 0, 0, 1], 3030))
        .await;
}
{% endhighlight %}

## Building REST APIs with rocket

`rocket` is known for its simplicity and ease of use, managing routing, parameter parsing, and JSON serialization 
automatically.

{% highlight rust %}
#[macro_use] extern crate rocket;

#[get("/")]
fn hello() -> &'static str {
    "Hello, Rocket!"
}

#[launch]
fn rocket() -> _ {
    rocket::build().mount("/", routes![hello])
}
{% endhighlight %}

# Asynchronous Web Frameworks (warp, rocket)

Both `warp` and `rocket` support asynchronous programming, enabling scalable, non-blocking web services.

## Asynchronous Handler Example in warp

In `warp`, asynchronous handlers are defined using `async` functions, allowing for efficient handling of multiple 
connections.

{% highlight rust %}
use warp::Filter;

#[tokio::main]
async fn main() {
    let hello = warp::path::end().map(|| async { warp::reply::json(&"Hello, async Warp!") });

    warp::serve(hello)
        .run(([127, 0, 0, 1], 3030))
        .await;
}
{% endhighlight %}

# WebAssembly (Wasm) and Rust

WebAssembly (Wasm) allows Rust to run in the browser, making high-performance applications possible on the web. Rust’s 
`wasm-pack` tool simplifies packaging and deploying Rust code as Wasm.

## Setting up a WebAssembly Project with wasm-pack

Install `wasm-pack`:

{% highlight bash %}
cargo install wasm-pack
{% endhighlight %}

Create a new project:

{% highlight bash %}
wasm-pack new my_wasm_project
cd my_wasm_project
{% endhighlight %}

Build and generate Wasm:

{% highlight bash %}
wasm-pack build --target web
{% endhighlight %}

Rust with Wasm is ideal for applications requiring high-performance computations, like game engines or real-time data 
visualizations.

# Frontend Development with Yew and Sycamore

Rust has emerging frontend frameworks like `Yew` and `Sycamore` for building interactive web applications.

## Yew

`Yew` is inspired by React, allowing Rust code to manage component-based UIs in the browser via WebAssembly.

{% highlight rust %}
use yew::prelude::*;

struct App;

impl Component for App {
    type Message = ();
    type Properties = ();

    fn create(_: Self::Properties, _: ComponentLink<Self>) -> Self {
        App
    }

    fn update(&mut self, _: Self::Message) -> bool {
        true
    }

    fn view(&self) -> Html {
        html! {
            <div>
                <h1>{ "Hello, Yew!" }</h1>
            </div>
        }
    }
}

fn main() {
    yew::start_app::<App>();
}
{% endhighlight %}

## Sycamore

`Sycamore` is another WebAssembly-based frontend library, offering reactivity and efficient rendering, much like React 
or Solid.js.

{% highlight rust %}
use sycamore::prelude::*;

#[component]
fn App<G: Html>() -> View<G> {
    view! {
        h1 { "Hello, Sycamore!" }
    }
}

fn main() {
    sycamore::render(|| view! { App {} });
}
{% endhighlight %}

# Cross-platform Web and Mobile Apps with Tauri

`Tauri` is a Rust-based framework for building lightweight, secure desktop applications with web technologies. Tauri 
uses Rust for the backend and HTML/CSS/JavaScript for the frontend, providing an alternative to Electron with lower 
memory usage.

## Setting Up Tauri

Install Tauri CLI:

{% highlight bash %}
cargo install tauri-cli
{% endhighlight %}

Create a new Tauri project:

{% highlight bash %}
tauri init
{% endhighlight %}

Build and run the app:

{% highlight bash %}
cargo tauri dev
{% endhighlight %}

Tauri is ideal for web-based desktop applications that require native capabilities like filesystem access and system 
notifications.

# Summary

Rust’s growing web ecosystem includes powerful libraries and frameworks for server-side development, REST APIs, and 
cross-platform applications. Whether building high-performance APIs with `warp`, creating frontend interfaces with 
`Yew`, or deploying Rust with WebAssembly, Rust provides a robust toolkit for modern web development.
