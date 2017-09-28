---
layout: post
title: Create a REST API with Go
date: 2017-09-27
comments: false
categories: [ "rest", "api", "go" ]
---

Let's create a [REST api](https://en.wikipedia.org/wiki/Representational_state_transfer) using [golang](https://golang.org/). In our example, we'll walk through what's required to make an API for a Todo-style application. 

## Starting off

First up, we're going to create a project. I've called mine "todo".

{% highlight bash %}
mkdir -p $GOPATH/src/github.com/tuttlem/todo
{% endhighlight %}

This gives us a project folder. Start off editing your `main.go` file. We'll pop the whole application into this single file, as it'll be simple enough.

{% highlight golang %}
package main

import (
  "fmt"
)

func main() {
  fmt.Println("Todo application")
}
{% endhighlight %}

## The Server

We can turn our console application now into a server application pretty easily with the `net/http` module. Once we import this, we'll use the `ListenAndServe` function to stand a server up. While we're at it, we'll create a `NotImplementedHandler` so we can assertivly tell our calling clients that we haven't done anything just yet.

{% highlight golang %}
package main

import (
  "net/http"
)

func main() {

  // start the server listening, and always sending back
  // the "NotImplemented" message
  http.ListenAndServe(":3000", NotImplementedHandler);

}

var NotImplementedHandler = http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
  w.Header().Set("Content-Type", "application/json")
  w.WriteHeader(http.StatusNotImplemented)
})
{% endhighlight %}

Testing this service will be a little pointless, but we can see our 501's being thrown:

{% highlight text %}
➜  ~ curl --verbose http://localhost:3000/something
*   Trying 127.0.0.1...
* Connected to localhost (127.0.0.1) port 3000 (#0)
> GET /something HTTP/1.1
> Host: localhost:3000
> User-Agent: curl/7.47.0
> Accept: */*
> 
< HTTP/1.1 501 Not Implemented
< Content-Type: application/json
< Date: Wed, 27 Sep 2017 13:26:33 GMT
< Content-Length: 0
< 
* Connection #0 to host localhost left intact
{% endhighlight %}

## Routing

Routing will allow us to direct a user's request to the correct piece of functionality. Routing also helps us extract input parameters for requests. Using [mux from gorilla](https://github.com/gorilla/mux) we can quickly setup the list, create, update and delete endpoints we need to accomplish our TODO application.

{% highlight golang %}
import (
  // . . . 
  "github.com/gorilla/mux"
  // . . . 
)

func main() {

  r := mux.NewRouter()

  r.Handle("/todos", NotImplementedHandler).Methods("GET")
  r.Handle("/todos", NotImplementedHandler).Methods("POST")
  r.Handle("/todos/{id}", NotImplementedHandler).Methods("PUT")
  r.Handle("/todos/{id}", NotImplementedHandler).Methods("DELETE")

  // start the server listening, and always sending back
  // the "NotImplemented" message
  http.ListenAndServe(":3000", r);

}
{% endhighlight %}

What's nice about this, is that our actual routes are what will emit the `501`. Anything that completely misses the router will result in a much more accurate `404`. Perfect.

## Handlers

We can give the server some handlers now. A handler takes the common shape of:

{% highlight golang %}
func handler(w http.ResponseWriter, r *http.Request) {
}
{% endhighlight %}

The `http.ResponseWriter` typed `w` parameter is what we'll use to send a payload back to the client. `r` takes the form of the request, and it's what we'll use as an input to the process. This is all looking very "server's output as a function of its input" to me.

{% highlight golang %}
var ListTodoHandler = NotImplementedHandler
var CreateTodoHandler = NotImplementedHandler
var UpdateTodoHandler = NotImplementedHandler
var DeleteTodoHandler = NotImplementedHandler
{% endhighlight %}

Which means that our router (whilst still unimplemented) starts to make a little more sense.

{% highlight golang %}
r.Handle("/todos", ListTodoHandler).Methods("GET")
r.Handle("/todos", CreateTodoHandler).Methods("POST")
r.Handle("/todos/{id}", UpdateTodoHandler).Methods("PUT")
r.Handle("/todos/{id}", DeleteTodoHandler).Methods("DELETE")
{% endhighlight %}

## Modelling data

We need to start modelling this data so that we can prepare an API to work with it. The following `type` declaration creates a structure that will define our todo item:

{% highlight golang %}
type Todo struct {
  Id              int    `json:"id"`
  Description     string `json:"description"`
  Complete        bool   `json:"complete"`
}
{% endhighlight %}

Note the `json` directives at the end of each of the members in the structure. This is allowing us to control how the member is represented as an encoded JSON value. A more idomatic JSON has lowercased member names.

The "database" that our API will manage is a slice.

{% highlight golang %}
var Todos []Todo
var Id int

// . . . inside "main"
// Initialize the todo "database"
Id = 1
Todos = []Todo{ Todo{Id: Id, Description: "Buy Cola"} }
{% endhighlight %}

## Implementation

To "list" out todo items, we simply return the encoded slice.

{% highlight golang %}
var ListTodoHandler = http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
  json.NewEncoder(w).Encode(Todos);  
})
{% endhighlight %}

Creating an item is a bit more complex due to value marshalling.

{% highlight golang %}
var CreateTodoHandler = http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
  decoder := json.NewDecoder(r.Body);
  var newTodo Todo

  err := decoder.Decode(&newTodo)

  if err != nil {
    w.WriteHeader(http.StatusInternalServerError)
    return
  } 

  defer r.Body.Close()

  Id ++
  newTodo.Id = Id

  Todos = append(Todos, newTodo)

  w.WriteHeader(http.StatusCreated)
  json.NewEncoder(w).Encode(Id);  
})
{% endhighlight %}

In order to implement a delete function, we need a `Filter` implementation that knows about `Todo` objects.

{% highlight golang %}
func Filter(vs []Todo, f func(Todo) bool) []Todo {
  vsf := make([]Todo, 0)
  for _, v := range vs {
    if f(v) {
      vsf = append(vsf, v)
    }
  }
  return vsf
}
{% endhighlight %}

We then add a reference to `strconv` because we'll need `Atoi` to take in the `string` `id` and convert it to an `int`. Remember, the `Id` attribute of our `Todo` object is an `int`.

{% highlight golang %}
var DeleteTodoHandler = http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
  params := mux.Vars(r)
  id, _ := strconv.Atoi(params["id"])

  Todos = Filter(Todos, func(t Todo) bool { 
    return t.Id != id
  })

  w.WriteHeader(http.StatusNoContent)
})
{% endhighlight %}

Finally, an update. We'll do the same thing as a `DELETE`, but we'll swap the posted object in.

{% highlight golang %}
var UpdateTodoHandler = http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
  params := mux.Vars(r)
  id, _ := strconv.Atoi(params["id"])

  Todos = Filter(Todos, func(t Todo) bool { 
    return t.Id != id
  })

  decoder := json.NewDecoder(r.Body);
  var newTodo Todo

  err := decoder.Decode(&newTodo)

  if err != nil {
    w.WriteHeader(http.StatusInternalServerError)
    return
  } 

  defer r.Body.Close()

  newTodo.Id = id

  Todos = append(Todos, newTodo)

  w.WriteHeader(http.StatusNoContent)
})
{% endhighlight %}

The `UpdateTodoHandler` appears to be a mix of the delete action as well as create.

## Up and running

You're just about done. The Todo api is doing what we've asked it to do. The only thing left now, is to get some logging going. We'll do that with some clever middleware [again, from gorilla](https://github.com/gorilla/handlers) that will do just that.

{% highlight golang %}
import (
  // . . .

  "os"

  "github.com/gorilla/handlers"
 
  // . . . 

)

// . . down in main() now

  http.ListenAndServe(":3000", 
    handlers.LoggingHandler(os.Stdout, r))

{% endhighlight %}

This now gives us a status on requests hitting our server.

## That's all

That's all for now. The [full source](https://gist.github.com/tuttlem/a730ab82760073682dab6c009dce186e) is available as a gist.
