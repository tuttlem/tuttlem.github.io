---
layout: post
title: RPC for your Python code
date: 2017-10-02
comments: false
categories: [ "rpc", "protobuf", "python" ]
---

[gRPC](https://grpc.io/) is an [RPC](https://en.wikipedia.org/wiki/Remote_procedure_call) framework from Google that simplifies standing your application up for remote access.

In today's article, we'll build a remote calculator.

## Prepare your system

Before we begin, you'll need a couple of packages to assist in creating this project. 

Both `grpcio` and `grpcio-tools` can be installed with the following:

{% highlight bash %}
pip install grpcio
pip install grpcio-tools
{% endhighlight %}

## Create your definition

Before we begin, we really need a clear idea on how our service will look. This involves creating a contract which will detail the data structures and service definitions that will be utilised between system actors.

To do this, we'll use a `proto` file (in the [protobuf](https://github.com/google/protobuf) format) which we'll use to generate our contract code.

In our application we can *add*, *subtract*, *multiply* and *divide*. This is a stateful service, so we'll be creating sessions to conduct calculations in. A *create* method will create a session, where as the *answer* method will tear our session down, emitting the result.

{% highlight text %}
syntax = "proto3";

message Number {
  float value = 1;
}

message SessionOperation {
  string token = 1;
  float value = 2;
}

service Calculator {
  rpc Create(Number) returns (SessionOperation) { }
  rpc Answer(SessionOperation) returns (Number) { }

  rpc Add(SessionOperation) returns (Number) { }
  rpc Subtract(SessionOperation) returns (Number) { }
  rpc Multiply(SessionOperation) returns (Number) { }
  rpc Divide(SessionOperation) returns (Number) { }
}
{% endhighlight %}

Running this file through `grpc_tools` with the following command:

{% highlight bash %}
python -m grpc_tools.protoc -I. --python_out=. --grpc_python_out=. calc.proto
{% endhighlight %}

We're now left with two automatically generated files, `calc_pb2_grpc.py` and `calc_pb2.py`. These files hold the foundations of value mashalling and service definition for us.

## Implementing the server

Now that we've generated some stubs to get our server running, we need to supply the implementation itself. A class `CalculatorServicer` amongst other artifacts were generated for us. We derive this class to supply our functions out.

{% highlight python %}
class CalculatorServicer(calc_pb2_grpc.CalculatorServicer):

    def Create(self, request, context):
        serial = str(uuid.uuid4())
        calc_db[serial] = request.value

        response = calc_pb2.SessionOperation()
        response.token = serial
        response.value = calc_db[serial]

        return response
{% endhighlight %}

Here's the `Create` implementation. You can see that it's just reserving a piece of the `calc_db` dictionary, and storing the initial value.

`request` is in the shape of the message that we defined for this service. In the case of `Create` the input message is in the type of `Number`. You can see that the `value` attribute is being accessed.

The remainder of the implementation are the arithmetic operations along with the session closure:

{% highlight python %}
    def Answer(self, request, context):
        serial = request.token

        response = calc_pb2.Number()
        response.value = calc_db[serial]

        calc_db[serial] = None

        return response

    def Add(self, request, context):
        value = request.value
        serial = request.token

        calc_db[serial] = calc_db[serial] + value        

        response = calc_pb2.Number()
        response.value = calc_db[serial]
        return response

    def Subtract(self, request, context):
        value = request.value
        serial = request.token

        calc_db[serial] = calc_db[serial] - value        

        response = calc_pb2.Number()
        response.value = calc_db[serial]
        return response

    def Multiply(self, request, context):
        value = request.value
        serial = request.token

        calc_db[serial] = calc_db[serial] * value        

        response = calc_pb2.Number()
        response.value = calc_db[serial]
        return response

    def Divide(self, request, context):
        value = request.value
        serial = request.token

        calc_db[serial] = calc_db[serial] / value        

        response = calc_pb2.Number()
        response.value = calc_db[serial]
        return response
{% endhighlight %}

Finally, we need to start accepting connections.

## Standing the server up

The following code sets up the calculator.

{% highlight python %}
server = grpc.server(futures.ThreadPoolExecutor(max_workers=10))
calc_pb2_grpc.add_CalculatorServicer_to_server(CalculatorServicer(), server)

print('Starting server. Listening on port 3000.')
server.add_insecure_port('[::]:3000')
server.start()

try:
    while True:
        time.sleep(10000)
except KeyboardInterrupt:
    server.stop(0)
{% endhighlight %}

## Invoking the code

Now, we'll create a client to invoke these services.

{% highlight python %}
import grpc

import calc_pb2
import calc_pb2_grpc

channel = grpc.insecure_channel('localhost:3000')

stub = calc_pb2_grpc.CalculatorStub(channel)
initial = calc_pb2.Number(value=0)

session = stub.Create(initial)
print 'Session is ' + session.token

stub.Add(calc_pb2.SessionOperation(token=session.token, value=5))
stub.Subtract(calc_pb2.SessionOperation(token=session.token, value=3))
stub.Multiply(calc_pb2.SessionOperation(token=session.token, value=10))
stub.Divide(calc_pb2.SessionOperation(token=session.token, value=2))

answer = stub.Answer(calc_pb2.SessionOperation(token=session.token, value=0))
print 'Answer is ' + str(answer.value)
{% endhighlight %}

So, we're setting up a session with a value of `0`. We then . . 

* Add `5`
* Subtract `3`
* Multiply by `10`
* Divide by `2`

We should end up with `10`.

{% highlight text %}
➜  remote-calc python calc_client.py
Session is 167aa460-6d14-4ecc-a729-3afb1b99714e
Answer is 10.0
{% endhighlight %}

## Wrapping up

This is a really simple, trivial, well studied (contrived) example of how you'd use this technology. It does demonstrate the ability to offer your python code remotely.

