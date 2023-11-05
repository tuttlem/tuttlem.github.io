---
layout: post
title: systemd Services and Sockets
date: 2023-11-05
comments: false
categories: [ "" ]
---

### Introduction

[systemd](https://systemd.io/) is a set of basic tools that any system can use to build more sophisticated service applications. Using these building you can
create [units](https://www.freedesktop.org/software/systemd/man/latest/systemd.unit.html) which can be a:

* service
* socket
* device
* mount
* automount
* swap
* target
* path
* timer
* slice
* scope

In today's article, we'll go through an example that uses `service` and `socket` to build a simple server.

### Hello, World

To start with, let's create a "Hello, World" service that will do nothing more than take your connection and send you back the string `"Hello, world"`. First we define our service in a file. Ours is `hw.service`. 

You can install this at `~/.config/systemd/user/hw.service`.

{% highlight text %}
# hw.service
[Unit]
Description=Hello World Service
After=network.target hw.socket
Requires=hw.socket

[Service]
Type=simple
ExecStart=/usr/bin/python3 %h/tmp/serve.py
TimeoutStopSec=5

[Install]
WantedBy=default.target
{% endhighlight %}

The `ExecStart` holds what `systemd` will hand the socket connection off to. In this case, we're going to hand the connection off to a `python` socket server running from our `~/tmp` directory.

You can see that our requires `hw.socket`. It needs to be up before it will respond to requests. You install this one at `~/.config/systemd/user/hw.socket`.

{% highlight text %}
# hw.socket
[Unit]
Description=Hello World Socket
PartOf=hw.service

[Socket]
ListenStream=127.0.0.1:7777

[Install]
WantedBy=sockets.target
{% endhighlight %}

Our socket will listen on `7777` waiting for connections.

The `serve.py` mentioned in the service file is what `systemd` will hand the connection off to. The implementation of that server is a simple socket server:

{% highlight python %}
#!/usr/bin/env python3

from socketserver import TCPServer, StreamRequestHandler
import socket
import logging

class Handler(StreamRequestHandler):
    def handle(self):
        self.data = self.rfile.readline().strip()
        logging.info("From <%s>: %s" % (self.client_address, self.data))
        self.wfile.write("Hello, world!\r\n".encode("utf-8"))

class Server(TCPServer):
    
    # The constant would be better initialized by a systemd module
    SYSTEMD_FIRST_SOCKET_FD = 3

    def __init__(self, server_address, handler_cls):
        # ignore the bind/listen steps
        TCPServer.__init__(
            self, server_address, handler_cls, bind_and_activate=False
        )
        
        # take the socket from systemd
        self.socket = socket.fromfd(
            self.SYSTEMD_FIRST_SOCKET_FD, self.address_family, self.socket_type
        )

if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)

		# host and port values are ignored here  
    server = Server(("localhost", 9999), Handler)
    server.serve_forever()
{% endhighlight %}

Inside of the `Handler` class, in the constructor you can see that we avoid the `bind` and `listen` steps. This is because `systemd` has already done this for us. We're just
going to be handed a file descriptor with the socket already attached.

{% highlight python %}
# ignore the bind/listen steps
TCPServer.__init__(
    self, server_address, handler_cls, bind_and_activate=False
)

# take the socket from systemd
self.socket = socket.fromfd(
    self.SYSTEMD_FIRST_SOCKET_FD, self.address_family, self.socket_type
)
{% endhighlight %}

That's exactly what's happening with `fromfd` here. We're given a socket to work with via descriptor `3`.

The actual implementation of our handler is not doing much more than taking in the request data, and sending back `"Hello World"`.

{% highlight python %}
def handle(self):
    self.data = self.rfile.readline().strip()
    logging.info("From <%s>: %s" % (self.client_address, self.data))
    self.wfile.write("Hello, world!\r\n".encode("utf-8"))
{% endhighlight %}


### Getting it installed

You can start your server listening with the following now:

{% highlight bash %}
systemctl --user daemon-reload
systemctl --user start hw.socket
{% endhighlight %}

You should be up and running.

### Testing

You can use `telnet` to take a look at your server:

{% highlight text %}
➜  ~ telnet 127.0.0.1 7777
Trying 127.0.0.1...
Connected to 127.0.0.1.
Escape character is '^]'.
Hello
Hello, world!
Connection closed by foreign host.
{% endhighlight %}

Alternatively, you can just use `netcat`:

{% highlight text %}
➜  ~ echo 'test' | netcat 127.0.0.1 7777 
Hello, world!
{% endhighlight %}

### Check that it's working

After you've tested it a few times, you'll be albe to see requests in the logs.

{% highlight bash %}
journalctl -f --user-unit hw.service
{% endhighlight %}

You should see the lines from the `logging.info` calls.

{% highlight text %}
Nov 05 18:23:15 ditto systemd[2245]: Started Hello World Service.
Nov 05 18:23:18 ditto python3[9883]: INFO:root:From <('127.0.0.1', 38292)>: b'Hello'
{% endhighlight %}

### Cleaning up

Once you're done and you'd like to remove these, simply stop the service, remove the units, and reload.

{% highlight bash %}
# stop the service
systemctl --user stop hw.socket

# remove the service and socket
rm ~/.config/systemd/user/hw.*

# reload systemd state
systemctl --user daemon-reload 
{% endhighlight %}


