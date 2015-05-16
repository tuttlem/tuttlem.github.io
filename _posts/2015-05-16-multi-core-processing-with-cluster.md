---
layout: post
title: Multi-core processing with Cluster
date: 2015-05-16
comments: false
categories: [ "node", "node.js", "cluster", "multicore" ]
---

[Node.js](https://nodejs.org/) operates in a single thread. In order to get your program to take advantage of all of the cores in your machine, you'll need some extra help. Today's post is about the [cluster](https://nodejs.org/api/cluster.html) module which has been created to solve this very problem.

### What is it?

The [cluster](https://nodejs.org/api/cluster.html) module gives your Node.js application the ability to create new processes that will execute your code. Any children that you spawn will share your server ports so this is an excellent utility for process resilience in network applications.

Using the cluster library, you're given a master and worker relationship in your code. The master has the ability to spawn new workers. From there you can use message passing, IPC, network, etc. to communicate between your workers and the master.

### A simple example

In the following sample, I'll put together a http server that allows a user to kill and create processes. You'll also see the round-robin approach to requests as well as each of the workers sharing the same port.

{% highlight js %}
var cluster = require('cluster'),
    http = require('http'),
    os = require('os');

if (cluster.isMaster) {
    var nWorkers = os.cpus().length;

    console.log('Creating ' + nWorkers + ' workers');

    for (var i = 0; i < nWorkers; i ++) {
        var w = cluster.fork();
        w.on('message', function (msg) {

            console.log(msg);

            if (msg.cmd == 'life') {
                var w = cluster.fork();
                console.log('Just spawned ' + w.process.pid);
            }

        });
    }

    cluster.on('exit', function (worker, code, signal) {
        console.log('Worker ' + worker.process.pid + ' has finished');
    });

} else {
    http.createServer(function (req, res) {

        if (req.url == '/poison') {
            cluster.worker.kill();

            res.writeHead(200);
            res.end('pid is taking poison ' + process.pid);            
        } else if (req.url == '/life') {
            process.send({ cmd: 'life' });

            res.writeHead(200);
            res.end('new pid was requested');            
        } else {            
            res.writeHead(200);
            res.end('pid of this worker is ' + process.pid);
        }

    }).listen(3000);
}
{% endhighlight %}

Measuring `isMaster` and in other cases `isWorker` allows you to place code for both sides of your process. This is <em>like</em> the tradition unix [fork](http://en.wikipedia.org/wiki/Fork_%28system_call%29) process.

We count the number of cpu cores and store that off in `nWorkers`. This is how many workers we'll create. Messages are delivered from the worker using the `send` function. These are then caught and interpreted by the master using the `message` event.

The master will go through the workers in a round-robin fashion (by default) who are all listening on port 3000.

There is plenty more to this API than what's in this example. Check out the documentation for more information.
