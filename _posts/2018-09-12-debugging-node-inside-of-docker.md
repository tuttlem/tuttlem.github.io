---
layout: post
title: Debugging node inside of Docker
date: 2018-09-12
comments: false
categories: [ "node", "debug" ]
---

If your development setup is anything like mine, you'll like to put all of your applications into their own containers so that they're isolated from each other. This also gives me a little added guarantee that all of an application's dependencies are wrapped up nicely before moving the code between environments.

Sometimes, debugging can be a little awkward if this is how you run. In today's post, I'll take you through debugging your node apps inside of a container.

### Execution

The execution environment is quite simple. We'll assume that a bash script allows us to start a container which holds our application, and injects any instruction to its console:

{% highlight bash %}
docker run --rm -ti \
       -v $(pwd):/usr/src/app \
       -w /usr/src/app \
       -p 3000:3000 \
       -p 9229:9229 \
       node \
       $@
{% endhighlight %}

We'll assume the following:

* Our application serves over port 3000
* Debugging will run on port 9229
* Our application gets mounted to `/usr/src/app` inside the container

### Allowing inspection

Now we need to tell our node process that we want to inspect the process, and allow debugging. This is as simple as using the `--inspect` switch with your `node` or in my case `nodemon` invocations. Here is my `debug` run script inside of my `package.json`:

{% highlight text %}
"debug": "node_modules/.bin/nodemon --inspect=0.0.0.0:9229 index.js",
{% endhighlight %}

This starts execution, mounting the debug port on `9229` (to align with our docker invocation); it's also allowing connections from any remote computer to perform debugging. Handy.

### Start debugging

Once you've issued `./run npm run debug` at the console, you're ready to start debugging.

I use [WebStorm](https://www.jetbrains.com/webstorm/) for some projects, [vim](https://www.vim.org) for others; and sometimes will use [Chrome Dev Tools](https://developers.google.com/web/tools/chrome-devtools/) with `chrome://inspect` to be able to see debugging information on screen.

Hope this helps you keep everything isolated; but integrated enough to debug!

