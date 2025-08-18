---
layout: post
title: Create your own Filesystem with FUSE
date: 2025-08-18
comments: false
categories: [ fuse, curl, rest ]
---

# Introduction

[FUSE](https://www.kernel.org/doc/html/next/filesystems/fuse.html) is a powerful Linux kernel module that lets you implement your own filesystems entirely in user space. No 
kernel hacking required. With it, building your own virtual filesystem becomes surprisingly achievable and even... fun.

In today's article, we'll build a filesystem that's powered entirely by [HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP). Every file operation — reading a 
file, listing a directory, even getting file metadata — will be handled by a [REST](https://en.wikipedia.org/wiki/REST) API. On the client side, we'll 
use [libcurl](https://curl.se/libcurl/) to perform HTTP calls from C, and on the server side, a simple Python [Flask](https://flask.palletsprojects.com/en/stable/) app will serve as our 
in-memory file store.

Along the way, you'll learn how to:

* Use FUSE to handle filesystem operations in user space
* Make REST calls from C using libcurl
* Create a minimal RESTful backend for serving file content
* Mount and interact with your filesystem like any other directory

Up in [my github repository](https://github.com/tuttlem) I have added this project if you'd like to pull it down and try 
it. It's called [restfs](https://github.com/tuttlem/restfs).

Let's get into it.

# Defining a FUSE Filesystem

Every FUSE-based filesystem starts with a `fuse_operations` struct. This is essentially a table of function pointers — 
you provide implementations for the operations you want your filesystem to support.

Here’s the one used in `restfs`:

{% highlight c %}
static struct fuse_operations restfs_ops = {
    .getattr = restfs_getattr,
    .readdir = restfs_readdir,
    .open    = restfs_open,
    .read    = restfs_read
};
{% endhighlight %}

This tells FUSE: “When someone calls `stat()` on a file, use `restfs_getattr`. When they list a directory, use 
`restfs_readdir`, and so on.”

Let’s break these down:

- **`getattr`**: Fills in a `struct stat` with metadata about a file or directory — size, mode, timestamps, etc. It’s the equivalent of `stat(2)`.
- **`readdir`**: Lists the contents of a directory. It’s how `ls` knows what to show.
- **`open`**: Verifies that a file can be opened. You don’t need to return a file descriptor — just confirm the file exists and is readable.
- **`read`**: Reads data from a file into a buffer. This is where the real I/O happens.

Each function corresponds to a familiar POSIX operation. For this demo, we’re implementing just the basics — enough to 
mount the FS, `ls` it, and `cat` a file.

If you leave an operation out, FUSE assumes it’s unsupported — for example, we haven’t implemented `write`, `mkdir`, 
or `unlink`, so the filesystem will be effectively read-only.

# Making REST Calls from C with libcurl

To interact with our HTTP-based server, we use `libcurl`, a powerful and flexible HTTP client library for C. In 
`restfs`, we wrap `libcurl` in a helper function called `http_io()` that performs an HTTP request and returns a parsed 
response object.

Here’s the core of the function:

{% highlight c %}
struct _rest_response* http_io(const char *url, const char *body, const char *type) {
   CURL *curl = NULL;
   CURLcode res;
   long status = 0L;

   struct _http_write_buffer buf;
   buf.data = malloc(1);
   buf.size = 0;

   curl = curl_easy_init();

   if (curl) {
      curl_easy_setopt(curl, CURLOPT_URL, url);
      curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, type);

      if (body) {
         curl_easy_setopt(curl, CURLOPT_POSTFIELDS, body);
         curl_easy_setopt(curl, CURLOPT_POSTFIELDSIZE, strlen(body));
      }

      curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, http_write_callback);
      curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)&buf);

      curl_easy_setopt(curl, CURLOPT_USERAGENT, _http_user_agent);

      struct curl_slist *headers = NULL;
      headers = curl_slist_append(headers, "Content-Type: application/json");
      curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);

      res = curl_easy_perform(curl);
      curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &status);
      curl_easy_cleanup(curl);
      curl_slist_free_all(headers);

      if (res != CURLE_OK) {
         fprintf(stderr, "error: %s\n", curl_easy_strerror(res));
         if (buf.data) free(buf.data);
         return NULL;
      }
   }

   return rest_make_response(buf.data, buf.size, status);
}
{% endhighlight %}

Let’s break it down:

- `curl_easy_init()` creates a new easy handle.
- `CURLOPT_URL` sets the request URL.
- `CURLOPT_CUSTOMREQUEST` lets us specify GET, POST, PUT, DELETE, etc.
- If a body is provided (e.g. for POST/PUT), we pass it in using `CURLOPT_POSTFIELDS`.
- `CURLOPT_WRITEFUNCTION` and `CURLOPT_WRITEDATA` capture the server’s response into a buffer.
- Headers are added manually to indicate we're sending/expecting JSON.
- After the call, we extract the HTTP status code and clean up.

The result is returned as a `_rest_response` struct:

{% highlight c %}
struct _rest_response {
   int status;
   json_object *json;
   char *data;     // raw response body
   size_t length;  // response size in bytes
};
{% endhighlight %}

This makes it easy to access either the full raw data or a parsed JSON object depending on the use case.

To parse the JSON responses from the server, we use the [`json-c`](https://github.com/json-c/json-c) library — a 
lightweight and widely used C library for working with JSON data. This allows us to easily extract fields like 
`st_mode`, `st_size`, or timestamps directly from the server's responses.

To simplify calling common HTTP methods, we define a few handy macros:

{% highlight c %}
#define rest_get(uri)         http_io(uri, NULL, "GET")
#define rest_delete(uri)      http_io(uri, NULL, "DELETE")
#define rest_post(uri, body)  http_io(uri, body, "POST")
#define rest_put(uri, body)   http_io(uri, body, "PUT")
{% endhighlight %}

With these in place, calling a REST endpoint is as simple as:

{% highlight c %}
struct _rest_response *res = rest_get("/getattr?path=/hello.txt");
{% endhighlight %}

This layer abstracts away the curl boilerplate so each FUSE handler can focus on interpreting the result.

# The Backend

So far we’ve focused on the FUSE client — how file operations are translated into HTTP requests. But for the system to 
work, we need something on the other side of the wire to respond.

Enter: a minimal Python server built with Flask.

This server acts as a fake in-memory filesystem. It knows nothing about actual disk files — it just stores a few 
predefined paths and returns metadata and file contents in response to requests.

Let’s look at the key parts:

- A Python dictionary (`fs`) holds a small set of files and their byte contents.
- The `/getattr` endpoint returns a JSON version of `struct stat` for a given file path.
- The `/readdir` endpoint lists all available files (we only support the root directory).
- The `/read` endpoint returns a slice of the file contents, based on `offset` and `size`.

Here’s a simplified version of the server:

{% highlight python %}
from flask import Flask, request, jsonify
from urllib.parse import unquote
import os, stat, time

app = Flask(__name__)
fs = { '/hello.txt': b"Hello, RESTFS!\\n" }

def now(): return { "tv_sec": int(time.time()), "tv_nsec": 0 }

@app.route('/getattr')
def getattr():
    path = unquote(request.args.get('path', ''))
    if path == "/":
        return jsonify({ "st_mode": stat.S_IFDIR | 0o755, ... })
    if path in fs:
        return jsonify({ "st_mode": stat.S_IFREG | 0o644, "st_size": len(fs[path]), ... })
    return ('Not Found', 404)

@app.route('/readdir')
def readdir():
    return jsonify([name[1:] for name in fs.keys()])  # ['hello.txt']

@app.route('/read')
def read():
    path = request.args.get('path')
    offset = int(request.args.get('offset', 0))
    size = int(request.args.get('size', 4096))
    return fs[path][offset:offset+size]
{% endhighlight %}

This is enough to make `ls` and `cat` work on the mounted filesystem. The client calls `getattr` and `readdir` to 
explore the directory, and uses `read` to pull down bytes from the file.

## End to End

With the server running and the client compiled, we can now bring it all together.

Start the Flask server in one terminal:

{% highlight plain %}
python server.py
{% endhighlight %}

Then, in another terminal, create a mountpoint and run the `restfs` client:

{% highlight plain %}
mkdir /tmp/restmnt
./restfs --base http://localhost:5000/ /tmp/restmnt -f
{% endhighlight %}

Now try interacting with your mounted filesystem just like any other directory:

{% highlight plain %}
➜  restmnt ls -l
total 1
-rw-r--r-- 1 michael michael  6 Jan  1  1970 data.bin
-rw-r--r-- 1 michael michael 15 Jan  1  1970 hello.txt

➜  restmnt cat hello.txt
Hello, RESTFS!
{% endhighlight %}

You should see logs from the server indicating incoming requests:

{% highlight plain %}
[GETATTR] path=/
127.0.0.1 - - [18/Aug/2025 21:29:46] "GET /getattr?path=/ HTTP/1.1" 200 -
[READDIR] path=/
127.0.0.1 - - [18/Aug/2025 21:29:46] "GET /readdir?path=/ HTTP/1.1" 200 -
[GETATTR] path=/hello.txt
127.0.0.1 - - [18/Aug/2025 21:29:46] "GET /getattr?path=/hello.txt HTTP/1.1" 200 -
127.0.0.1 - - [18/Aug/2025 21:29:47] "GET /open?path=/hello.txt HTTP/1.1" 200 -
127.0.0.1 - - [18/Aug/2025 21:29:47] "GET /read?path=/hello.txt&offset=0&size=4096 HTTP/1.1" 200 -
[GETATTR] path=/
127.0.0.1 - - [18/Aug/2025 21:29:47] "GET /getattr?path=/ HTTP/1.1" 200 -
{% endhighlight %}

Under the hood, every file operation is being translated into a REST call, logged by the Flask server, and fulfilled 
by your in-memory dictionary.

This is where the whole thing becomes delightfully real — you've mounted an HTTP API as if it were a native part of 
your filesystem.

# Conclusion

`restfs` is a fun and minimal example of what FUSE can unlock — filesystems that aren't really filesystems at all. 
Instead of reading from disk, we’re routing every file operation over HTTP, backed by a tiny REST server.

While this project is intentionally lightweight and a bit absurd, the underlying ideas are surprisingly practical. 
FUSE is widely used for things like encrypted filesystems, network mounts, and user-space views over application state. 
And `libcurl` remains a workhorse for robust HTTP communication in C programs.

What you've seen here is just the start. You could extend `restfs` to support writing files, persisting data to disk, 
mounting a remote object store, or even representing entirely virtual data (like logs, metrics, or debug views).

Sometimes the best way to understand a system is to reinvent it — badly, on purpose.