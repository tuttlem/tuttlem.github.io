---
layout: post
title: Writing a Key Value Server in Rust
date: 2025-01-26
comments: false
categories: [ "redis", "rust" ]
---

# Introduction


In today's post, we'll build a simple key value server; but we'll do it in an iterative way. We'll build it up simple 
and then add safety, concurrency, and networking as we go.

# Implementation

Now we'll get started with our iterations. The finished code will be available at the end of this post.

## Baseline

All of our implementations will deal with a `KeyValueStore` `struct`. This `struct` will hold all of the variables that 
we want to keep track of in our server.

{% highlight rust %}
use std::collections::HashMap;

struct KeyValueStore {
    data: HashMap<String, String>,
}
{% endhighlight %}

We define `data` as the in-memory representation of our database. We use `String` keys and store `String` values.

Our implementation is very basic. All we're really doing is shadowing the functionality that `HashMap` provides.

{% highlight rust %}
impl KeyValueStore {
    fn new() -> Self {
        Self {
            data: HashMap::new(),
        }
    }

    fn insert(&mut self, key: String, value: String) {
        self.data.insert(key, value);
    }

    fn get(&self, key: &str) -> Option<&String> {
        self.data.get(key)
    }

    fn delete(&mut self, key: &str) {
        self.data.remove(key);
    }
}
{% endhighlight %}

This is a pretty decent starting point. We can use our `KeyValueStore` in some basic tests:

{% highlight rust %}
fn main() {
    let mut store = KeyValueStore::new();
    store.insert("key1".to_string(), "value1".to_string());
    println!("{:?}", store.get("key1"));
    store.delete("key1");
    println!("{:?}", store.get("key1"));
}
{% endhighlight %}

## Variants

`String` is pretty limiting to store as far as the value side is concerned. We can upgrade this to specifically use 
data types that we will find useful via an `enum`:

{% highlight rust %}
#[derive(Debug, Clone)]
enum Value {
    String(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Binary(Vec<u8>),
    // Add more variants as needed
}
{% endhighlight %}

We can swap out the value side of our `data` member now, too.

{% highlight rust %}
struct KeyValueStore {
    data: HashMap<String, Value>,
}
{% endhighlight %}

The implementation simply swaps the `String` for `Value`:

{% highlight rust %}
impl KeyValueStore {
    fn new() -> Self {
        Self {
            data: HashMap::new(),
        }
    }

    fn insert(&mut self, key: String, value: Value) {
        self.data.insert(key, value);
    }

    fn get(&self, key: &str) -> Option<&Value> {
        self.data.get(key)
    }

    fn delete(&mut self, key: &str) {
        self.data.remove(key);
    }
}
{% endhighlight %}

We're now able to not only store strings. We can store integers, floats, binary, and booleans. This makes our key value 
store a lot more versatile.

## Thread Safety

We will have multiple threads of execution trying to perform actions on this structure at the same time, so we will 
add some thread safety to the process now. Wrapping `data` in `Arc` will give us a thread safe, reference counting 
pointer. We're also going to need to lock this data structure for reading and for writing. We can use `RwLock` to 
take care of that for us.

We update our data structure to include these new types:

{% highlight rust %}
struct KeyValueStore {
    data: Arc<RwLock<HashMap<String, Value>>>,
}
{% endhighlight %}

Now our implementation functions need to change to work with these new structures. We can keep the structure of 
functions the same though.

{% highlight rust %}
impl KeyValueStore {
    fn new() -> Self {
        Self {
            data: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    fn insert(&self, key: String, value: Value) {
        let mut locked = self.data.write().unwrap();
        locked.insert(key, value);
    }

    fn get(&self, key: &str) -> Option<Value> {
        let mut locked = self.data.read().unwrap();
        locked.get(key).cloned()
    }

    fn delete(&self, key: &str) {
        let mut locked = self.data.write().unwrap();
        locked.remove(key);
    }
}
{% endhighlight %}

These functions are now safe, which means calling code can be multithreaded and we can guaranteed that our data 
structure will be treated consistently.

{% highlight rust %}
fn main() {
    let store = Arc::new(KeyValueStore::new());

    // Create a vector to hold thread handles
    let mut handles = vec![];

    // Spawn threads to perform inserts
    for i in 0..5 {
        let store = Arc::clone(&store);
        let handle = thread::spawn(move || {
            let key = format!("key{}", i);
            let value = Value::Integer(i * 10);
            store.insert(key.clone(), value);
            println!("Thread {} inserted: {}", i, key);
        });
        handles.push(handle);
    }

    // Spawn threads to read values
    for i in 0..5 {
        let store = Arc::clone(&store);
        let handle = thread::spawn(move || {
            let key = format!("key{}", i);
            if let Some(value) = store.get(&key) {
                println!("Thread {} read: {} -> {:?}", i, key, value);
            } else {
                println!("Thread {} could not find: {}", i, key);
            }
        });
        handles.push(handle);
    }

    // Spawn threads to delete keys
    for i in 0..5 {
        let store = Arc::clone(&store);
        let handle = thread::spawn(move || {
            let key = format!("key{}", i);
            store.delete(&key);
            println!("Thread {} deleted: {}", i, key);
        });
        handles.push(handle);
    }

    // Wait for all threads to complete
    for handle in handles {
        handle.join().unwrap();
    }

    println!("Final state of the store: {:?}", store.data.read().unwrap());
}
{% endhighlight %}

## Error handling

You can see that we're using `unwrap` in the implementation functions, which might be ok for tests or short scripts. If 
we're going to expect to run this code in production, we'd be best replacing these with actual error handling counterparts.

In order to do that, we need to define our error domain first. We create an enum called `StoreError`. As we fill out 
our implementation, we'll run into a number of different error cases. We'll use `StoreError` to centralise all of these 
errors so we can express them clearly.

{% highlight rust %}
#[derive(Debug)]
enum StoreError {
    LockError(String),
    KeyNotFound(String),
}

impl<T> From<PoisonError<T>> for StoreError {
    fn from(err: PoisonError<T>) -> Self {
        StoreError::LockError(format!("Lock poisoned: {}", err))
    }
}
{% endhighlight %}

We've implemented `PoisonError` for our `StoreError` because the `PoisonError` type is an error which can be returned 
whenever a lock is acquired. If something goes wrong and we've acquired a lock, it's a `PoisonError` that's used.

Our `insert`, `get`, and `delete` methods now need an upgrade. We'll be returning `Result<T, E>` values from our 
functions now to accomodate potential failures.

{% highlight rust %}
fn insert(&self, key: String, value: Value) -> Result<(), StoreError> {
    let mut locked = self.data.write()?;
    locked.insert(key, value);
    Ok(())
}

fn get(&self, key: &str) -> Result<Option<Value>, StoreError> {
    let locked = self.data.read()?;
    Ok(locked.get(key).cloned()) // Clone the value to return an owned copy
}

fn delete(&self, key: &str) -> Result<(), StoreError> {
    let mut locked = self.data.write()?;
    if locked.remove(key).is_none() {
        return Err(StoreError::KeyNotFound(key.to_string()));
    }
    Ok(())
}
{% endhighlight %}

We've removed the use of `unwrap` now, swapping out to using the `?` operator. This will allow us to actually handle 
any failure that is bubbled out of calling code.

## Using the File System

We need to be able to persist the state of our key value store out to disk for durability. In order to do this, we need 
to keep track of where we'll write the file. We add a `file_path` member to our structure:

{% highlight rust %}
struct KeyValueStore {
    data: Arc<RwLock<HashMap<String, Value>>>,
    file_path: Option<String>,
}

impl KeyValueStore {
    fn new(file_path: Option<String>) -> Self {
        Self {
            data: Arc::new(RwLock::new(HashMap::new())),
            file_path,
        }
    }
}
{% endhighlight %}

Starting out this implementation simply, we just write a `load` and `save` function that we can call at any time. Before 
we do this we need some extra dependencies added for serialisation:

{% highlight toml %}
[dependencies]
serde = { version = "1.0.217", features = ["derive"] }
serde_json = "1.0.137"
{% endhighlight %}

This will allow us to reduce our internal state to JSON.

Loading the database off disk 

{% highlight rust %}
/// Load the state from a file
fn load(&self) -> Result<(), StoreError> {
    if let Some(ref path) = self.file_path {
        match fs::read_to_string(path) {
            Ok(contents) => {
                let deserialized: HashMap<String, Value> = serde_json::from_str(&contents)?;
                let mut locked = self.data.write()?;
                *locked = deserialized; // Replace the current state with the loaded one
                Ok(())
            }
            Err(e) if e.kind() == ErrorKind::NotFound => {
                // File doesn't exist, just return Ok (no data to load)
                Ok(())
            }
            Err(e) => Err(e.into()),
        }
    } else {
        Err(StoreError::IoError("File path not set".to_string()))
    }
}
{% endhighlight %}

We need to make sure that a `file_path` was specified. We read everything off from the file into `contents` as a big 
string. Using `serde_json::from_str` we can turn that contents into the deserialised representation. From there, we 
simply swap out the underlying content.

We've got some new errors to deal with here in `IoError`.

{% highlight rust %}
#[derive(Debug)]
enum StoreError {
    LockError(String),
    KeyNotFound(String),
    IoError(String),
    SerdeError(String),
}
{% endhighlight %}

This will be used for our write implementation which looks like this:

{% highlight rust %}
/// Save the current state to a file
fn save(&self) -> Result<(), StoreError> {
    if let Some(ref path) = self.file_path {
        let locked = self.data.read()?;
        let serialized = serde_json::to_string(&*locked)?;
        fs::write(path, serialized)?;
        Ok(())
    } else {
        Err(StoreError::IoError("File path not set".to_string()))
    }
}
{% endhighlight %}

The magic here really is the `serde_json::to_string` taking our internal state and writing it as json.

An example of how this looks is like this:

{% highlight json %}
{
    "key2":{"Integer":20},
    "key4":{"Integer":40},
    "key1":{"Integer":10},
    "key3":{"Integer":30},
    "key0":{"Integer":0}
}
{% endhighlight %}

## Networking

Finally, we'll add some networking to the solution. A really basic network interface will allow remote clients to 
perform the get, set, and delete operations for us.

The `handle_client` function is the heart of the server process, performing the needed processing on incoming requests 
and routing them to the database instance:

{% highlight rust %}
fn handle_client(mut stream: TcpStream, store: Arc<KeyValueStore>) {
    let mut buffer = [0; 512];

    // Read the incoming request
    match stream.read(&mut buffer) {
        Ok(_) => {
            let request = String::from_utf8_lossy(&buffer);
            let mut parts = request.trim().split_whitespace();
            let command = parts.next();

            let response = match command {
                Some("SET") => {
                    let key = parts.next().unwrap_or_default().to_string();
                    let value = parts.next().unwrap_or_default().to_string();
                    store.insert(key, Value::String(value));
                    "OK\n".to_string()
                }
                Some("GET") => {
                    let key = parts.next().unwrap_or_default();
                    if let Ok(Some(value)) = store.get(key) {
                        format!("{:?}\n", value)
                    } else {
                        "Key not found\n".to_string()
                    }
                }
                Some("DEL") => {
                    let key = parts.next().unwrap_or_default();
                    store.delete(key);
                    "OK\n".to_string()
                }
                _ => "Unknown command\n".to_string(),
            };

            // Send the response back to the client
            stream.write_all(response.as_bytes()).unwrap();
        }
        Err(e) => eprintln!("Failed to read from socket: {}", e),
    }
}
{% endhighlight %}

Out networking "protocol" looks like this:

{% highlight text %}
-- set the key "key1" to the value "hello"
SET key1 hello

-- get the value of the key "key1"
GET key1

-- remove the value and key "key1"
DEL key1
{% endhighlight %}

This is all made possible by the following:

{% highlight rust %}
let request = String::from_utf8_lossy(&buffer);
let mut parts = request.trim().split_whitespace();
let command = parts.next();
{% endhighlight %}

We read in the request data from the client into `request`. This gets split up on white spaces into `parts` with `command` 
given the first of these parts. The code is **expecting** `command` to be either `SET`, `GET`, or `DEL` that is then 
handled in the following pattern match.

This function gets mounted onto the server in the main function which now looks like this:

{% highlight rust %}
fn main() {
    let store = Arc::new(
        KeyValueStore::new(None)
    );
    let listener = TcpListener::bind("127.0.0.1:7878").unwrap();

    println!("Server running on 127.0.0.1:7878");

    for stream in listener.incoming() {
        match stream {
            Ok(stream) => {
                let store = Arc::clone(&store);
                std::thread::spawn(move || handle_client(stream, store));
            }
            Err(e) => eprintln!("Connection failed: {}", e),
        }
    }
}
{% endhighlight %}

We're starting our server on port `7878` and handling each connection with our `handle_client` function.

Running this and giving it a test with telnet gives us the following:

{% highlight text %}
➜  telnet 127.0.0.1 7878
Trying 127.0.0.1...
Connected to 127.0.0.1.
Escape character is '^]'.
SET key1 hello
OK
Connection closed by foreign host.

➜  telnet 127.0.0.1 7878
Trying 127.0.0.1...
Connected to 127.0.0.1.
Escape character is '^]'.
GET key1
String("hello")
Connection closed by foreign host.

➜  telnet 127.0.0.1 7878
Trying 127.0.0.1...
Connected to 127.0.0.1.
Escape character is '^]'.
DEL key1
OK
Connection closed by foreign host.

➜  telnet 127.0.0.1 7878
Trying 127.0.0.1...
Connected to 127.0.0.1.
Escape character is '^]'.
GET key1
Key not found
Connection closed by foreign host.
{% endhighlight %}

So, it works. It's crude and needs to be patched to be a little more production ready than this - but this is a start.

# Conclusion

In this article, we walked through building a thread-safe, persistent key-value store in Rust. We started with a simple 
in-memory implementation and iteratively improved it by:

- Adding support for multiple data types using an enum.
- Ensuring thread safety with `RwLock` and `Arc`.
- Replacing `unwrap` with proper error handling.
- Adding file persistence using JSON serialization and deserialization.
- Added some basic network access

This provides a solid foundation for a more robust and scalable key-value server. Next steps could include:
- Implementing advanced features like snapshots or replication.
- Optimizing for performance with tools like async I/O or a custom storage engine.
- Partial reads and memory mapping
- Clustering

The full implementation can be found [here](https://gist.github.com/tuttlem/5654a6a172b8603ff103ad4d51608931).