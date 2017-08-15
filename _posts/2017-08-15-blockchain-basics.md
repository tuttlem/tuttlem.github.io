---
layout: post
title: Blockchain Basics
date: 2017-08-15
comments: false
categories: [ "blockchain", "encryption", "hash", "sha2" ]
---

A [blockchain](https://en.wikipedia.org/wiki/Blockchain) is a linked list of record items that are chained together with hashes. To make it a little more concrete, each subsequent block in a chain contains its predecessors hash as a piece of the information made up to make its own hash.

This forms a strong chain of records that is very difficult to change without re-processing all of the ancestor records.

Each record in the chain typically stores:

* A timestamp
* The actual data for the block
* A reference to the predecessor block

In today's post, I'll try to continue this explanation using an implementation written in C++.

## A simple implementation

It'll be a pretty easy build. We'll need a `block` class, which really does all of the work for us. We'll need a way to `hash` a block in a way that gives us a re-usable string. Finally, we'll put the whole implementation using a `vector`.

## The block

We need a *timestamp*, the actual *data* and the *hash of the predecessor*.

{% highlight cpp %}
class block {
public:
  block(const long int ts, const std::string &data, const std::string &prev_hash)
    : _ts(ts), _data(data), _prev_hash(prev_hash) { }

public:
  const long ts() const   { return _ts; }
  const std::string& data() const { return _data; }
  const std::string& prev_hash() const { return _prev_hash; }

private:
  long _ts;
  std::string _data;
  std::string _prev_hash;
};
{% endhighlight %}

In this class, `_ts` assumes the role of the timestamp; `_data` holds an arbitrary string of our data and `_prev_hash` will be the hex string of the hash from the previous record.

The block needs a way of hashing all of its details to produce a new hash. We'll do this by concatenating all of the data within the block, and running it through a [SHA256](https://en.wikipedia.org/wiki/SHA-2) hasher. I found a really simple implementation [here](https://github.com/okdshin/PicoSHA2).

{% highlight cpp %}
std::string hash(void) const {
  std::stringstream ss;

  ss << _ts 
     << _data 
     << _prev_hash; 

  std::string src = ss.str();
  std::vector<unsigned char> hash(32);
  
  picosha2::hash256(
    src.begin(), 
    src.end(), 
    hash.begin(), 
    hash.end()
  );

  return picosha2::bytes_to_hex_string(
    hash.begin(), hash.end()
  );
}
{% endhighlight %}

`_ts`, `_data` and `_prev_hash` get concatenated and hashed.

Now we need a way to seed a chain, as well as build subsequent blocks. Seeding a list is nothing more than just generating a single block that contains no previous reference:

{% highlight cpp %}
static block create_seed(void) {
  auto temp_ts = std::chrono::system_clock::now().time_since_epoch();

  return block(
    temp_ts.count(),
    "Seed block",
    ""
  );
}
{% endhighlight %}

Really simple. Empty string can be swapped out for `nullptr` should we want to add some more branches to the hasher and change the internal type of `_prev_hash`. This will do for our purposes though.

{% highlight cpp %}
static block create_next(const block &b, const std::string &data) {
  auto temp_ts = std::chrono::system_clock::now().time_since_epoch();

  return block(
    temp_ts.count(),
    data, 
    b.hash()
  );    
}
{% endhighlight %}

The *next* blocks need to be generated _from_ another block; in this case `b`. We use its hash to populate the `_prev_hash` field of the new block.

This is the key part of the design though. With the previous block making in to being a part of the concatenated string that gets hashed into this new block, we form a strong dependency on it. This dependency is what *chains* the records together and makes it very difficult to change.

Finally, we can test out our implementation. I've created a function called `make_data` which just generates a JSON string, ready for the `_data` field to manage. It simply holds 3 random numbers; but you could imagine that this might be imperative data for your business process.

{% highlight cpp %}
int main(int argc, char *argv[]) {

  std::vector<block> chain = { 
    block::create_seed()
  };

  for (int i = 0; i < 5; i ++) {
    // get the last block in the chain
    auto last = chain[chain.size() - 1];

    // create the next block
    chain.push_back(block::create_next(last, make_data()));
  }

  print_chain(chain);

  return 0;
}
{% endhighlight %}

Running this code, we can see that the chains are printed to screen:

{% highlight text %}
index: 0
ts: 1502801929223372929
data: Seed block
this: b468ae4c1a5a0b416162a59ebcdd75922ab011d0cc434c8c408b6507459abd5b
prev: 
-------------------------------------
index: 1
ts: 1502801929223494692
data: { "a": 1804289383,"b": 846930886,"c": 1681692777 }
this: 25d892a8de27890ee057923e784124c9c07161ff340d3d11e5f76b5a865e03af
prev: b468ae4c1a5a0b416162a59ebcdd75922ab011d0cc434c8c408b6507459abd5b
-------------------------------------
index: 2
ts: 1502801929223598810
data: { "a": 1714636915,"b": 1957747793,"c": 424238335 }
this: 8a7d5fe462e71663cccda99f80cce99b25199b82fbefc11c6a3f6c2cc4e985f3
prev: 25d892a8de27890ee057923e784124c9c07161ff340d3d11e5f76b5a865e03af
-------------------------------------
index: 3
ts: 1502801929223720644
data: { "a": 719885386,"b": 1649760492,"c": 596516649 }
this: 0da2de773551a15f6bca003196a02b30312c895dab6835c9ac3434f852eeaa60
prev: 8a7d5fe462e71663cccda99f80cce99b25199b82fbefc11c6a3f6c2cc4e985f3
-------------------------------------
index: 4
ts: 1502801929223837467
data: { "a": 1189641421,"b": 1025202362,"c": 1350490027 }
this: a3709be80f80c24ac6ebc526a9dcec5e2212c03260a2f82f5e9943f762becb6e
prev: 0da2de773551a15f6bca003196a02b30312c895dab6835c9ac3434f852eeaa60
-------------------------------------
index: 5
ts: 1502801929223952445
data: { "a": 783368690,"b": 1102520059,"c": 2044897763 }
this: c1de653540556064b3d01fba21d0a80a07071b19969d3e635ad66eb3db2e6272
prev: a3709be80f80c24ac6ebc526a9dcec5e2212c03260a2f82f5e9943f762becb6e
-------------------------------------
{% endhighlight %}

Note that *index* isn't a member of the class; it just counts while we're iterating over the vector. The real membership here is established through the `_prev_hash`; as discussed above.

## Where to?

Now that the storage mechanism is understood, we can apply [proof-of-work](https://en.bitcoin.it/wiki/Proof_of_work) paradigms to attribute a sense of value to our records. More information on how this has been applied can be read up in the following:

* [Hashcash](https://en.bitcoin.it/wiki/Hashcash)
* [Merkle Trees](https://en.bitcoin.it/wiki/Protocol_documentation#Merkle_Trees)
* [Proof of Burn](https://en.bitcoin.it/wiki/Proof_of_burn)
* [Proof of Work](https://en.bitcoin.it/wiki/Proof_of_work)

The full source code for this article can be found [here](https://gist.github.com/tuttlem/c1d2611ec35f6228fd41a7091111b60d).

