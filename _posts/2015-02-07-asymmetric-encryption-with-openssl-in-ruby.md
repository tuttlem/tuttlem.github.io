---
layout: post
title: Asymmetric encryption with OpenSSL in Ruby
date: 2015-02-07
comments: false
categories: [ "openssl", "encryption", "ruby" ]
---

[Asymmetric encryption](http://en.wikipedia.org/wiki/Public-key_cryptography) is a category of cryptographic strategies employed to share information between two parties using two separate keys. 

In today's post, I want to show how the encryption flow actually works using some Ruby code. 

### Decisions

Before we can get started, we need to make some decisions regarding the encryption that we're going to use. The two assumptions that I've made up front are about the [key size](http://en.wikipedia.org/wiki/Key_size) and [digest function](http://en.wikipedia.org/wiki/Cryptographic_hash_function). I've stored these assumptions in a hash up front:

{% highlight ruby %}
common = {
	:key_length  => 4096,
	:digest_func => OpenSSL::Digest::SHA256.new
}
{% endhighlight %}

We'll use 4096 bit key lengths and [SHA-256](http://en.wikipedia.org/wiki/SHA-2) as our digest function.

### Parties

First thing that we have to establish is that we have two parties. They both want to send a message to each other that no one else can read. They're both defined in our ruby code as `party_a` and `party_b`.

There's no network separating these parties, so you'll have to use your imagination.

To create a party, I've used the following:

{% highlight ruby %}
def make_party(conf, name)

	# create a public/private key pair for this party
	pair = OpenSSL::PKey::RSA.new(conf[:key_length])

	# extract the public key from the pair
	pub  = OpenSSL::PKey::RSA.new(pair.public_key.to_der)

	{ :keypair => pair, :pubkey => pub, :name => name }

end
{% endhighlight %}

Using the configuration assumptions that we'd declared above, this function will create a key pair, extract the public key and give the party a name (nice for display purposes).

### Processing a message

Next up, we'll prepare a message to send. There's a little trickery that you need to remember here:

* :keypair is private and never seen by the other party
* :pubkey is distributed between the parties

To prove that the message was sent by the originator, the sender generates a signature for the message. This is done by the sender using the sender's private key and the pre-defined digest function:

{% highlight ruby %}
# using the sender's private key, generate a signature for the message
signature = from_party[:keypair].sign(conf[:digest_func], message)
{% endhighlight %}

Using the recipient's public key, the sender will encrypt the plain text:

{% highlight ruby %}
# messages are encrypted (by the sender) using the recipient's public key
encrypted = to_party[:pubkey].public_encrypt(message)
{% endhighlight %}

The recipient can now decrypt the message using their private key:

{% highlight ruby %}
# messages are decrypted (by the recipient) using their private key
decrypted = to_party[:keypair].private_decrypt(encrypted)
{% endhighlight %}

Finally, the recipient can verify that the message is actually from the sender by checking the signature:

{% highlight ruby %}
if from_party[:pubkey].verify(conf[:digest_func], signature, decrypted)
	puts "Verified!"
end
{% endhighlight %}

That's all there is to it.

A full working gist that this article uses code from can be found [here](https://gist.github.com/tuttlem/67659db70931e0fdccd2).