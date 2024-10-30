---
layout: post
title: Learning Rust Part 14 - Security and Cryptography
date: 2024-10-30
comments: false
categories: [ "rust" ]
---

# Introduction

Rust’s strong memory safety guarantees and growing ecosystem of security libraries make it an excellent choice for 
building secure applications. From encryption and password hashing to secure communication and cross-compilation for 
secure systems, Rust provides a solid foundation for high-security applications. In this post, we’ll explore key tools 
and techniques for secure application development in Rust.

# Encryption Libraries (e.g., `rust-crypto`, `ring`)

Rust offers a range of encryption libraries, including `rust-crypto` and `ring`, which provide cryptographic algorithms 
like AES, RSA, and SHA-2. These libraries enable secure encryption, decryption, hashing, and digital signatures.

## Using `ring` for Encryption and Hashing

The `ring` library is popular for cryptographic operations in Rust, offering efficiency and ease of use.

## Example: Hashing with SHA-256

{% highlight rust %}
use ring::digest::{Context, SHA256, Digest};

fn sha256_hash(data: &[u8]) -> Digest {
    let mut context = Context::new(&SHA256);
    context.update(data);
    context.finish()
}

fn main() {
    let data = b"Hello, Rust!";
    let hash = sha256_hash(data);
    println!("SHA-256 hash: {:?}", hash);
}
{% endhighlight %}

## Example: AES Encryption and Decryption with `ring`

The `ring` library provides AES-GCM for authenticated encryption, ensuring both confidentiality and data integrity.

{% highlight rust %}
use ring::aead::{AES_256_GCM, SealingKey, UnboundKey, LessSafeKey, Nonce, Aad, OpeningKey};
use ring::rand::{SystemRandom, SecureRandom};

fn aes_encrypt(data: &[u8], key: &[u8]) -> Vec<u8> {
    let unbound_key = UnboundKey::new(&AES_256_GCM, key).unwrap();
    let sealing_key = LessSafeKey::new(unbound_key);
    let nonce = Nonce::assume_unique_for_key([0u8; 12]);
    let mut in_out = data.to_vec();
    sealing_key.seal_in_place_append_tag(nonce, Aad::empty(), &mut in_out).unwrap();
    in_out
}
{% endhighlight %}

# Password Hashing and Secure Storage

Password hashing is crucial for securely storing user passwords. Libraries like `argon2` provide key derivation 
functions (e.g., Argon2, scrypt, bcrypt) that are secure against brute-force attacks.

## Argon2 with `argon2` Library

The `argon2` crate enables secure password hashing, an essential feature for storing user credentials.

{% highlight rust %}
use argon2::{self, Config};

fn hash_password(password: &[u8], salt: &[u8]) -> Vec<u8> {
    let config = Config::default();
    argon2::hash_raw(password, salt, &config).unwrap()
}

fn main() {
    let password = b"super_secret_password";
    let salt = b"random_salt";
    let hash = hash_password(password, salt);
    println!("Password hash: {:?}", hash);
}
{% endhighlight %}

## Secure Storage

Storing sensitive information, like API keys and secrets, securely is essential. You can use encrypted databases or 
dedicated secure storage libraries like `secrecy` to ensure data stays confidential in memory.

# TLS and SSL with `rustls`

For secure communication, Rust provides `rustls`, a TLS library built on `ring`. Unlike C-based libraries like OpenSSL, 
`rustls` is memory-safe and avoids common vulnerabilities.

## Setting up a TLS Server with `rustls`

Using `rustls`, you can build a TLS-enabled server that ensures secure data transmission.

{% highlight rust %}
use rustls::{ServerConfig, NoClientAuth};
use std::sync::Arc;
use tokio_rustls::TlsAcceptor;
use tokio::net::TcpListener;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let certs = load_certs("cert.pem")?;
    let key = load_private_key("key.pem")?;
    let config = ServerConfig::builder()
        .with_safe_defaults()
        .with_no_client_auth()
        .with_single_cert(certs, key)?;

    let listener = TcpListener::bind("127.0.0.1:8443").await?;
    let acceptor = TlsAcceptor::from(Arc::new(config));

    loop {
        let (socket, _) = listener.accept().await?;
        let acceptor = acceptor.clone();
        tokio::spawn(async move {
            let _tls_stream = acceptor.accept(socket).await.unwrap();
            println!("TLS connection established");
        });
    }
}
{% endhighlight %}

In this example, `rustls` is configured with certificates for server authentication, and incoming connections are 
wrapped in TLS for secure communication.

# Cross-compilation for Secure Systems

Cross-compilation allows you to build Rust applications for secure or embedded environments, such as ARM-based systems 
or Linux-based IoT devices. Tools like `rustup` and custom target configurations facilitate cross-compiling Rust code.

## Example: Cross-compiling for ARM

To cross-compile for an ARM-based system (e.g., Raspberry Pi), use `rustup` to install the appropriate target.

{% highlight bash %}
rustup target add armv7-unknown-linux-gnueabihf
cargo build --target armv7-unknown-linux-gnueabihf
{% endhighlight %}

For more secure systems, you can use `musl` as a static linking target, ensuring binary compatibility and reducing 
dependencies.

{% highlight bash %}
rustup target add x86_64-unknown-linux-musl
cargo build --target x86_64-unknown-linux-musl
{% endhighlight %}

# Security Best Practices in Rust

While Rust’s safety guarantees are a strong foundation, additional best practices can further enhance application 
security:

- **Minimize `unsafe` blocks**: Limit the use of `unsafe` code to avoid memory vulnerabilities.
- **Use password hashing for sensitive data**: Store passwords using Argon2, bcrypt, or scrypt, not as plaintext.
- **Leverage strong typing and lifetimes**: Rust’s type system prevents common errors by ensuring proper data handling.
- **Employ secure libraries**: Use libraries like `ring`, `rustls`, and `argon2` rather than implementing cryptographic functions, as custom cryptography is challenging to secure.

## Security Audits and Code Analysis

Rust’s ecosystem includes tools for static analysis and security auditing, such as `cargo-audit`, which checks 
dependencies for known vulnerabilities.

{% highlight bash %}
cargo install cargo-audit
cargo audit
{% endhighlight %}

`cargo-audit` is especially useful for detecting security issues in third-party libraries.

# Secure Memory Management

Rust’s zero-cost abstractions ensure safety without sacrificing performance, which is critical for secure memory 
handling. Libraries like `secrecy` help secure data in memory, preventing leaks and ensuring sensitive data is cleared 
when no longer needed.

## Using `secrecy` for Sensitive Data

The `secrecy` crate provides secure wrappers around sensitive data types, ensuring they’re wiped from memory when 
dropped.

{% highlight rust %}
use secrecy::{Secret, ExposeSecret};

fn main() {
    let password = Secret::new(String::from("super_secret_password"));
    println!("Password: {}", password.expose_secret());
}
{% endhighlight %}

`secrecy` is useful for managing in-memory secrets, ensuring sensitive data is not accidentally leaked.

# Summary

Rust’s security-focused libraries, memory safety guarantees, and secure-by-default principles make it ideal for 
developing cryptographic applications and secure systems. With tools for encryption, password hashing, TLS, 
cross-compilation, and secure memory handling, Rust provides a strong foundation for building secure, high-performance 
applications.
