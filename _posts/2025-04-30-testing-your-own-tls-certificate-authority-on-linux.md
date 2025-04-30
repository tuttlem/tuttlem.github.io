---
layout: post
title: Testing Your Own TLS Certificate Authority on Linux
date: 2025-04-30
comments: false
categories: [ linux, tls, certificates, security, walkthrough ]
---

# Introduction

Sometimes it’s not enough to read about TLS certificates — you want to *own the whole stack*.  

In this post, we’ll walk through creating your own Certificate Authority (CA), issuing your own certificates, trusting 
them at the system level, and standing up a real HTTPS server that uses them.

If you’ve ever wanted to:

- Understand what happens behind the scenes when a certificate is “trusted”
- Build local HTTPS services with real certificates (no self-signed warnings)
- Experiment with mTLS or cert pinning

… this is a great place to start.

This walkthrough is based on [this excellent article by Previnder](https://previnder.com/tls-ca-linux/) — with my own 
notes, commentary, and a working HTTPS demo to round it out.

## Step 1: Create a Root Certificate Authority

[Certificate authorities](https://en.wikipedia.org/wiki/Certificate_authority) are the big "trustworthy" companies that 
issue us certificates. Their *root* certificates are trusted by operating systems, and web browsers so that we don't 
receive trust errors when trying to use them.

From [wikipedia](https://en.wikipedia.org/wiki/Certificate_authority):

> In cryptography, a certificate authority or certification authority (CA) is an entity that stores, signs, and issues digital certificates. A digital certificate certifies the ownership of a public key by the named subject of the certificate.

Here, we're taking the role of the certificate authority. As we'll be creating a root certificate, these are naturally 
self-signed.

```bash
# Generate a private key for your CA
openssl genpkey -algorithm RSA -pkeyopt rsa_keygen_bits:4096 -out rootca.key

# Generate a self-signed certificate
openssl req -x509 -key rootca.key -out rootca.crt -subj "/CN=localhost-ca/O=localhost-ca"
```

You now have a root CA private key (`rootca.key`) and a self-signed root certificate (`rootca.crt`). This is your 
*trusted source of truth* for signing other certificates. This is the key and our certificate for our certificate 
authority that we've called "localhost-ca".

We have now setup our "Root CA" entity. From here, there's a little bit of a handshake that we have to follow in order 
to get our certificate signed by the CA. Here is a basic flow diagram:

```text
+-------------------+                 +-------------------+
|                   |                 |                   |
|     Customer      |                 |        CA         |
|                   |                 |                   |
+--------+----------+                 +---------+---------+
         |                                      |
         | Generate Private Key                 |
         |------------------------------------->|
         |                                      |
         | Create Certificate Signing Request   |
         | (CSR) with Public Key and Details    |
         |------------------------------------->|
         |                                      |
         | CA Verifies CSR Information          |
         | and Signs Certificate                |
         |<-------------------------------------|
         |                                      |
         | Receives Signed Certificate          |
         |------------------------------------->|
         |                                      |
         | Installs Certificate on Server       |
         |------------------------------------->|
         |                                      |
         | Client Connects to Server via HTTPS  |
         |------------------------------------->|
         |                                      |
         | Server Presents Certificate          |
         |------------------------------------->|
         |                                      |
         | Client Verifies Certificate          |
         | Against Trusted CA                   |
         |------------------------------------->|
```

1. *Customer* generates a private key and creates a CSR containing their public key and identifying information.
2. *CA* verifies the CSR details and signs it, issuing a certificate.
3. *Customer* installs the signed certificate on their server.
4. *Client* connects to the server, which presents the certificate.
5. *Client* verifies the certificate against trusted CAs to establish a secure connection.

Let’s move on and actually sign our customer’s certificate.

## Step 2: Create a Certificate-Signing Request (CSR)

We're now acting on behalf of one of our "customers" as the certificate authority. We'll create a private key for our 
"customer's" signed certificate. 

```bash
openssl genpkey -algorithm RSA -pkeyopt rsa_keygen_bits:4096 -out customer.key
```

Now that we have this private key, we'll create a [certificate signing request](https://en.wikipedia.org/wiki/Certificate_signing_request). 
This process is also done by the customer, where the output (a .csr file) is sent to the root authority. In order to do 
this we create a short config file to describe the request.

```ini
;; csr.conf

[req]
distinguished_name = dn
prompt             = no
req_extensions = req_ext

[dn]
CN=localhost

[req_ext]
subjectAltName = @alt_names

[alt_names]
DNS.0 = localhost
```

Under the `[dn]` section, we have a value `CN` which tells the root authority the domain that we want a certificate for.

We now generate the signing request:

```bash
openssl req -new -key customer.key -out customer.csr -config csr.conf
```

{% include callout.html type="warning" title="Note:" text="Be sure the Common Name (CN) matches the domain or hostname you’ll be securing." %}
 
---

## Step 3: Get the Signed Certificate

All that is left now is to process the signing request file (which we were given by our customer). Doing this will 
produce a certificate that we then give back to our customer.

```bash
openssl x509                \
        -req                \
        -days 3650          \
        -extensions req_ext \
        -extfile csr.conf   \
        -CA rootca.crt      \
        -CAkey rootca.key   \
        -in customer.csr    \
        -out customer.crt 
```

You should now have a `customer.crt` certificate that is signed by your own trusted CA. 

We can check these details with the following:

```bash
openssl x509 -in customer.crt -text -noout
```

You should see `localhost-ca` in the "Issuer".

```text
Issuer: CN=localhost-ca, O=localhost-ca
```

## Step 4: Trust Your CA System-Wide

Just because you've done this doesn't mean that anybody (including you) trusts it. In order to get your software to 
trust the certificates that are created signed by your root CA, you need to get them added into the stores of your
computer.

For Debian-based operating systems:

```bash
sudo cp rootca.crt /usr/local/share/ca-certificates/my-root-ca.crt
sudo update-ca-certificates
```

For Arch-based operating systems:

```bash
sudo trust anchor rootca.crt
```

Now your system trusts anything signed by your CA — including your `customer.crt`.

You can confirm:

```bash
openssl verify -CAfile /etc/ssl/certs/ca-certificates.crt customer.crt
```

## Step 5: Spin Up an HTTPS Server

Finally, we can test this all out in a browser by securing a local website using these certificates.

Create a simple Python HTTPS server:

```python
# server.py
import http.server
import ssl

server_address = ('127.0.0.1', 443)
httpd = http.server.HTTPServer(server_address, http.server.SimpleHTTPRequestHandler)

context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
context.load_cert_chain(certfile='customer.crt', keyfile='customer.key')

httpd.socket = context.wrap_socket(httpd.socket, server_side=True)

print("Serving HTTPS on https://127.0.0.1:443")
httpd.serve_forever()
```

When you hit [https://localhost/](https://localhost/) in a browser, you may still see a browser warning if your root 
CA hasn’t been imported into the browser’s own trust store. If so, you may still need to add the rootCA certificate 
into the browser's certificate store.

## Wrap-up

You now control your own Certificate Authority, and you've issued a working TLS cert that browsers and tools can trust.

This kind of setup is great for:

- Local development without certificate warnings
- Internal tools and dashboards
- Testing mTLS, revocation, and more

Your CA key is powerful — guard it carefully. And if you want to go deeper, try adding:

- Client certificate authentication
- Revocation (CRLs or OCSP)
- Using your CA with `nginx`, `Caddy`, or `Docker`

Happy encrypting.
