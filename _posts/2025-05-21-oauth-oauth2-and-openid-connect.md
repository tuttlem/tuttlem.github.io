---
layout: post
title: Untangling OAuth, OAuth2, and OpenID Connect
date: 2025-05-21
comments: false
categories: [ web, security, identity, oauth, authentication ]
---

# Introduction

Authentication and authorization power almost everything we do online — but these words are thrown around so much, 
they're often misunderstood. Add in terms like **OAuth2**, **OpenID Connect**, **tokens**, **flows**, and even 
**FAPI**, and suddenly you're in acronym soup.

This post is here to *untangle the mess*.

We'll walk through the big ideas behind OAuth and OpenID Connect, introduce the core **roles** and **flows**, and build 
a set of intuitive examples you can base your mental model on. By the end, you'll know:

- The difference between **authentication** and **authorization**
- What OAuth2 actually does (and what it doesn’t)
- The roles: **Resource Owner**, **Client**, **Authorization Server**, **Resource Server**
- The different flows — and when to use each
- How OpenID Connect builds login flows on top of OAuth2

We won't cover OAuth in this article. OAuth as a concept has been around since 2007. The original version — 
**OAuth 1.0a** — solved the problem of granting third-party access to user data without passwords, but it required 
complex cryptographic signing and didn’t assume HTTPS. OAuth2 replaced it with a cleaner, TLS-based approach that’s now 
the foundation for everything from “Login with Google” to Open Banking APIs.

# Authorization vs Authentication

Let’s get the definitions straight first:

- **Authentication** = Who are you?
- **Authorization** = What are you allowed to do?

Think of a hotel:

- Showing your ID at the front desk = **authentication**
- Being given a keycard for your room = **authorization**

[OAuth2](https://oauth.net/2/) was designed for **authorization**, not login. But because it passes identity-ish tokens around, people started 
using it for login flows — which is what [OpenID Connect](https://openid.net/specs/openid-connect-core-1_0.html) was built to formalize.

# OAuth2 Roles

OAuth2 involves four key actors:

| Role                | Description                                                                 |
|---------------------|-----------------------------------------------------------------------------|
| **Resource Owner**  | The user who owns the data or resource                                      |
| **Client**          | The app that wants to use the resource                                      |
| **Authorization Server** | The service that authenticates the user and issues tokens               |
| **Resource Server** | The API or service holding the protected resource                          |

Example:
- You're the **Resource Owner** - you *own* your GitHub profile
- GitHub is the **Authorization Server**
- A third-party app (like VSCode) is the **Client**
- GitHub’s API is the **Resource Server**

These roles are who will be playing different parts when we go to explain the OAuth2 flows in the next section.

# OAuth2 Flows

OAuth2 defines several **flows**, depending on the type of client and security model.

## Authorization Code Flow

**Used when** 
* Your client is a web app executing on the server-side 
* Your client is a mobile apps and this can be used with [PKCE](https://oauth.net/2/pkce/)

**Steps:**
1. Client sends user to Authorization Server’s **authorize endpoint** (typically a browser redirect)
2. User logs in, approves scopes
3. Server redirects back to client with a **code**
4. Client sends the **code** (plus credentials) to **token endpoint**
5. Client receives **access token**, optionally **refresh token**

<div class="mermaid">
sequenceDiagram
participant User
participant Client
participant AuthServer as Authorization Server

    User->>Client: (1a) Initiates login
    Client->>AuthServer: (1b) Redirect user to authorize endpoint
    User->>AuthServer: (2) Login + Consent
    AuthServer-->>Client: (3) Redirect with Authorization Code
    Client->>AuthServer: (4) Exchange Code (+ Verifier)
    AuthServer-->>Client: (5) Access Token (+ Refresh Token)
</div>

**Why it’s good:** 
* Keeps tokens off the front-end, as the access token is passed directly to the server hosting the client
* Supports refresh tokens  

* **Use with PKCE for mobile/SPAs**


## Client Credentials Flow

**Used when:**
* The client is the resource owner
* Machine-to-machine access (no user)
* Server-side automation, microservices, etc.

**Steps:**
1. Client authenticates to the token endpoint directly
2. Sends its **client ID and secret**
3. Gets an **access token**
4. Client now accesses protected resource

<div class="mermaid">
sequenceDiagram
    participant Client
    participant AuthServer as Authorization Server
    participant Resource as Resource Server

    Client->>AuthServer: (1) Authenticate with client_id + secret
    AuthServer-->>Client: (2) Access Token
    Client->>Resource: (3) API call with token
    Resource-->>Client: (4) Protected resource
</div>

Use this in situations where there is **no user involved.** 

## Resource Owner Password Credentials (ROPC) Flow

**Used when:** 
* The client is completely trusted with user credential
* Really only for legacy apps  

**Should you use it?** No. Never. It's deprecated.

**Steps:**
1. User gives username and password directly to client
2. Client sends them to token endpoint
3. Gets access token

<div class="mermaid">
sequenceDiagram
    participant User
    participant Client
    participant AuthServer as Authorization Server

    User->>Client: (1) Provide username + password
    Client->>AuthServer: (2) Forward credentials
    AuthServer-->>Client: (3) Access Token
</div>

**Why it's bad:** 
* Client sees the user’s password. 

{% include callout.html type="warning" title="Warning:" text="Don't do this anymore." %}

## Device Authorization Flow

**Used when:**
* The client is a Smart TV or console
* The client is CLI tools

**Steps:**
1. Client requests a device + user code from token endpoint
2. Device shows the **user code** and asks user to visit a URL
3. User logs in on their phone/laptop
4. Client polls the token endpoint until authorized
5. Gets access token

<div class="mermaid">
sequenceDiagram
    participant Client
    participant User
    participant AuthServer as Authorization Server

    Client->>AuthServer: (1) Request device_code + user_code
    AuthServer-->>Client: (2) Return codes
    Client->>User: (2b) Display code + URL
    User->>AuthServer: (3) Log in + consent on separate device
    Client->>AuthServer: (4) Poll token endpoint
    AuthServer-->>Client: (5) Access Token
</div>

**No browser on the device needed!** 

Common on Xbox, Apple TV, etc.

## PKCE – Proof Key for Code Exchange

Originally designed for mobile apps, **PKCE** (pronounced “pixy”) adds extra safety to the Authorization Code Flow.

**Why it matters:**
- Public clients can’t hold secrets
- PKCE protects the code exchange from being hijacked

**How it works:**
1. Client generates a random `code_verifier`
2. Derives a `code_challenge = SHA256(code_verifier)`
3. Sends the `code_challenge` with the initial authorize request
4. Exchanges the code using the original `code_verifier`

<div class="mermaid">
sequenceDiagram
    participant Client

    Client->>Client: (1) Generate code_verifier
    Client->>Client: (2) Derive code_challenge = SHA256(code_verifier)
    Client->>AuthServer: (3) Send code_challenge with auth request
    Client->>AuthServer: (4) Exchange code + code_verifier at token endpoint
</div>


**Required in:** All public clients, including SPAs and mobile apps

## Hybrid Flow (OIDC-specific)

**Used when:** 
* Apps that want both `id_token` and `code` at once

**Combines:**
- Immediate authentication (`id_token`)
- Deferred authorization (`code → access_token`)

An example of this is when a login page that needs to show the user’s name immediately, but still needs a backend 
exchange for secure API calls

# OpenID Connect

OAuth2 doesn't handle identity. That’s where **OpenID Connect (OIDC)** steps in. It’s a layer on top of OAuth2 that 
turns it into a proper login protocol.

OIDC adds:

- `id_token`: A JWT that proves who the user is
- `userinfo endpoint`: For extra user profile data
- `openid` scope: Triggers identity behavior
- `/.well-known/openid-configuration`: A discovery doc

**How it works (OpenID Connect Flow):**
1. Client redirects to authorization server with `response_type=code&scope=openid`
2. User logs in and approves
3. Server returns `code`
4. Client exchanges `code` for:
    - `access_token`
    - `id_token`
5. Client validates `id_token` (aud, iss, exp, sig)

<div class="mermaid">
sequenceDiagram
    participant User
    participant Client
    participant AuthServer as Authorization Server

    Client->>AuthServer: (1) Redirect with response_type=code&scope=openid
    User->>AuthServer: (2) Log in + consent
    AuthServer-->>Client: (3) Authorization Code
    Client->>AuthServer: (4) Exchange code
    AuthServer-->>Client: (4b) id_token + access_token
    Client->>Client: (5) Validate id_token (aud, iss, exp, sig)
</div>


You now know **who the user is** and **can access their resources**.



# Pitfalls and Best Practices

## Do

- Always use PKCE (mandatory for public clients)
- Use short-lived access tokens and refresh tokens
- Validate all tokens — especially `id_token`
- Never store tokens in localStorage

## Don't

- Don’t use implicit flow anymore
- Don’t mix up `access_token` and `id_token`

# Conclusion

OAuth2 and OpenID Connect underpin almost every secure app on the internet — but they aren’t simple. They describe a 
flexible framework, not a single implementation, and that’s why they feel confusing.

If you want more information, here are some helpful links.

- [OAuth2.1 (Draft)](https://oauth.net/2.1/) – Cleaner, modernized spec
- [openid.net/specs](https://openid.net/specs/) – OpenID Connect specs
- [jwt.io](https://jwt.io/) – Inspect JWTs
- [oauthdebugger.com](https://oauthdebugger.com/) – Play with flows
- [Auth0 playground](https://auth0.com/docs/flows/concepts/auth-code-pkce) – Try PKCE interactively

