---
layout: post
title: Hexagonal Architecture in Rust
date: 2025-08-31
comments: false
categories: [ rust, architecture, hexagonal ]
---

# Introduction

[Hexagonal Architecture](https://en.wikipedia.org/wiki/Hexagonal_architecture_(software)), also known as Ports and 
Adapters, is a compelling design pattern that encourages the decoupling of domain logic from infrastructure concerns. 

In this post, I’ll walk through a Rust project called **`banker`** that adopts this architecture, showing how it helps 
keep domain logic clean, composable, and well-tested.

You can follow along with the full code up in my [GitHub Repository](https://github.com/tuttlem/banker) to get this 
running locally.

# Project Structure

The `banker` project is organized as a set of crates:

{% highlight plain %}
crates/
├── banker-core       # The domain and business logic
├── banker-adapters   # Infrastructure adapters (e.g. in-memory repo)
├── banker-fixtures   # Helpers and test data
└── banker-http       # Web interface via Axum
{% endhighlight %}

Each crate plays a role in isolating logic boundaries:

- **`banker-core`** defines the domain entities, business rules, and traits (ports).
- **`banker-adapters`** implements the ports with concrete infrastructure (like an in-memory repository).
- **`banker-fixtures`** provides test helpers and mock repositories.
- **`banker-http`** exposes an HTTP API with `axum`, calling into the domain via ports.

Structurally, the project flows as follows:

<div class="mermaid">
graph TD
  subgraph Core
    BankService
    AccountRepo[AccountRepo trait]
  end

  subgraph Adapters
    HTTP[HTTP Handler]
    InMemory[InMemoryAccountRepo]
    Fixtures[Fixture Test Repo]
  end

  HTTP -->|calls| BankService
  BankService -->|trait| AccountRepo
  InMemory -->|implements| AccountRepo
  Fixtures -->|implements| AccountRepo

</div>

## Defining the Domain (banker-core)

In Hexagonal Architecture, the domain represents the core of your application—the rules, behaviors, and models that 
define what your system actually does. It’s intentionally isolated from infrastructure concerns like databases or HTTP. 
This separation ensures the business logic remains testable, reusable, and resilient to changes in external technology 
choices.

The `banker-core` crate contains the central business model:

{% highlight rust %}
pub struct AccountId(pub String);

pub struct Account {
    pub id: AccountId,
    pub balance_cents: i64,
}

pub trait AccountRepo {
    fn get(&self, id: &AccountId) -> Result<Option<Account>>;
    fn upsert(&self, account: &Account) -> Result<()>;
}
{% endhighlight %}

The `Bank` service orchestrates operations:

{% highlight rust %}
pub struct Bank<R: AccountRepo> {
    repo: R,
}

impl<R: AccountRepo> Bank<R> {
    pub fn deposit(&self, cmd: Deposit) -> Result<Account, BankError> {
        let mut acct = self.repo.get(&cmd.id)?.ok_or(BankError::NotFound)?;
        acct.balance_cents += cmd.amount_cents;
        self.repo.upsert(&acct)?;
        Ok(acct)
    }
    // ... open and withdraw omitted for brevity
}
{% endhighlight %}

The `Bank` struct acts as the use-case layer, coordinating logic between domain entities and ports.

## Implementing Adapters

In Hexagonal Architecture, adapters are the glue between your domain and the outside world. They translate external 
inputs (like HTTP requests or database queries) into something your domain understands—and vice versa. Adapters 
implement the domain's ports (traits), allowing your application core to remain oblivious to how and where the data 
comes from.

The in-memory repository implements the `AccountRepo` trait and lives in `banker-adapters`:

{% highlight rust %}
pub struct InMemoryAccountRepo {
    inner: Arc<Mutex<HashMap<AccountId, Account>>>,
}

impl AccountRepo for InMemoryAccountRepo {
    fn get(&self, id: &AccountId) -> Result<Option<Account>> {
        Ok(self.inner.lock().unwrap().get(id).cloned())
    }
    fn upsert(&self, account: &Account) -> Result<()> {
        self.inner.lock().unwrap().insert(account.id.clone(), account.clone());
        Ok(())
    }
}
{% endhighlight %}

This adapter is used both in the HTTP interface and in tests.

## Testing via Fixtures

`banker-fixtures` provides helpers to test the domain independently of any infrastructure:

{% highlight rust %}
pub fn deposit(bank: &Bank<impl AccountRepo>, id: &AccountId, amt: i64) -> Account {
    bank.deposit(Deposit { id: id.clone(), amount_cents: amt }).unwrap()
}

#[test]
fn withdrawing_too_much_fails() {
    let bank = Bank::new(InMemRepo::new());
    let id = rand_id("acc");
    open(&bank, &id);
    deposit(&bank, &id, 100);

    let err = bank.withdraw(Withdraw { id, amount_cents: 200 }).unwrap_err();
    assert!(matches!(err, BankError::InsufficientFunds));
}
{% endhighlight %}

## Connecting via Transport

The outermost layer of a hexagonal architecture typically handles transport—the mechanism through which external actors 
interact with the system. In our case, that’s HTTP, implemented using the [axum](https://docs.rs/axum/latest/axum/) framework. This layer invokes domain 
services via the ports defined in banker-core, ensuring the business logic remains insulated from the specifics of web 
handling.

In `banker-http`, we wire up the application for HTTP access using `axum`:

{% highlight rust %}
#[tokio::main]
async fn main() -> Result<()> {
    let state = AppState {
        bank: Arc::new(Bank::new(InMemoryAccountRepo::new())),
    };
    let app = Router::new()
        .route("/open", post(open))
        .route("/deposit", post(deposit))
        .route("/withdraw", post(withdraw))
        .with_state(state);
    axum::serve(tokio::net::TcpListener::bind("127.0.0.1:8080").await?, app).await?;
    Ok(())
}
{% endhighlight %}

Each handler invokes domain logic through the `Bank` service, returning simple JSON responses.

This is one example of a primary adapter—other adapters (e.g., CLI, gRPC) could be swapped in without changing the core.

## Takeaways

- Traits in Rust are a perfect match for defining **ports**.
- Structs implementing those traits become **adapters**—testable and swappable.
- The core domain crate (`banker-core`) has no dependencies on infrastructure or `axum`.
- Tests can exercise the domain logic via fixtures and in-memory mocks.

Hexagonal Architecture in Rust isn’t just theoretical—it’s ergonomic. With traits, lifetimes, and ownership semantics, 
you can cleanly separate concerns while still writing expressive, high-performance code.
