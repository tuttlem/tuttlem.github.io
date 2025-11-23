---  
layout: post  
title: Creating extensions in Rust for PostgreSQL  
date: 2025-11-23  
comments: false  
categories: [ pg, postgres, sql, rust ]  
---

In a [previous post]({% post_url 2025-01-21-creating-extensions-in-c-for-postgresql %}) I walked through building PostgreSQL extensions in C. It worked, but the process reminded me why systems programming slowly migrated away from raw C for anything larger than a weekend hack. Writing even a trivial function required boilerplate macros, juggling `PG_FUNCTION_ARGS`, and carefully tiptoeing around memory contexts.

This time, we’re going to do the same thing again — but in **Rust**.

Using the [pgrx](https://github.com/pgcentralfoundation/pgrx) framework, you can build fully-native Postgres extensions with:

- no hand-written SQL wrappers  
- no PGXS Makefiles  
- no manual tuple construction  
- no `palloc`/`pfree` memory management  
- a hot-reloading development Postgres  
- and **zero unsafe code** unless you choose to use it

Let’s walk through the entire process: installing pgrx, creating a project, adding a function, and calling it from Postgres.

---

# 1. Installing pgrx

Install the pgrx cargo subcommand:

{% highlight bash %}
cargo install --locked cargo-pgrx
{% endhighlight %}

Before creating an extension, pgrx needs to know which versions of Postgres you want to target.  
Since I’m running PostgreSQL 17, I simply asked pgrx to download and manage its own copy:

{% highlight bash %}
cargo pgrx init --pg17 download
{% endhighlight %}

This is important.

Instead of installing into `/usr/share/postgresql` (which requires root and is generally a bad idea), pgrx keeps everything self-contained under:

`~/.pgrx/17.x/pgrx-install/`

This gives you:

- a private Postgres 17 instance  
- a writable extension directory  
- zero interference with your system Postgres  
- a smooth, reproducible development environment

---

# 2. Creating a New Extension

With pgrx initialised, create a new project:

{% highlight bash %}
cargo pgrx new hello_rustpg
cd hello_rustpg
{% endhighlight %}

This generates a full extension layout:

{% highlight plain %}
Cargo.toml
src/lib.rs
sql/hello_rustpg.sql
hello_rustpg.control
{% endhighlight %}

When you compile the project, pgrx automatically generates SQL wrappers and installs everything into its own Postgres instance.

---

# 3. A Minimal Rust Function

Open `src/lib.rs` and add:

{% highlight rust %}
use pgrx::prelude::*;

pgrx::pg_module_magic!();

#[pg_extern]
fn hello_rustpg() -> &'static str {
    "Hello from Rust + pgrx on Postgres 17!"
}
{% endhighlight %}

That’s all you need.  
pgrx generates the SQL wrapper for you, handles type mapping, and wires everything into Postgres.

---

# 4. Running It Inside Postgres

Start your pgrx-managed Postgres 17 instance:

{% highlight bash %}
cargo pgrx run pg17
{% endhighlight %}

Inside psql:

{% highlight sql %}
CREATE EXTENSION hello_rustpg;
SELECT hello_rustpg();
{% endhighlight %}

Result:

{% highlight text %}
 hello_rustpg            
-------------------------------
 Hello from Rust + pgrx on Postgres 17!
(1 row)
{% endhighlight %}

Done. A working native extension — no Makefiles, no C, no segfaults.

---

# 5. Returning a Table From Rust

Let’s do something a little more interesting: return rows.

Replace your `src/lib.rs` with:

{% highlight rust %}
use pgrx::prelude::*;
use pgrx::spi::SpiResult;

pgrx::pg_module_magic!(name, version);

#[pg_extern]
fn hello_hello_rustpg() -> &'static str {
    "Hello, hello_rustpg"
}

#[pg_extern]
fn list_tables() -> TableIterator<'static, (name!(schema, String), name!(table, String))> {
    let sql = "
        SELECT schemaname::text AS schemaname,
               tablename::text AS tablename
        FROM pg_tables
        WHERE schemaname NOT IN ('pg_catalog', 'information_schema')
        ORDER BY schemaname, tablename;
    ";

    let rows = Spi::connect(|client| {
        client
            .select(sql, None, &[])?
            .map(|row| -> SpiResult<(String, String)> {
                let schema: Option<String> = row["schemaname"].value()?;
                let table: Option<String>  = row["tablename"].value()?;

                Ok((schema.expect("schemaname null"),
                    table.expect("tablename null")))
            })
            .collect::<SpiResult<Vec<_>>>()
    })
    .expect("SPI failed");

    TableIterator::new(rows.into_iter())
}

{% endhighlight %}

Re-run:

{% highlight bash %}
cargo pgrx run pg17
{% endhighlight %}

Then:

{% highlight sql %}
SELECT * FROM list_tables();
{% endhighlight %}

If you don't have any tables, your list will be empty. Otherwise you'll see something like:

{% highlight text %}
 schema |    table    
--------+-------------
 public | names
 public | order_items
 public | orders
 public | users
(4 rows)
{% endhighlight %}

This is the point where Rust starts to feel like cheating:  
you’re returning tuples without touching `TupleDesc`, `heap_form_tuple()`, or any of Postgres’s internal APIs.

---

# 6. Accessing Catalog Metadata (Optional but Fun)

Here’s one more example: listing foreign keys.

{% highlight rust %}
#[pg_extern]
fn list_foreign_keys() -> TableIterator<
    'static,
    (
        name!(table_name, String),
        name!(column_name, String),
        name!(foreign_table_name, String),
        name!(foreign_column_name, String),
    ),
> {
    let sql = r#"
        SELECT
            tc.table_name::text        AS table_name,
            kcu.column_name::text      AS column_name,
            ccu.table_name::text       AS foreign_table_name,
            ccu.column_name::text      AS foreign_column_name
        FROM information_schema.table_constraints AS tc
        JOIN information_schema.key_column_usage AS kcu
            ON tc.constraint_name = kcu.constraint_name
           AND tc.table_schema = kcu.table_schema
        JOIN information_schema.constraint_column_usage AS ccu
            ON ccu.constraint_name = tc.constraint_name
           AND ccu.table_schema = tc.table_schema
        WHERE tc.constraint_type = 'FOREIGN KEY'
        ORDER BY tc.table_name, kcu.column_name;
    "#;

    let rows = Spi::connect(|client| {
        client
            .select(sql, None, &[])?
            .map(|row| -> SpiResult<(String, String, String, String)> {
                let t:  Option<String> = row["table_name"].value()?;
                let c:  Option<String> = row["column_name"].value()?;
                let ft: Option<String> = row["foreign_table_name"].value()?;
                let fc: Option<String> = row["foreign_column_name"].value()?;

                Ok((t.expect("null"), c.expect("null"), ft.expect("null"), fc.expect("null")))
            })
            .collect::<SpiResult<Vec<_>>>()
    })
    .expect("SPI failed");

    TableIterator::new(rows.into_iter())
}
{% endhighlight %}

In psql:

{% highlight sql %}
SELECT * FROM list_foreign_keys();
{% endhighlight %}

Example output:

{% highlight text %}
 table_name  | column_name | foreign_table_name | foreign_column_name 
-------------+-------------+--------------------+---------------------
 order_items | order_id    | orders             | id
 orders      | user_id     | users              | id
(2 rows)
{% endhighlight %}

This begins to show how easy it is to build introspection tools — or even something more adventurous, like treating your relational schema as a graph.

---

# 7. Testing in Rust

pgrx includes a brilliant test harness.

Add this:

{% highlight rust %}
#[cfg(any(test, feature = "pg_test"))]
#[pg_schema]
mod tests {
    use super::*;
    use pgrx::prelude::*;

    #[pg_test]
    fn test_hello_rustpg() {
        assert_eq!(hello_rustpg(), "Hello from Rust + pgrx on Postgres 17!");
    }
}

/// Required by `cargo pgrx test`
#[cfg(test)]
pub mod pg_test {
    pub fn setup(_opts: Vec<&str>) {}
    pub fn postgresql_conf_options() -> Vec<&'static str> { vec![] }
}
{% endhighlight %}

Then run:

{% highlight bash %}
cargo pgrx test pg17
{% endhighlight %}

These are *real* Postgres-backed tests.  
It’s one of the biggest advantages of building extensions in Rust.

---

# Conclusion

After building extensions in both C and Rust, I'm firmly in the Rust + pgrx camp.

You still get:

- full access to Postgres internals  
- native performance  
- the ability to drop into unsafe when needed  

But you also get:

- safety  
- ergonomics  
- powerful testing  
- a private Postgres instance during development  
- drastically simpler code  

In the next article I’ll push further and treat foreign keys as edges — effectively turning a relational schema into a graph.

But for now, this is a clean foundation:  
**a native PostgreSQL extension written in Rust, tested, and running on Postgres 17.**
