---
layout: post
title: A Practical Guide to Compiler Phases
date: 2025-06-03
comments: false
categories: [ "" ]
---

# Introduction

Compilers often sound mysterious — full of dragons, jargon, and intimidating diagrams. But under the hood, they all 
follow the same basic blueprint.

In this post, we’ll walk through every major **compiler phase** by implementing them in Rust. We'll start with a raw 
source string and end with real output — step by step — using this ultra-simple language:

{% highlight text %}
let x = 1 + 2;
{% endhighlight %}

We’ll transform that line through tokenization, parsing, type checking, intermediate representation, optimization, 
and finally evaluation — all in plain Rust. Every phase will be practical, with real code you can run. By the end, 
you’ll have a minimal but complete compiler front-end — a solid foundation you can build on.

Here’s a high-level view of the journey:

<div class="mermaid">
graph TD
    A["Source Code"] --> B["Lexical Analysis<br>(Tokenizer)"]
    B --> C["Parsing<br>(AST Builder)"]
    C --> D["Semantic Analysis<br>(Type Checking)"]
    D --> E["IR Generation<br>(Lowering to Intermediate Form)"]
    E --> F["Optimization<br>(Constant Folding, etc.)"]
    F --> G["Code Generation<br>(Assembly or Interpreter)"]
    G --> H["Final Output<br>(Binary or Result)"]
</div>

Let’s dive in!

# Lexical Analysis

Lexical analysis — also called **tokenization** or **scanning** — is the first phase of a compiler. It takes a stream 
of raw characters (your source code) and breaks it into **tokens**: the smallest meaningful units of the language.

Tokens include things like keywords (`let`), identifiers (`x`), operators (`+`), and literals (`1`, `2`). Once 
tokenized, the compiler can start to reason about structure instead of individual characters.

In our example:

{% highlight text %}
let x = 1 + 2;
{% endhighlight %}

The tokenizer will produce something like:

{% highlight text %}
[Let, Identifier("x"), Equals, Integer(1), Plus, Integer(2), Semicolon]
{% endhighlight %}

So, we need to define our tokens:

{% highlight rust %}
#[derive(Debug, PartialEq)]
pub enum Token {
    Let,
    Identifier(String),
    Equals,
    Integer(i64),
    Plus,
    Semicolon,
}
{% endhighlight %}

Now we need to actually turn a string into a set of tokens.

{% highlight rust %}
pub fn tokenize(input: &str) -> Vec<Token> {
    let mut chars = input.chars().peekable();
    let mut tokens = Vec::new();

    while let Some(&ch) = chars.peek() {
        match ch {
            ' ' | '\n' | '\t' => {
                chars.next();
            }

            '=' => {
                chars.next();
                tokens.push(Token::Equals);
            }

            '+' => {
                chars.next();
                tokens.push(Token::Plus);
            }

            ';' => {
                chars.next();
                tokens.push(Token::Semicolon);
            }

            '0'..='9' => {
                let mut num = String::new();
                while let Some('0'..='9') = chars.peek() {
                    num.push(chars.next().unwrap());
                }
                let value = num.parse().unwrap();
                tokens.push(Token::Integer(value));
            }

            'a'..='z' | 'A'..='Z' | '_' => {
                let mut ident = String::new();
                while let Some(ch) = chars.peek() {
                    if ch.is_alphanumeric() || *ch == '_' {
                        ident.push(chars.next().unwrap());
                    } else {
                        break;
                    }
                }

                match ident.as_str() {
                    "let" => tokens.push(Token::Let),
                    _ => tokens.push(Token::Identifier(ident)),
                }
            }

            _ => panic!("Unexpected character: {}", ch),
        }
    }

    tokens
}
{% endhighlight %}

You can see here that there's no real validation going on here. We're just turning significant characters into tokens. 
The only piece of validation that does occur is if a character isn't supported by the process, we'll `panic`.

# Parsing

Now that we have a stream of tokens, it’s time to make sense of them structurally. That’s the job of **parsing** — 
converting a flat list of tokens into a **tree-like structure** that represents the **hierarchy and meaning** of the 
code.

This structure is called an **Abstract Syntax Tree** (AST).

An AST is a simplified, structured representation of your source code that **captures its grammatical structure** — 
without worrying about superficial syntax details like commas, parentheses, or whitespace. It lets the compiler 
understand *what the code means* rather than just *what it looks like*.

Taking our simple example from above, we want to produce a tree that looks like this:

<div class="mermaid">
graph TD
    A["LetBinding"]
    A1["name: 'x'"]
    A2["value: BinaryOp"]
    A --> A1
    A --> A2

    B1["left: Literal(1)"]
    B2["op: '+'"]
    B3["right: Literal(2)"]
    A2 --> B1
    A2 --> B2
    A2 --> B3
</div>

The parser turns our flat tokens into this rich structure — making it possible for later phases (like type checking 
and code generation) to analyze, transform, and ultimately execute the code.

The expressions that our parser will support can be a literal value, or it can be a binary operation. You'll notice 
that the binary operation is also self-referencing `Expr`.

{% highlight rust %}
#[derive(Debug)]
pub enum Expr {
    Literal(i64),
    BinaryOp {
        left: Box<Expr>,
        op: char,
        right: Box<Expr>,
    },
}

#[derive(Debug)]
pub struct LetBinding {
    pub name: String,
    pub value: Expr,
}
{% endhighlight %}

The let binding is a type to bind our expression to a symbolic name.

Now we can take a list of tokens, and parse that out into a `LetBinding`:

{% highlight rust %}
pub fn parse(tokens: &[Token]) -> LetBinding {
    let mut pos = 0;

    fn expect_token(tokens: &[Token], pos: &mut usize, expected: &Token) {
        if &tokens[*pos] != expected {
            panic!("Expected {:?}, got {:?}", expected, tokens[*pos]);
        }
        *pos += 1;
    }

    expect_token(tokens, &mut pos, &Token::Let);

    let name = match &tokens[pos] {
        Token::Identifier(s) => {
            pos += 1;
            s.clone()
        }
        other => panic!("Expected identifier, got {:?}", other),
    };

    expect_token(tokens, &mut pos, &Token::Equals);

    let left = match &tokens[pos] {
        Token::Integer(n) => {
            pos += 1;
            Expr::Literal(*n)
        }
        _ => panic!("Expected integer"),
    };

    let op = match &tokens[pos] {
        Token::Plus => {
            pos += 1;
            '+'
        }
        _ => panic!("Expected '+'"),
    };

    let right = match &tokens[pos] {
        Token::Integer(n) => {
            pos += 1;
            Expr::Literal(*n)
        }
        _ => panic!("Expected integer"),
    };

    expect_token(tokens, &mut pos, &Token::Semicolon);

    LetBinding {
        name,
        value: Expr::BinaryOp {
            left: Box::new(left),
            op,
            right: Box::new(right),
        },
    }
}
{% endhighlight %}

# Semantic Analysis

Once we have an Abstract Syntax Tree (AST), we know *how* the code is structured. But we still don’t know if the code 
makes sense.

That’s where **semantic analysis** comes in. This phase checks the *meaning* of the code — validating things like:
- Are all variables declared before they’re used?
- Are types used correctly?
- Can the operations actually be performed?

Even though `let x = 1 + 2;` is syntactically valid, we still need to ensure the types on both sides of `+` are 
compatible, and that `x` is a valid target for assignment.

Semantic analysis walks the AST and performs:
- **Type checking** (e.g., can you add these two things?)
- **Scope resolution** (e.g., is this variable declared?)
- **Error reporting** for violations (e.g., type mismatches)

We need to tell our compiler about types.

{% highlight rust %}
#[derive(Debug, PartialEq)]
enum Type {
    Int,
}
{% endhighlight %}

Now we can lean on the recursive nature of our `Expr` struct to process it recursively. Very simply, we only support 
one type: `Int`.

{% highlight rust %}
fn type_check(expr: &Expr) -> Result<Type, String> {
    match expr {
        Expr::Literal(_) => Ok(Type::Int),
        Expr::BinaryOp { left, right, .. } => {
            let lt = type_check(left)?;
            let rt = type_check(right)?;
            if lt == Type::Int && rt == Type::Int {
                Ok(Type::Int)
            } else {
                Err("Type mismatch".into())
            }
        }
    }
}
{% endhighlight %}


# Intermediate Representation (IR)

At this point, we have code that’s both **structurally valid** (parsed into an AST) and **semantically correct** 
(passes type checking). Now it’s time to **lower** that code into something simpler and easier to manipulate: an 
**Intermediate Representation**, or **IR**.

Think of IR as the "compiler's private language" — a stripped-down version of your program that's:
- Easier to optimize
- Easier to analyze
- Easier to transform into real machine instructions

In real-world compilers, IR might take the form of:
- [LLVM IR](https://llvm.org/docs/LangRef.html): used by Rust itself
- [MIR](https://rustc-dev-guide.rust-lang.org/mir/index.html): Rust’s internal mid-level representation
- [Bytecode](https://en.wikipedia.org/wiki/Java_bytecode): as used by interpreters like Java

In our toy compiler, we’ll build a **stack-based IR** — a sequence of simple instructions like:

{% highlight text %}
LoadConst 1
LoadConst 2
Add
Store x
{% endhighlight %}

These IR instructions are:
- Flat (no nesting like ASTs)
- Uniform (each step is explicit)
- Machine-friendly (easy to interpret or compile)

This gives us a stable, minimal foundation for:
- **Optimizations** like constant folding
- **Code generation** for interpreters or assembly
- **Debugging and instrumentation**

We’ll now:
- Define an IR instruction set in Rust
- Walk the AST to emit IR
- Prepare for later phases like optimization and evaluation

This is the compiler’s **bridge between high-level intent and low-level action**.

We need a way to define our IR:

{% highlight rust %}
#[derive(Debug)]
pub enum IR {
    LoadConst(i64),
    Add,
    Store(String),
}
{% endhighlight %}

Now we can generate IR from our `LetBinding`:

{% highlight rust %}
pub fn generate_ir(binding: &LetBinding) -> Vec<IR> {
    let mut ir = Vec::new();

    match &binding.value {
        Expr::BinaryOp { left, right, op } => {
            if let Expr::Literal(l) = **left {
                ir.push(IR::LoadConst(l));
            }
            if let Expr::Literal(r) = **right {
                ir.push(IR::LoadConst(r));
            }

            if *op == '+' {
                ir.push(IR::Add);
            }
        }
        _ => panic!("Unsupported expression"),
    }

    ir.push(IR::Store(binding.name.clone()));
    ir
}
{% endhighlight %}

# Optimization

Once we have our Intermediate Representation (IR), we can start to **improve it**.

That’s what the optimization phase is all about — rewriting the IR to make it:
- Faster to run
- Simpler to execute
- More efficient in terms of operations

Crucially, optimizations **don’t change the meaning** of the program. They just make it better.

In our toy compiler, we’ll implement a classic example: **constant folding**. This is when the compiler evaluates 
constant expressions *ahead of time*.

Instead of this:

{% highlight text %}
LoadConst 1
LoadConst 2
Add
Store x
{% endhighlight %}

We generate this:

{% highlight text %}
LoadConst 3
Store x
{% endhighlight %}

That means less work at runtime — and it's a stepping stone to more advanced techniques like dead code elimination, 
common subexpression elimination, or register allocation.

Even in small compilers, optimization is important because:
- It reduces unnecessary instructions
- It prepares the IR for efficient code generation
- It gives you experience with real-world compiler passes

In this section, we’ll:
- Walk through our IR instructions
- Detect simple constant patterns
- Replace them with pre-computed values

The logic will be basic — but the mechanism will mirror what real compilers do at massive scale.

{% highlight rust %}
pub fn optimize(ir: &[IR]) -> Vec<IR> {
    if let [IR::LoadConst(a), IR::LoadConst(b), IR::Add, IR::Store(name)] = &ir[..] {
        vec![
            IR::LoadConst(a + b),
            IR::Store(name.clone()),
        ]
    } else {
        ir.to_vec()
    }
}
{% endhighlight %}

# Code Generation / Evaluation

Rather than generating x86 assembly, let’s just **interpret** the IR.

{% highlight rust %}
use std::collections::HashMap;

pub fn run(ir: &[IR]) {
    let mut stack = Vec::new();
    let mut vars = HashMap::new();

    for instr in ir {
        match instr {
            IR::LoadConst(n) => stack.push(*n),
            IR::Add => {
                let b = stack.pop().unwrap();
                let a = stack.pop().unwrap();
                stack.push(a + b);
            }
            IR::Store(name) => {
                let val = stack.pop().unwrap();
                vars.insert(name.clone(), val);
                println!("{} = {}", name, val);
            }
        }
    }
}
{% endhighlight %}

# Putting it all together

Now we can use each of these functions in a pretty neat sequence:

{% highlight rust %}
fn main() {
    let source = "let x = 1 + 2;";
    let tokens = tokenize(source);
    let parsed = parse(&tokens);
    let _ = type_check(&parsed.value).expect("Type error");
    let ir = generate_ir(&parsed);
    let opt_ir = optimize(&ir);
    run(&opt_ir);
}
{% endhighlight %}

Output:

{% highlight text %}
x = 3
{% endhighlight %}

# Summary

Well, that's the basics of the internals of a compiler!

We've built the start of something here that could be built upon with new pieces at every step.