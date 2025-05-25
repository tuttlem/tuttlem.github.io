---
layout: post
title: Building a Stack-Based VM in Rust - Part 5
date: 2025-05-25
comments: false
categories: [ rust, vm, forth, systems ]
---

# Introduction

In [Part 4]({% post_url 2025-05-25-building-a-stack-based-vm-in-rust-part-4 %}), we introduced **named words** and a
dictionary that allowed our VM to call subroutines by name. But there was still one major gap:

We couldn’t write Forth-like code.

You still had to manually build `vec![Instruction::Push(5), ...]` in Rust. That changes now.

In this post, we’ll add a **hand-rolled parser** that understands simple Forth syntax — including word definitions like
`: square dup * ;` — and emits instructions automatically.

By the end of this part, you’ll be able to write:

{% highlight forth %}
5 square : square dup * ;
{% endhighlight %}

And run it on your virtual machine with no hardcoded addresses or manual instruction building.

# The Goal

Our parser will:

- Tokenize simple Forth input
- Track whether we're inside a `:` definition
- Split instructions into `main` and `definitions`
- Insert a `Halt` after top-level code to prevent fall-through
- Track the correct addresses for word definitions
- Build a final list of instructions ready to run

Here’s the full updated code.

# Parser Support

First of all, we define our `Parser` struct — this separates the parsing logic from the VM runtime.

{% highlight rust %}
struct Parser {
    main: Vec<Instruction>,
    definitions: Vec<Instruction>,
    dictionary: HashMap<String, usize>,
}
{% endhighlight %}

Here's what each member does:

- `main`: Top-level code that runs first. This is where calls like `5 square` are emitted.
- `definitions`: Instructions belonging to `: word ... ;` definitions.
- `dictionary`: A mapping of word names (like `"square"`) to their starting address in the final instruction stream.

We initialize the parser with empty sections:

{% highlight rust %}
impl Parser {
    fn new() -> Self {
        Self {
            main: Vec::new(),
            definitions: Vec::new(),
            dictionary: HashMap::new(),
        }
    }
}
{% endhighlight %}

# Token Parsing

The heart of the parser is the `parse` method. We split the input on whitespace and interpret each token in turn.

{% highlight rust %}
fn parse(&mut self, input: &str) {
    let mut tokens = input.split_whitespace().peekable();
    let mut defining: Option<String> = None;
    let mut buffer: Vec<Instruction> = Vec::new();

    while let Some(token) = tokens.next() {
        match token {
            ":" => {
                // Beginning of a word definition
                let name = tokens.next().expect("Expected word name after ':'");
                defining = Some(name.to_string());
                buffer.clear();
            }
            ";" => {
                // End of word definition
                if let Some(name) = defining.take() {
                    buffer.push(Instruction::Return);
                    let addr = self.main.len() + self.definitions.len() + 1; // +1 for HALT
                    self.dictionary.insert(name, addr);
                    self.definitions.extend(buffer.drain(..));
                } else {
                    panic!("Unexpected ';' outside of word definition");
                }
            }
            word => {
                // Otherwise, parse an instruction
                let instr = if let Ok(n) = word.parse::<i32>() {
                    Instruction::Push(n)
                } else {
                    match word {
                        "dup" => Instruction::Dup,
                        "drop" => Instruction::Drop,
                        "swap" => Instruction::Swap,
                        "over" => Instruction::Over,
                        "+" => Instruction::Add,
                        "*" => Instruction::Mul,
                        "depth" => Instruction::Depth,
                        _ => Instruction::CallWord(word.to_string()),
                    }
                };

                // Add to appropriate section
                if defining.is_some() {
                    buffer.push(instr);
                } else {
                    self.main.push(instr);
                }
            }
        }
    }
}
{% endhighlight %}

### Breakdown of the cases:

- `:` begins a new named word definition.
- `;` ends the definition, emits a `Return`, and stores the word's starting address in the dictionary.
- A number becomes a `Push(n)` instruction.
- Built-in words like `+` and `*` become direct `Instruction` variants.
- Any unknown token is assumed to be a user-defined word, and gets translated to `CallWord("name")`.

# Finalizing the Program

Once parsing is complete, we combine the `main` program with definitions — separated by a `Halt` to ensure we don’t fall through.

{% highlight rust %}
fn finalize(self) -> (Vec<Instruction>, HashMap<String, usize>) {
    let mut instructions = self.main;
    instructions.push(Instruction::Halt); // Halts after main program
    instructions.extend(self.definitions);
    (instructions, self.dictionary)
}
{% endhighlight %}

# Main Program

Our `main()` function now uses the parser to construct the program from a Forth-style string.

{% highlight rust %}
fn main() {
    let mut parser = Parser::new();
    parser.parse("5 square : square dup * ;");

    let (instructions, dictionary) = parser.finalize();

    for instr in &instructions {
        println!("{:?}", instr);
    }

    let mut vm = VM::new(instructions);
    vm.dictionary = dictionary;
    vm.run();

    println!("Final stack: {:?}", vm.stack); 
}
{% endhighlight %}

You should see the following output:

{% highlight text %}
Push(5)
CallWord("square")
Halt
Dup
Mul
Return
Final stack: [25]
{% endhighlight %}

# Conclusion

This was a big leap forward: we now parse and run real Forth-like programs, entirely from text.

The parser separates top-level code from definitions, calculates addresses correctly, inserts a `Halt`, and builds a 
dictionary of reusable named words.

We now have:

- A working VM
- An extensible instruction set
- Named words and subroutines
- A parser for Forth-style input

The code for this part can be found [up on GitHub](https://github.com/tuttlem/tiny_forth/tree/part5).
