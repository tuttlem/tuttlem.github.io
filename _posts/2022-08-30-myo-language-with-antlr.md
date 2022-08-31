---
layout: post
title: MYO Language with Antlr
date: 2022-08-30
comments: false
categories: [ "antlr", "lang" ]
---

### Introduction

[ANTLR](https://www.antlr.org/) is a code generation tool for making language
parsers. Using a grammer file, you can get ANTLR to generate code to read, 
interpret, and execute your very own code.

In today's article I'll walk through the basic setup to create a Calculator
language that can execute simple equations in a [golang](https://go.dev/) 
project of our own.

### Before you begin

You'll need a [JRE](https://www.java.com/en/download/manual.jsp).

Before we start, there are some software pre-requisites. You will need to 
install ANTLR. This is a simple [JAR File](https://docs.oracle.com/javase/tutorial/deployment/jar/basicsindex.html)
that we can invoke locally.

{% highlight bash %}
$ wget http://www.antlr.org/download/antlr-4.7-complete.jar
$ alias antlr='java -jar $PWD/antlr-4.7-complete.jar'
{% endhighlight %}

### Code generation

Now that we've got ANTLR installed, it's time to generate some code. We do this
using a grammer file. A very comprehensive calculator can be found in the examples
of the [antlr grammers](https://github.com/antlr/grammars-v4) repository [here](https://github.com/antlr/grammars-v4/blob/master/calculator/calculator.g4).

For today's example, we'll just focus on addition, subtraction, multiplication, and division 
with the following grammer file:

{% highlight plain %}
// Calc.g4
grammar Calc;

// Tokens
MUL: '*';
DIV: '/';
ADD: '+';
SUB: '-';
NUMBER: [0-9]+;
WHITESPACE: [ \r\n\t]+ -> skip;

// Rules
start : expression EOF;

expression
   : expression op=('*'|'/') expression # MulDiv
   | expression op=('+'|'-') expression # AddSub
   | NUMBER                             # Number
   ;
{% endhighlight %}

Even without fully understanding the grammer language, you can see that there is
some basic token definitions, rules, and expression definitions.

`MUL`, `DIV`, `ADD`, `SUB`, `NUMBER`, and `WHITESPACE` all being significant to 
the language that we're definting.

The `expression` definition not only defines operations for us, but will also be
key in defining operator precedence, with the `MulDiv` rule occuring before the `AddSub`
rule, finally dealing with `Number`.

We can turn this grammer file into some go code with the following invocation:

{% highlight bsah %}
$ antlr -Dlanguage=Go -o parser Calc.g4
{% endhighlight %}

This creates a `parser` folder for us now with a few different pieces of go code.

### Parsers, Lexers, and Listener

If you look in the `parser` folder at the code that was created, you shoul see something
similar to this:

{% highlight plain %}
└── parser
    ├── calc_base_listener.go
    ├── calc_lexer.go
    ├── CalcLexer.tokens
    ├── calc_listener.go
    ├── calc_parser.go
    └── Calc.tokens
{% endhighlight %}

The Lexer's job is to perform [Lexical Analysis](https://en.wikipedia.org/wiki/Lexical_analysis) on
arbitrary pieces of text, and tokenizes that text into a set of symbols. For example, the input
of `1 + 2` might get tokenized to `NUMBER 1, ADD, NUMBER 2`. These tokens are now
fed into the parser.

The [Parser's](https://en.wikipedia.org/wiki/Parsing#Parser) job is to take these
tokens, and make sure they conform to the rules of the language. You can imagine that
a LISP style language would expect `ADD, NUMBER 1, NUMBER 2` rather than a c-style
language that would expect the operator in between the number tokens.

After the string has passed through the lexer and the parser, it now runs through
the listener where we can write some code to respond to these symbols in order.

### Implementation

The internal implementation of this calculator is a stack-based calculator. This gets
represented as `struct`:

{% highlight go %}
type calculatorListener struct {
	*parser.BaseCalcListener
	stack []int
}
{% endhighlight %}

The internal state of the calculator are `int` values on that stack. As operations
execute, the program will take the top of the stack as well that second-to-the-top 
and perform arithmetic, leaving the result on the top of the stack.

{% highlight go %}
func (l *calculatorListener) push(i int) {
	l.stack = append(l.stack, i)
}

func (l *calculatorListener) pop() int {
	if len(l.stack) < 1 {
		panic("TOS invalid")
	}

	result := l.stack[len(l.stack)-1]
	l.stack = l.stack[:len(l.stack)-1]

	return result
}
{% endhighlight %}

The `BaseCalcListner` type that was generated for us has all of the hooks we need
to latch onto the complete the implementation. The `NUMBER`, `ADDSUB`, and `MULDIV` rules
all get their own listener for us to respond to.

{% highlight go %}
func (l *calculatorListener) ExitMulDiv(c *parser.MulDivContext) {
  // get TOS and STOS
	rhs, lhs := l.pop(), l.pop()

  // perform the required operation, pushing the result back
  // up as the new TOS
	switch c.GetOp().GetTokenType() {
	case parser.CalcParserMUL:
		l.push(lhs * rhs)
	case parser.CalcParserDIV:
		l.push(lhs / rhs)
	default:
		panic(fmt.Sprintf("not yet implemented: %s", c.GetOp().GetText()))
	}
}

func (l *calculatorListener) ExitAddSub(c *parser.AddSubContext) {
  // get TOS and STOS
	rhs, lhs := l.pop(), l.pop()

  // perform the required operation, pushing the result back
  // up as the new TOS
	switch c.GetOp().GetTokenType() {
	case parser.CalcParserADD:
		l.push(lhs + rhs)
	case parser.CalcParserSUB:
		l.push(lhs - rhs)
	default:
		panic(fmt.Sprintf("not yet implemented: %s", c.GetOp().GetText()))
	}
}

func (l *calculatorListener) ExitNumber(c *parser.NumberContext) {
  // coerce the string into an integer
	i, err := strconv.Atoi(c.GetText())
	if err != nil {
		panic(err.Error())
	}

  // push onto the stack
	l.push(i)
}
{% endhighlight %}

### Execution

Now we go from text input to execution. In the following snippet, the 
input stream feeds the text into the lexer. The lexer then gets setup
as a stream ready to tokenize our input.

Finally, all of those tokens get parsed to make sure they represent
valid expressions for our language.

{% highlight go %}
equation := "1 + 5 - 2 * 20"
is := antlr.NewInputStream(equation)
lexer := parser.NewCalcLexer(is)
stream := antlr.NewCommonTokenStream(lexer, antlr.TokenDefaultChannel)

p := parser.NewCalcParser(stream)
{% endhighlight %}

We can now walk the parser tree with a listener attached. The listener
will fire off our hooks that we defined earlier; and our stack-based calculator
should leave us with the result at the TOS.

{% highlight go %}
var listener calcListener
antlr.ParseTreeWalkerDefault.Walk(&listener, p.Start())
answer := listener.pop()

fmt.Printf("%s = %d", equation, answer)
{% endhighlight %}

We should be left with something like this on screen:

{% highlight plain %}
1 + 5 - 2 * 20 = -34
{% endhighlight %}

### Conclusion

As you can see, ANTLR is a very powerful tool for writing all of the pieces
of a compiler (or in this case, an interpreter) to get you kick started very
quickly. 

You'd almost be insane to ever do this stuff yourself!

