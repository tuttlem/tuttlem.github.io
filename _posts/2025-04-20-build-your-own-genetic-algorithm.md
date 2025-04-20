---
layout: post
title: Build your own Genetic Algorithm
date: 2025-04-20
comments: false
categories: [ "" ]
---

# Introduction


Genetic algorithms (GAs) are one of those wild ideas in computing where the solution isn't hand-coded — it's *grown*.

They borrow inspiration straight from biology. Just like nature evolved eyes, wings, and brains through selection and 
mutation, we can evolve solutions to problems in software. Not by brute-force guessing, but by letting generations of 
candidates compete, reproduce, and adapt.

At a high level, a genetic algorithm looks like this:

1. **Create a population** of random candidate solutions.
2. **Score** each one — how “fit” or useful is it?
3. **Select** the best performers.
4. **Breed** them together to make the next generation.
5. **Mutate** some of them slightly, to add variation.
6. **Repeat** until something good enough evolves.

There’s no central intelligence. No clever algorithm trying to find the best answer. Just selection pressure pushing 
generations toward better solutions — and that’s often enough.

What’s powerful about GAs is that they’re not tied to any specific kind of problem. If you can describe what a “good” 
answer looks like, even fuzzily, a GA might be able to evolve one. People have used them to:

- Evolve art or music
- Solve optimization problems
- Train strategies for games
- Design antennas for NASA

In this post, we're going to build a genetic algorithm from scratch — in pure Python — and show it working on a fun 
little challenge: evolving a string of text until it spells out `"HELLO WORLD"`.

It might be toy-sized, but the core principles are exactly the same as the big stuff.

# Defining the parts

Here, we'll break down the genetic algorithm idea into simple, solvable parts.

## Solution

First of all, we need to define a solution. The solution is what we want to work towards. It can be considered our 
"chromosome" in this example.

{% highlight python %}
TARGET = "HELLO WORLD"
{% endhighlight %}

Every character of this string can then be considered a "gene".

## Defining Fitness

Now we need a function that tells us how fit our individual is, or how close it is to our defined target:

{% highlight python %}
def fitness(individual):
    return sum(1 for i, j in zip(individual, TARGET) if i == j)
{% endhighlight %}

Here, we pair each "gene" (char) of the `individual` and target. We simply count up how many of them match. Higher score 
means higher fitness.

## Populate

We create an initial population to work with, just with some random data. We need to start somewhere, so this is as 
good as anything.

{% highlight python %}
population = [random_individual() for _ in range(POP_SIZE)]
{% endhighlight %}

`random_individual` will return a string the same size as our solution, but will go with random characters at each 
index. This provides our starting point.

## Genetics

Two key operations give genetic algorithms their evolutionary flavor: **crossover** and **mutation**. This is what 
gives us generations, allowing this algorithm to grow.

### Crossover (Recombination)

In biology, crossover happens when two parents create a child: their DNA gets shuffled together. A bit from mum, a bit 
from dad, spliced at some random point. The child ends up with a new mix of traits, some from each.

We can do exactly that with strings of characters (our “DNA”). Here's the basic idea:

{% highlight python %}
def crossover(a, b):
    split = random.randint(0, len(a))
    return a[:split] + b[split:]
{% endhighlight %}

This picks a random point in the string, then takes the first part from parent `a` and the second part from parent `b`. 
So if `a` is `"HELLOXXXX"` and `b` is `"YYYYYWORLD"`, a crossover might give you `"HELLYWORLD"`. New combinations, 
new possibilities.

### Mutation

Of course, biology isn’t just about inheritance — it also relies on randomness. DNA can get copied imperfectly: a 
flipped bit, a swapped base. Most mutations are useless. But every once in a while, one’s brilliant.

Same deal in our algorithm:

{% highlight python %}
def mutate(s):
    s = list(s)
    i = random.randint(0, len(s) - 1)
    s[i] = random_char()
    return "".join(s)
{% endhighlight %}

This picks a random character in the string and replaces it with a new random one — maybe turning an `"X"` into a `"D"`, 
or an `"O"` into an `"E"`. It adds diversity to the population and prevents us from getting stuck in a rut.

Together, crossover and mutation give us the raw machinery of evolution: recombination and novelty. With just these two 
tricks, plus a way to score fitness and select the best candidates, we can grow something surprisingly smart from 
totally random beginnings.

# Putting it all together

Now, we just loop on this. We do this over and over, until we land at a solution that marries to our "good solution" that 
we fed this system with to being with. You can see the "HELLO WORLD" example in action here, and exactly how the 
algorithm came to its answer:

{% highlight plain %}
Gen    0 | Best: RAVY  OTRTK | Score: 2
Gen    1 | Best: RAVY  OTRLH | Score: 3
Gen    2 | Best: LZELORMOHLD | Score: 5
Gen    3 | Best: SFLLORMOHLD | Score: 6
Gen    4 | Best: SFLLO OTRLD | Score: 7
Gen    5 | Best: SFLLO OTRLD | Score: 7
Gen    6 | Best: SFLLO OORLD | Score: 8
Gen    7 | Best: SFLLO MORLD | Score: 8
Gen    8 | Best: SFLLO MORLD | Score: 8
Gen    9 | Best: SFLLO MORLD | Score: 8
Gen   10 | Best: SFLLO MORLD | Score: 8
Gen   11 | Best: SFLLO MORLD | Score: 8
Gen   12 | Best: SFLLO SORLD | Score: 8
Gen   13 | Best: NFLLO MORLD | Score: 8
Gen   14 | Best: SFLLO MORLD | Score: 8
Gen   15 | Best: SFLLO WORLD | Score: 9
Gen   16 | Best: SFLLO WORLD | Score: 9
Gen   17 | Best: HFLLO WORLD | Score: 10
Gen   18 | Best: HFLLO WORLD | Score: 10
Gen   19 | Best: HFLLO WORLD | Score: 10
Gen   20 | Best: HFLLO WORLD | Score: 10
Gen   21 | Best: HFLLO WORLD | Score: 10
Gen   22 | Best: HELLO WORLD | Score: 11
{% endhighlight %}

It obviously depends on how your random number generator is feeling, but your mileage will vary.

# Code listing

A full code listing of this in action is here:

{% highlight python %}
import random
import string

TARGET = "HELLO WORLD".upper()
POP_SIZE = 100
MUTATION_RATE = 0.01
GENERATIONS = 100000

def random_char():
    return random.choice(string.ascii_uppercase + " ")

def random_individual():
    return ''.join(random_char() for _ in range(len(TARGET)))

def fitness(individual):
    return sum(1 for i, j in zip(individual, TARGET) if i == j)

def mutate(individual):
    return ''.join(
        c if random.random() > MUTATION_RATE else random_char()
        for c in individual
    )

def crossover(a, b):
    split = random.randint(0, len(a) - 1)
    return a[:split] + b[split:]

# Initial population
population = [random_individual() for _ in range(POP_SIZE)]

for generation in range(GENERATIONS):
    scored = [(ind, fitness(ind)) for ind in population]
    scored.sort(key=lambda x: -x[1])

    best = scored[0]
    print(f"Gen {generation:4d} | Best: {best[0]} | Score: {best[1]}")

    if best[1] == len(TARGET):
        print("🎉 Target reached!")
        break

    # Keep top 10 as parents
    parents = [ind for ind, _ in scored[:10]]

    # Make new population
    population = [
        mutate(crossover(random.choice(parents), random.choice(parents)))
        for _ in range(POP_SIZE)
    ]
{% endhighlight %}

# Conclusion

Genetic algorithms are a beautiful way to turn randomness into results. You don’t need deep math or fancy machine 
learning models — just a way to measure how good something is, and the patience to let evolution do its thing.

What’s even more exciting is that this toy example uses the exact same principles behind real-world tools that tackle 
complex problems in scheduling, design, game playing, and even neural network tuning.

If you’re curious to take this further, there are full-featured libraries and frameworks out there built for serious 
applications:

* [DEAP](https://deap.readthedocs.io/en/master/) (Distributed Evolutionary Algorithms in Python) – A flexible framework for evolutionary algorithms. Great for research and custom workflows.
* [PyGAD](https://pygad.readthedocs.io/en/latest/) – Simple, powerful, and easy to use — especially good for optimizing neural networks and functions.
* [ECJ](https://cs.gmu.edu/~eclab/projects/ecj/) – A Java-based evolutionary computing toolkit used in academia and industry.
* [Jenetics](https://jenetics.io/) – Another Java library that’s modern, elegant, and geared toward real engineering problems.

These libraries offer more advanced crossover strategies, selection techniques (like tournament or roulette-wheel), 
and even support for parallel processing or multi-objective optimization.

But even with a simple string-matching example, you’ve now seen how it all works under the hood — survival of the 
fittest, one generation at a time.

Now go evolve something weird.