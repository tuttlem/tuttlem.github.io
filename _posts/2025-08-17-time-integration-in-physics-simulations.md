---
layout: post
title: Time Integration in Physics Simulations
date: 2025-08-17
comments: false
categories: [ math, physics, integration, simulations, python ]
---

# Introduction

When simulating physical systems—whether it's a bouncing ball, orbiting planets, or particles under gravity—accurately 
updating positions and velocities over time is crucial. This process is known as **time integration**, and it's the 
backbone of most game physics and real-time simulations.

In this post, we'll explore two fundamental methods for time integration: **Euler's method** and **Runge-Kutta 4 (RK4)**. 

We'll go through how each of these methods is represented mathemtically, and then we'll translate that into code. 
We'll build a small visual simulation in Python using `pygame` to see how the two methods behave differently when 
applied to the same system.

# The Simulation

Our simulation consists of a central massive object (a "sun") and several orbiting bodies, similar to a simplified 
solar system. Each body is influenced by the gravitational pull of the others, and we update their positions and 
velocities in each frame of the simulation loop.

At the heart of this simulation lies a decision: how should we advance these objects forward in time? This is where the 
integration method comes in.

## Euler's Method

Euler's method is the simplest way to update motion over time. It uses the current velocity to update position, and the 
current acceleration to update velocity:

$$
\begin{aligned}
\vec{x}_{t+\Delta t} &= \vec{x}_t + \vec{v}_t \cdot \Delta t \\\\
\vec{v}_{t+\Delta t} &= \vec{v}_t + \vec{a}_t \cdot \Delta t
\end{aligned}
$$

This translates down into the following python code:

{% highlight python %}
def step_euler(bodies):
    accs = [compute_acc(bodies, i) for i in range(len(bodies))]
    for b, a in zip(bodies, accs):
        b.pos += b.vel * DT
        b.vel += a * DT
{% endhighlight %}

This is easy to implement, but has a major downside: **error accumulates quickly**, especially in systems with strong 
forces or rapidly changing directions.

Here's an example of it running:

<video muted autoplay loop width="800">
    <source src="{{ site.url }}/assets/euler.mp4" type="video/mp4">
</video>

## RK4

Runge-Kutta 4 (RK4) improves on Euler by sampling the system at multiple points within a single timestep. It estimates 
what will happen halfway through the step, not just at the beginning. This gives a much better approximation of curved 
motion and reduces numerical instability.

Runge-Kutta 4 samples the derivative at four points:

$$
\begin{aligned}
\vec{k}_1 &= f(t, \vec{y}) \\\\
\vec{k}_2 &= f\left(t + \frac{\Delta t}{2}, \vec{y} + \frac{\vec{k}_1 \cdot \Delta t}{2}\right) \\\\
\vec{k}_3 &= f\left(t + \frac{\Delta t}{2}, \vec{y} + \frac{\vec{k}_2 \cdot \Delta t}{2}\right) \\\\
\vec{k}_4 &= f\left(t + \Delta t, \vec{y} + \vec{k}_3 \cdot \Delta t\right) \\\\
\vec{y}_{t+\Delta t} &= \vec{y}_t + \frac{\Delta t}{6}(\vec{k}_1 + 2\vec{k}_2 + 2\vec{k}_3 + \vec{k}_4)
\end{aligned}
$$

{% include callout.html type="info" title="Combined State!" text="The y vector here represents both position and velocity as a combined state vector." %}

This translates down into the following python code:

{% highlight python %}
def step_rk4(bodies):
    n = len(bodies)
    pos0 = [b.pos.copy() for b in bodies]
    vel0 = [b.vel.copy() for b in bodies]

    a1 = [compute_acc(bodies, i) for i in range(n)]

    for i, b in enumerate(bodies):
        b.pos = pos0[i] + vel0[i] * (DT / 2)
        b.vel = vel0[i] + a1[i] * (DT / 2)
    a2 = [compute_acc(bodies, i) for i in range(n)]

    for i, b in enumerate(bodies):
        b.pos = pos0[i] + b.vel * (DT / 2)
        b.vel = vel0[i] + a2[i] * (DT / 2)
    a3 = [compute_acc(bodies, i) for i in range(n)]

    for i, b in enumerate(bodies):
        b.pos = pos0[i] + b.vel * DT
        b.vel = vel0[i] + a3[i] * DT
    a4 = [compute_acc(bodies, i) for i in range(n)]

    for i, b in enumerate(bodies):
        b.pos = pos0[i] + vel0[i] * DT + (DT**2 / 6) * (a1[i] + 2*a2[i] + 2*a3[i] + a4[i])
        b.vel = vel0[i] + (DT / 6) * (a1[i] + 2*a2[i] + 2*a3[i] + a4[i])
{% endhighlight %}

RK4 requires more code and computation, but the visual payoff is immediately clear: smoother orbits, fewer explosions, 
and longer-lasting simulations.

Here's an example of it running:

<video muted autoplay loop width="800">
    <source src="{{ site.url }}/assets/rk4.mp4" type="video/mp4">
</video>

## Trade-offs

Euler is fast and simple. It's great for prototyping, simple games, or systems where precision isn't critical.

RK4 is more accurate and stable, especially in chaotic or sensitive systems—but it’s computationally more expensive. 
In real-time applications (like games), you’ll need to weigh performance vs. quality.

Also, both methods depend heavily on the size of the timestep. Larger steps amplify error; smaller ones improve 
accuracy at the cost of performance.

# Conclusion

Switching from Euler to RK4 doesn't just mean writing more code—it fundamentally changes how your simulation evolves over time. If you're seeing odd behaviors like spiraling orbits, exploding systems, or jittery motion, trying a higher-order integrator like RK4 might fix it.

Or, it might inspire a deeper dive into the world of numerical simulation—welcome to the rabbit hole!

You can find the [full code listing here](https://gist.github.com/tuttlem/1e29463621a103b9dd513b7f8cb33972) as a gist, 
so you can tweak and run it for yourself.
