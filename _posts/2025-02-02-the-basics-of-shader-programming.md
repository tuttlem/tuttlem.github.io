---
layout: post
title: The Basics of Shader Programming
date: 2025-02-02
comments: false
categories: [ "opengl", "gpu" ]
---

# Introduction

Shaders are one of the most powerful tools in modern computer graphics, allowing real-time effects, lighting, and 
animation on the **GPU (Graphics Processing Unit)**. They are used in **games, simulations, and rendering engines** 
to control how pixels and geometry appear on screen.

In this article, we’ll break down:

- What shaders are and why they matter
- How to write your first shader
- Understanding screen coordinates
- Animating a shader

This guide assumes **zero prior knowledge** of shaders and will explain **each line of code step by step**.

All of the code here can be run using [Shadertoy](https://www.shadertoy.com). You won't need to install any dependencies, 
but you will need a GPU-capable computer!

# What is a Shader?

A **shader** is a small program that runs on the GPU. Unlike regular CPU code, shaders are **executed in parallel for 
every pixel or vertex** on the screen.

## Types of Shaders

1. **Vertex Shader** – Moves and transforms individual points in 3D space.
2. **Fragment Shader (Pixel Shader)** – Determines the **final color of each pixel**.

For now, we’ll **focus on fragment shaders** since they control **how things look**.

# Your First Shader

Let’s start with the **simplest shader possible**: **a solid color fill**.

## Seeing Red!

{% highlight text %}
void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    fragColor = vec4(1.0, 0.0, 0.0, 1.0); // Solid red color
}
{% endhighlight %}

Breaking this code down:

- `void mainImage(...)` → This function runs **for every pixel** on the screen.
- `fragColor` → The **output color** of the pixel.
- `vec4(1.0, 0.0, 0.0, 1.0)` → This defines an **RGBA color**:
  - `1.0, 0.0, 0.0` → **Red**
  - `1.0` → **Fully opaque (no transparency)**

**Try This:** Change the color values:
- `vec4(0.0, 1.0, 0.0, 1.0);` → Green
- `vec4(0.0, 0.0, 1.0, 1.0);` → Blue
- `vec4(1.0, 1.0, 0.0, 1.0);` → Yellow

# Mapping Colors to Screen Position

Instead of filling the screen with a single color, let's **map colors to pixel positions**.

## A Gradient Shader

{% highlight text %}
void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = fragCoord / iResolution.xy; // Normalize coordinates (0 to 1)
    fragColor = vec4(uv.x, uv.y, 0.5, 1.0);
}
{% endhighlight %}

Breaking this code down:

- `fragCoord / iResolution.xy` → Converts pixel coordinates into a **0 → 1 range**.
- `uv.x` → Controls **red** (left to right gradient).
- `uv.y` → Controls **green** (bottom to top gradient).
- `0.5` → Keeps **blue** constant.

This creates a **smooth gradient** across the screen!

**Try This:** Swap `uv.x` and `uv.y` to see different patterns.

# Animation

Shaders can **react to time** using `iTime`. This lets us create **dynamic effects**.

## Moving Color Waves

{% highlight text %}
void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = fragCoord / iResolution.xy;
    float wave = sin(uv.x * 10.0 + iTime);
    fragColor = vec4(wave, wave, wave, 1.0);
}
{% endhighlight %}

Breaking this code down:

- `sin(uv.x * 10.0 + iTime)` → Creates **waves that move over time**.
- The **whole screen pulses** from **black to white** dynamically.

**Try This:** Change `10.0` to `20.0` or `5.0` to make waves **tighter or wider**.

# Wrapping up

Here have been some very simple shader programs to get started with. In future articles, we'll build on this knowledge 
to build more exciting graphics applications.
