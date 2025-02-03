---
layout: post
title: Raymarching Reflections
date: 2025-02-03
comments: false
categories: [ "shaders", "raymarching", "reflections" ]
---

# Introduction

Ray tracing is known for producing stunning reflections, we can achieve the same effect using ray 
marching. In this post, we’ll walk through a classic two-sphere reflective scene, but instead of traditional ray 
tracing, we’ll ray march our way to stunning reflections.

This post builds on previous articles:

- [The Basics of Shader Programming]({% post_url 2025-02-02-the-basics-of-shader-programming %})
- [Ray Marching and Mandelbulbs]({% post_url 2025-02-03-raymarching-and-mandelbulbs %})
- [Volumetric Fog in Shaders]({% post_url 2025-02-03-volumetric-fog-in-shaders %})

Let’s get started!

# Setup

The first step is defining a scene with two spheres and a ground plane. In ray marching, objects are defined using 
signed distance functions (SDFs). Our scene SDF is just a combination of smaller SDFs.

## SDFs

The SDF for a sphere gives us the distance from any point to the surface of the sphere:

{% highlight javascript %}
float sdfSphere(vec3 p, vec3 center, float radius) {
    return length(p - center) - radius;
}
{% endhighlight %}

The SDF for a ground plane:

{% highlight javascript %}
float sdfGround(vec3 p) {
    return p.y + 1.5;  // Flat ground at y = -1.5
}
{% endhighlight %}

Finally, we combine the objects into a scene SDF:

{% highlight javascript %}
float sceneSDF(vec3 p) {
    float sphere1 = sdfSphere(p, vec3(-1.0, 0.0, 3.0), 1.0);
    float sphere2 = sdfSphere(p, vec3(1.0, 0.0, 3.0), 1.0);
    float ground = sdfGround(p);
    return min(ground, min(sphere1, sphere2));
}
{% endhighlight %}

# Raymarching

Now we trace a ray through our scene using ray marching.

{% highlight javascript %}
vec3 rayMarch(vec3 rayOrigin, vec3 rayDir, int maxSteps, float maxDist) {
    float totalDistance = 0.0;
    vec3 hitPoint;

    for (int i = 0; i < maxSteps; i++) {
        hitPoint = rayOrigin + rayDir * totalDistance;
        float dist = sceneSDF(hitPoint);
        if (dist < 0.001) break;  // Close enough to surface
        if (totalDistance > maxDist) return vec3(0.5, 0.7, 1.0); // Sky color
        totalDistance += dist;
    }

    return hitPoint; // Return the hit location
}
{% endhighlight %}

# Surface Normals

For lighting and reflections, we need surface normals. These are estimated using small offsets in each direction:

{% highlight javascript %}
vec3 getNormal(vec3 p) {
    vec2 e = vec2(0.001, 0.0);
    return normalize(vec3(
        sceneSDF(p + e.xyy) - sceneSDF(p - e.xyy),
        sceneSDF(p + e.yxy) - sceneSDF(p - e.yxy),
        sceneSDF(p + e.yyx) - sceneSDF(p - e.yyx)
    ));
}
{% endhighlight %}

# Reflections

Reflections are computed using the reflect function:

$$
R = I - 2 (N \cdot I) N
$$

where:
- $$ I $$ is the **incoming ray direction**,
- $$ N $$ is the **surface normal**,
- $$ R $$ is the **reflected ray**.

In GLSL, this is done using:

{% highlight javascript %}
vec3 reflectedDir = reflect(rayDir, getNormal(hitPoint));
{% endhighlight %}

Now, we ray march again along the reflected direction:

{% highlight javascript %}
vec3 computeReflection(vec3 hitPoint, vec3 rayDir) {
    vec3 normal = getNormal(hitPoint);
    vec3 reflectedDir = reflect(rayDir, normal);
    
    vec3 reflectionHit = rayMarch(hitPoint + normal * 0.01, reflectedDir, 50, 10.0);
    return phongLighting(reflectionHit, -reflectedDir);
}
{% endhighlight %}

# Full Shader

{% highlight javascript %}
void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = (fragCoord - 0.5 * iResolution.xy) / iResolution.y;

    // Camera Setup
    vec3 rayOrigin = vec3(0, 0, -5);
    vec3 rayDir = normalize(vec3(uv, 1.0));

    // Perform Ray Marching
    vec3 hitPoint = rayMarch(rayOrigin, rayDir, 100, 10.0);

    // If we hit an object, apply shading
    vec3 color;
    if (hitPoint != vec3(0.5, 0.7, 1.0)) {
        vec3 viewDir = normalize(rayOrigin - hitPoint);
        vec3 baseLight = phongLighting(hitPoint, viewDir);
        vec3 reflection = computeReflection(hitPoint, rayDir);
        color = mix(baseLight, reflection, 0.5); // Blend reflections
    } else {
        color = vec3(0.5, 0.7, 1.0); // Sky color
    }

    fragColor = vec4(color, 1.0);
}
{% endhighlight %}

Running this shader, you should see two very reflective spheres reflecting each other.

<video muted autoplay loop width="800">
    <source src="{{ site.url }}/assets/raymarch-reflect.webm" type="video/webm">
</video>

# Conclusion

With just a few functions, we’ve recreated a classic ray tracing scene using ray marching. This technique allows 
us to:

* Render reflective surfaces without traditional ray tracing 
* Generate soft shadows using SDF normals
* Extend the method for refraction and more complex materials