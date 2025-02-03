---
layout: post
title: Raymarching and Mandelbulbs
date: 2025-02-03
comments: false
categories: [ "shaders", "raymarching", "mandelbulbs" ]
---

# Introduction

Fractals are some of the most mesmerizing visuals in computer graphics, but rendering them in 3D space requires 
special techniques beyond standard polygonal rendering. This article will take you into the world of ray marching, 
where we’ll use distance fields, lighting, and soft shadows to render a Mandelbulb fractal — one of the most famous 3D 
fractals.

By the end of this post, you'll understand:

- The basics of ray marching and signed distance functions (SDFs).
- How to render 3D objects without polygons.
- How to implement Phong shading for realistic lighting.
- How to compute soft shadows for better depth.
- How to animate a rotating Mandelbulb fractal.

This article will build on the knowledge that we established in the [Basics of Shader Programming]({% post_url 2025-02-02-the-basics-of-shader-programming %}) 
article that we put together earlier. If you haven't read through that one, it'll be worth taking a look at.

# What is Ray Marching?

[Ray marching](https://en.wikipedia.org/wiki/Ray_marching) is a distance-based rendering technique that is closely related to [ray tracing](https://en.wikipedia.org/wiki/Ray_tracing_(graphics)). 
However, instead of tracing rays until they hit exact geometry (like in traditional ray tracing), ray marching steps 
along the ray incrementally using distance fields.

Each pixel on the screen sends out a ray into 3D space. We then march forward along the ray, using a signed 
distance function (SDF) to tell us how far we are from the nearest object. This lets us render smooth implicit 
surfaces like fractals and organic shapes.

# Our first SDF

The simplest 3D object we can render using ray marching is a sphere. We define its shape using a 
signed distance function (SDF):

{% highlight text %}
// Sphere Signed Distance Function (SDF)
float sdfSphere(vec3 p, float r) {
    return length(p) - r;
}
{% endhighlight %}

The `sdfSphere()` function returns the shortest distance from any point in space to the sphere’s surface.

We can now step along that ray until we reach the sphere. We do this by integrating our `sfpSphere()` function into our 
`mainImage()` function:

{% highlight text %}
void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = (fragCoord - 0.5 * iResolution.xy) / iResolution.y;

    // Camera setup
    vec3 rayOrigin = vec3(0, 0, -3);  
    vec3 rayDir = normalize(vec3(uv, 1));

    float totalDistance = 0.0;
    const int maxSteps = 100;
    const float minDist = 0.001;
    const float maxDist = 20.0;
   
    for (int i = 0; i < maxSteps; i++) {
        vec3 pos = rayOrigin + rayDir * totalDistance;
        float dist = sdfSphere(pos, 1.0);

        if (dist < minDist) break;
        if (totalDistance > maxDist) break;

        totalDistance += dist;
    }

    vec3 col = (totalDistance < maxDist) ? vec3(1.0) : vec3(0.2, 0.3, 0.4);
    fragColor = vec4(col, 1.0);
}
{% endhighlight %}

First of all here, we convert the co-ordinate that we're processing into screen co-ordinates:

{% highlight text %}
vec2 uv = (fragCoord - 0.5 * iResolution.xy) / iResolution.y;
{% endhighlight %}

This `uv` value is now used to in the calculations to form our ray equation:

{% highlight text %}
vec3 rayOrigin = vec3(0, 0, -3);  
vec3 rayDir = normalize(vec3(uv, 1));
{% endhighlight %}

We now iterate (march) down the ray to a maximum of `maxSteps` (currently set to `100`) to determine if the ray 
intersects with our sphere (via `sdfSphere`).

Finally, we render the colour of our sphere if the distance is within tolerance; otherwise we consider this part of 
the background:

{% highlight text %}
vec3 col = (totalDistance < maxDist) ? vec3(1.0) : vec3(0.2, 0.3, 0.4);
fragColor = vec4(col, 1.0);
{% endhighlight %}

You should see something similar to this:

<video muted autoplay loop width="800">
    <source src="{{ site.url }}/assets/raymarch-sphere.webm" type="video/webm">
</video>

## Adding Lights

In order to make this sphere look a little more 3D, we can light it. In order to light any object, we need to be able 
to compute surface normals. We do that via a function like this:

{% highlight text %}
vec3 getNormal(vec3 p) {
    vec2 e = vec2(0.001, 0.0);  // Small offset for numerical differentiation
    return normalize(vec3(
        sdfSphere(p + e.xyy, 1.0) - sdfSphere(p - e.xyy, 1.0),
        sdfSphere(p + e.yxy, 1.0) - sdfSphere(p - e.yxy, 1.0),
        sdfSphere(p + e.yyx, 1.0) - sdfSphere(p - e.yyx, 1.0)
    ));
}
{% endhighlight %}

We make decisions about the actual colour via a lighting function. This lighting function is informed by the surface 
normals that it computes:

{% highlight text %}
vec3 lighting(vec3 p) {
    vec3 lightPos = vec3(2.0, 2.0, -2.0);  // Light source position
    vec3 normal = getNormal(p);  // Compute the normal at point 'p'
    vec3 lightDir = normalize(lightPos - p);  // Direction to light
    float diff = max(dot(normal, lightDir), 0.0);  // Diffuse lighting
    return vec3(diff);  // Return grayscale lighting effect
}
{% endhighlight %}

We can now integrate this back into our scene in the `mainImage` function. Rather than just making a static colour 
return when we establish a hit point, we start to execute the `lighting` function towards the end of the function:

{% highlight text %}
void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = (fragCoord - 0.5 * iResolution.xy) / iResolution.y;

    // Camera setup
    vec3 rayOrigin = vec3(0, 0, -3);  // Camera positioned at (0,0,-3)
    vec3 rayDir = normalize(vec3(uv, 1));  // Forward-facing ray

    // Ray marching parameters
    float totalDistance = 0.0;
    const int maxSteps = 100;
    const float minDist = 0.001;
    const float maxDist = 20.0;
    vec3 hitPoint;
   
    // Ray marching loop
    for (int i = 0; i < maxSteps; i++) {
        hitPoint = rayOrigin + rayDir * totalDistance;
        float dist = sdfSphere(hitPoint, 1.0);  // Distance to the sphere

        if (dist < minDist) break;  // If we are close enough to the surface, stop
        if (totalDistance > maxDist) break;  // If we exceed max distance, stop

        totalDistance += dist;
    }

    // If we hit something, apply shading; otherwise, keep background color
    vec3 col = (totalDistance < maxDist) ? lighting(hitPoint) : vec3(0.2, 0.3, 0.4);
   
    fragColor = vec4(col, 1.0);
}
{% endhighlight %}

You should see something similar to this:

<video muted autoplay loop width="800">
    <source src="{{ site.url }}/assets/raymarch-sphere-shaded.webm" type="video/webm">
</video>

# Mandelbulbs

We can now upgrade our rendering to use something a little more complex then our sphere. 

## SDF

The Mandelbulb is a 3D fractal inspired by the 2D Mandelbrot Set. Instead of working in 2D complex numbers, it uses 
spherical coordinates in 3D space.

The core formula: $$ z→zn+c $$ is extended to 3D using spherical math.

Instead of a sphere SDF, we’ll use an iterative function to compute distances to the fractal surface.

{% highlight text %}
float mandelbulbSDF(vec3 pos) {
    vec3 z = pos;
    float dr = 1.0;
    float r;
    const int iterations = 8;
    const float power = 8.0;

    for (int i = 0; i < iterations; i++) {
        r = length(z);
        if (r > 2.0) break;

        float theta = acos(z.z / r);
        float phi = atan(z.y, z.x);
        float zr = pow(r, power - 1.0);
        dr = zr * power * dr + 1.0;
        zr *= r;
        theta *= power;
        phi *= power;

        z = zr * vec3(sin(theta) * cos(phi), sin(theta) * sin(phi), cos(theta)) + pos;
    }

    return 0.5 * log(r) * r / dr;
}
{% endhighlight %}

This function iterates over complex numbers in 3D space to compute the Mandelbulb structure.

## Raymarching the Mandelbulb

Now, we can take a look at what this produces. We use our newly created SDF to get our hit point. We'll use this 
distance value as well to establish different colours.

{% highlight text %}
void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = (fragCoord - 0.5 * iResolution.xy) / iResolution.y;

    // Camera setup
    vec3 rayOrigin = vec3(0, 0, -4);
    vec3 rayDir = normalize(vec3(uv, 1));

    // Ray marching parameters
    float totalDistance = 0.0;
    const int maxSteps = 100;
    const float minDist = 0.001;
    const float maxDist = 10.0;
    vec3 hitPoint;
   
    // Ray marching loop
    for (int i = 0; i < maxSteps; i++) {
        hitPoint = rayOrigin + rayDir * totalDistance;
        float dist = mandelbulbSDF(hitPoint);  // Fractal distance function

        if (dist < minDist) break;
        if (totalDistance > maxDist) break;

        totalDistance += dist;
    }

    // Color based on distance (simple shading)
    vec3 col = (totalDistance < maxDist) ? vec3(1.0 - totalDistance * 0.1) : vec3(0.1, 0.1, 0.2);

    fragColor = vec4(col, 1.0);
}
{% endhighlight %}

You should see something similar to this:

<video muted autoplay loop width="800">
    <source src="{{ site.url }}/assets/raymarch-mandel.webm" type="video/webm">
</video>

## Rotation

We can't see much with how this object is oriented. By adding some basic animation, we can start to look at the complexities
of how this object is put together. We use the global `iTime` variable here to establish movement:

{% highlight text %}
void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = (fragCoord - 0.5 * iResolution.xy) / iResolution.y;

    // Rotate camera around the fractal using iTime
    float angle = iTime * 0.5;  // Adjust speed of rotation
    vec3 rayOrigin = vec3(3.0 * cos(angle), 0.0, 3.0 * sin(angle)); // Circular path
    vec3 target = vec3(0.0, 0.0, 0.0); // Looking at the fractal
    vec3 forward = normalize(target - rayOrigin);
    vec3 right = normalize(cross(vec3(0, 1, 0), forward));
    vec3 up = cross(forward, right);
   
    vec3 rayDir = normalize(forward + uv.x * right + uv.y * up);

    // Ray marching parameters
    float totalDistance = 0.0;
    const int maxSteps = 100;
    const float minDist = 0.001;
    const float maxDist = 10.0;
    vec3 hitPoint;

    // Ray marching loop
    for (int i = 0; i < maxSteps; i++) {
        hitPoint = rayOrigin + rayDir * totalDistance;
        float dist = mandelbulbSDF(hitPoint);  // Fractal distance function

        if (dist < minDist) break;
        if (totalDistance > maxDist) break;

        totalDistance += dist;
    }

    // Color based on distance (simple shading)
    vec3 col = (totalDistance < maxDist) ? vec3(1.0 - totalDistance * 0.1) : vec3(0.1, 0.1, 0.2);

    fragColor = vec4(col, 1.0);
}
{% endhighlight %}

You should see something similar to this:

<video muted autoplay loop width="800">
    <source src="{{ site.url }}/assets/raymarch-mandel-rotate.webm" type="video/webm">
</video>

## Lights

In order to make our fractal look 3D, we need to be able to compute our surface normals. We'll be using the 
`mandelbulbSDF` function above to accomplish this:

{% highlight text %}
vec3 getNormal(vec3 p) {
    vec2 e = vec2(0.001, 0.0);
    return normalize(vec3(
        mandelbulbSDF(p + e.xyy) - mandelbulbSDF(p - e.xyy),
        mandelbulbSDF(p + e.yxy) - mandelbulbSDF(p - e.yxy),
        mandelbulbSDF(p + e.yyx) - mandelbulbSDF(p - e.yyx)
    ));
}
{% endhighlight %}

We now use this function to light our object using phong shading:

{% highlight text %}
// Basic Phong shading
vec3 phongLighting(vec3 p, vec3 viewDir) {
    vec3 normal = getNormal(p);

    // Light settings
    vec3 lightPos = vec3(2.0, 2.0, -2.0);
    vec3 lightDir = normalize(lightPos - p);
    vec3 ambient = vec3(0.1); // Ambient light

    // Diffuse lighting
    float diff = max(dot(normal, lightDir), 0.0);
   
    // Specular highlight
    vec3 reflectDir = reflect(-lightDir, normal);
    float spec = pow(max(dot(viewDir, reflectDir), 0.0), 16.0); // Shininess factor
   
    return ambient + diff * vec3(1.0, 0.8, 0.6) + spec * vec3(1.0); // Final color
}
{% endhighlight %}

## Soft Shadows

To make the fractal look more realistic, we'll implement soft shadows. This will really enhance how this object looks.

{% highlight text %}
// Soft Shadows (traces a secondary ray to detect occlusion)
float softShadow(vec3 ro, vec3 rd) {
    float res = 1.0;
    float t = 0.02; // Small starting step
    for (int i = 0; i < 24; i++) {
        float d = mandelbulbSDF(ro + rd * t);
        if (d < 0.001) return 0.0; // Fully in shadow
        res = min(res, 10.0 * d / t); // Soft transition
        t += d;
    }
    return res;
}
{% endhighlight %}

## Pulling it all together

We can now pull all of these enhancements together with our main image function:

{% highlight text %}
void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = (fragCoord - 0.5 * iResolution.xy) / iResolution.y;

    // Rotating Camera
    float angle = iTime * 0.5;
    vec3 rayOrigin = vec3(3.0 * cos(angle), 0.0, 3.0 * sin(angle));
    vec3 target = vec3(0.0);
    vec3 forward = normalize(target - rayOrigin);
    vec3 right = normalize(cross(vec3(0, 1, 0), forward));
    vec3 up = cross(forward, right);
    vec3 rayDir = normalize(forward + uv.x * right + uv.y * up);

    // Ray marching
    float totalDistance = 0.0;
    const int maxSteps = 100;
    const float minDist = 0.001;
    const float maxDist = 10.0;
    vec3 hitPoint;

    for (int i = 0; i < maxSteps; i++) {
        hitPoint = rayOrigin + rayDir * totalDistance;
        float dist = mandelbulbSDF(hitPoint);

        if (dist < minDist) break;
        if (totalDistance > maxDist) break;

        totalDistance += dist;
    }

    // Compute lighting only if we hit the fractal
    vec3 color;
    if (totalDistance < maxDist) {
        vec3 viewDir = normalize(rayOrigin - hitPoint);
        vec3 baseLight = phongLighting(hitPoint, viewDir);
        float shadow = softShadow(hitPoint, normalize(vec3(2.0, 2.0, -2.0)));
        color = baseLight * shadow; // Apply shadows
    } else {
        color = vec3(0.1, 0.1, 0.2); // Background color
    }

    fragColor = vec4(color, 1.0);
}
{% endhighlight %}

Finally, you should see something similar to this:

<video muted autoplay loop width="800">
    <source src="{{ site.url }}/assets/raymarch-mandel-rotate-enh.webm" type="video/webm">
</video>

Pretty cool!
