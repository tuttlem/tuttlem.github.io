---
layout: post
title: Volumetric Fog in Shaders
date: 2025-02-03
comments: false
categories: [ "gl", "shaders", "gpu" ]
---

# Introduction

Rendering realistic 3D environments is more than just defining surfaces—atmospheric effects like fog, mist, and light 
scattering add a layer of depth and realism that makes a scene feel immersive. In this post, we'll explore volumetric 
fog and how we can implement it in our ray-marched Mandelbulb fractal shader.

## What is Volumetric Fog?

Volumetric fog is an effect that simulates light scattering through a medium, such as:

* Mist over a landscape
* Dense fog hiding distant objects
* Hazy light beams filtering through an object

Unlike simple screen-space fog, volumetric fog interacts with geometry, light, and depth, making it appear more 
natural. In our case, we'll use it to create a soft, atmospheric effect around our Mandelbulb fractal.

## How Does It Work?

Volumetric fog in ray marching is achieved by stepping through the scene and accumulating fog density based on 
distance. This is done using:

* **Exponential Fog** – A basic formula that fades objects into the fog over distance.
* **Light Scattering** – Simulates god rays by accumulating light along the ray path.
* **Procedural Noise Fog** – Uses random noise to create a more natural, rolling mist effect.

We'll build each of these effects step by step, expanding on our existing Mandelbulb shader to enhance its atmosphere. 
If you haven't seen them already, suggested reading are the previous articles in this series:

* [The Basics of Shader Programming]({% post_url 2025-02-02-the-basics-of-shader-programming %})
* [Raymarching and Mandelbulbs]({% post_url 2025-02-03-raymarching-and-mandelbulbs %})

# Basis

We will start with the following code, which is our phong shaded, lit, mandelbulb with the camera spinning around it.

{% highlight javascript %}
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

vec3 getNormal(vec3 p) {
    vec2 e = vec2(0.001, 0.0);
    return normalize(vec3(
        mandelbulbSDF(p + e.xyy) - mandelbulbSDF(p - e.xyy),
        mandelbulbSDF(p + e.yxy) - mandelbulbSDF(p - e.yxy),
        mandelbulbSDF(p + e.yyx) - mandelbulbSDF(p - e.yyx)
    ));
}

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

# Depth-based Blending

To create a realistic sense of depth, we can use depth-based blending to gradually fade objects into the fog as they 
move further away from the camera. This simulates how light scatters in the atmosphere, making distant objects appear 
less distinct.

In ray marching, we calculate fog intensity using exponential depth functions like:

$$
\text{fogAmount} = 1.0 - e^{-\text{distance} \times \text{densityFactor}}
$$

where `distance` is how far along the ray we've traveled, and `densityFactor` controls how quickly objects fade into 
fog.

By blending our object’s color with the fog color based on this function, we achieve a smooth atmospheric fade effect. 
Let's implement it in our shader.

{% highlight javascript %}
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
        color = baseLight * shadow;
    } else {
        color = vec3(0.1, 0.1, 0.2); // Background color
    }

    // Apply depth-based exponential fog
    float fogAmount = 1.0 - exp(-totalDistance * 0.15);  
    color = mix(color, vec3(0.5, 0.6, 0.7), fogAmount);  

    fragColor = vec4(color, 1.0);
}
{% endhighlight %}

Once this is running, you should see some fog appear to obscure our Mandelbulb:

<video muted autoplay loop width="800">
    <source src="{{ site.url }}/assets/volfog-distance.webm" type="video/webm">
</video>

# Light Scattering

When light passes through a medium like fog, dust, or mist, it doesn’t just stop—it scatters in different directions, 
creating beautiful effects like god rays or a soft glow around objects. This is known as volumetric light scattering.

In ray marching, we can approximate this effect by tracing secondary rays through the scene and accumulating light 
contribution along the path. The more dense the medium (or the more surfaces the ray encounters), the stronger the 
scattering effect. A simplified formula for this accumulation looks like:

$$
L = \sum_{i=0}^{n} \text{density}(p_i) \cdot \text{stepSize}
$$

where:

* $$ L $$ is the total scattered light along the ray.
* $$ \text{density}(p_i) $$ measures how much fog or medium is present at each step.
* $$ \text{stepSize} $$ controls how frequently we sample along the ray.

By applying this technique, we can simulate light beams filtering through objects, making our Mandelbulb feel immersed 
in an atmospheric environment. 

First we need a function to calcuate our light:

{% highlight javascript %}
float volumetricLight(vec3 ro, vec3 rd) {
    float density = 0.0;
    float t = 0.1;
    for (int i = 0; i < 50; i++) {
        vec3 pos = ro + rd * t;
        float d = mandelbulbSDF(pos);
        if (d < 0.001) density += 0.02; // Accumulate light scattering
        t += 0.1;
    }
    return density;
}
{% endhighlight %}

We also update the `mainImage()` function to add the light accumuation to the resulting pixel:

{% highlight javascript %}
// Compute volumetric lighting effect
float lightScattering = volumetricLight(rayOrigin, rayDir);
color += vec3(1.0, 0.8, 0.5) * lightScattering; // Warm glow
{% endhighlight %}

You can see the god rays through the centre of our fractal:

<video muted autoplay loop width="800">
    <source src="{{ site.url }}/assets/volfog-scatter.webm" type="video/webm">
</video>

With this code we've:

* Shot a secondary ray into the scene which accumulates scattered light
* The denser the fractal, the more light it scatters
* `density += 0.02` controls the intensity of the god rays

# Noise-based Fog

Real-world fog isn’t uniform—it swirls, shifts, and forms dense or sparse patches. To create a more natural effect, we 
can use procedural noise to simulate rolling mist or dynamic fog layers.

Instead of applying a constant fog density at every point, we introduce random variations using a noise function:

$$
\text{fogDensity}(p) = \text{baseDensity} \times \text{noise}(p)
$$

where:

* $$ \text{fogDensity}(p) $$ determines the fog's thickness at position $$ p $$.
* $$ \text{baseDensity} $$ is the overall fog intensity.
* $$ \text{noise}(p) $$ generates small-scale variations to make fog look natural.

By sampling noise along the ray, we can create wispy, uneven fog that behaves more like mist or smoke, enhancing the 
realism of our scene. Let’s implement this effect next.

We'll add procedural noise to simulate smoke or rolling mist.

{% highlight javascript %}
float noise(vec3 p) {
    return fract(sin(dot(p, vec3(12.9898, 78.233, 45.164))) * 43758.5453);
}

float proceduralFog(vec3 ro, vec3 rd) {
    float fogDensity = 0.0;
    float t = 0.1;
    for (int i = 0; i < 50; i++) {
        vec3 pos = ro + rd * t;
        fogDensity += noise(pos * 0.5) * 0.02;
        t += 0.1;
    }
    return fogDensity;
}
{% endhighlight %}

Finally, we blend the noise fog into the final colour inside of the `mainImage()` function:

{% highlight javascript %}
// Apply procedural noise fog
float fog = proceduralFog(rayOrigin, rayDir);
color = mix(color, vec3(0.7, 0.8, 1.0), fog);
{% endhighlight %}

After these modifications, you should start to see the fog moving as we rotate:

<video muted autoplay loop width="800">
    <source src="{{ site.url }}/assets/volfog-noise.webm" type="video/webm">
</video>

The final version of this shader can be found [here](https://gist.github.com/tuttlem/81d23f992987bac8a3f4ecf14204bd98).

# Conclusion

By adding volumetric effects to our ray-marched Mandelbulb, we've taken our scene from a simple fractal to a rich, 
immersive environment.

These techniques not only enhance the visual depth of our scene but also provide a foundation for more advanced 
effects like clouds, smoke, fire, or atmospheric light absorption.

