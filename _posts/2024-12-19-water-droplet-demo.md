---
layout: post
title: Water droplet demo
date: 2024-12-19
comments: false
categories: [ "" ]
---

# Introduction

Visual effects like water droplets are mesmerizing, and they showcase how simple algorithms can produce complex, beautiful 
animations. In this article, I’ll walk you through creating a water droplet effect using VGA mode 13h.

We'll rely on some of the code that we developed in the [VGA routines from Watcom C]({% post_url 2024-12-19-vga-routines-from-watcom-c %}) 
article for VGA setup and utility functions, focusing on how to implement  the effect itself. 

The effect that we're looking to produce should look something like this:

<video muted autoplay loop width="800">
    <source src="{{ site.url }}/assets/water.mp4" type="video/mp4">
</video>

# The Idea

The water droplet effect simulates circular ripples spreading out from random points on the screen. Here’s the high-level approach:
1. **Drops**: Represent each drop with a structure containing its position, energy, and ripple generation.
2. **Drawing Ripples**: Use trigonometry to create circular patterns for each ripple generation.
3. **Blur Effect**: Smooth the buffer to simulate water’s fluid motion.
4. **Palette**: Set a blue-themed palette to enhance the watery feel.

# Setting the Water Palette

First, we set a blue-tinted gradient palette. Each color gradually transitions from dark blue to bright blue.

{% highlight c %}
void set_water_palette() {
  uint16_t i;
  uint8_t r, g, b;

  for (i = 0; i < 256; i++) {
      r = i >> 2;  // Dim red
      g = i >> 2;  // Dim green
      b = 63;      // Maximum blue

      set_palette(i, r, g, b);
  }
}
{% endhighlight %}
    
# Representing Drops

Each drop is represented by a structure that tracks:
- `(x, y)`: The origin of the drop.
- `e`: Energy, which fades with time.
- `g`: Current ripple generation.

{% highlight c %}
struct drop {
  int x;    /* original x-coordinate */
  int y;    /* original y-coordinate */
  int e;    /* energy left in the drop */
  int g;    /* current generation */
};

struct drop drops[N_DROPS];
{% endhighlight %}

# Creating and Advancing Drops

Drops are reset with random positions, maximum energy, and zero ripple generation:

{% highlight c %}
void reset_drop(struct drop *d) {
  d->x = rand() % 320;
  d->y = rand() % 200;
  d->e = 200;
  d->g = 0;
}
{% endhighlight %}

Each frame, we reduce the drop's energy and increment its generation. When energy is exhausted, the drop stops 
producing ripples:

{% highlight c %}
void advance_drop(struct drop *d) {
  if (d->e > 0) {
    d->e--;
    d->g++;
  } else {
    d->g = 0;
  }
}
{% endhighlight %}

# Drawing Ripples

Ripples are drawn using polar coordinates. We calculate `x` and `y` offsets using cosine and sine functions for each 
angle and scale by the current generation.

{% highlight c %}
void draw_drop(struct drop *d, uint8_t *buffer) {
  // if this droplet still has some energy
  if (d->e > 0) {
    // 0 to 2π
    for (float rad = 0.0f; rad < 6.28f; rad += 0.05f) { 
      // x, y co-ordinates to go around the circle
      int xx = (int)(cos(rad) * (float)d->g);
      int yy = (int)(sin(rad) * (float)d->g);

      // translate them into the field
      xx += d->x;
      yy += d->y;

      // clip them to the visible field
      if ((xx >= 0) && (xx < 320) && (yy >= 0) && (yy < 200)) {
        uint16_t offset = xx + (yy << 6) + (yy << 8);  // VGA offset
        uint16_t c = buffer[offset];

        // clamp the pixel colour to 255
        if ((c + d->e) > 255) {
          c = 255;
        } else {
          c += d->e;
        }

        // set the pixel
        buffer[offset] = c;
      }
    }
  }
}
{% endhighlight %}

The colour that is rendered to the buffer is additive. We take the current colour at the pixel position, and add to it 
giving the droplets a sense of collision when they overlap.

# Simulating Fluid Motion

A blur effect smooths the ripples, blending them into neighboring pixels for a more fluid appearance. This is done by 
averaging surrounding pixels.

{% highlight c %}
void blur_buffer(uint8_t *buffer) {
  memset(buffer, 0, 320);         // Clear top border
  memset(buffer + 63680, 0, 320); // Clear bottom border

  for (uint16_t i = 320; i < 63680; i++) {
    buffer[i] = (
        buffer[i - 321] + buffer[i - 320] + buffer[i - 319] +
        buffer[i - 1]   + buffer[i + 1]   +
        buffer[i + 319] + buffer[i + 320] + buffer[i + 321]
    ) >> 3;  // Average of 8 neighbors
  }
}
{% endhighlight %}

# Main Loop

The main loop handles:
1. Adding new drops randomly.
2. Advancing and drawing existing drops.
3. Applying the blur effect.
4. Rendering the buffer to the VGA screen.

{% highlight c %}
int main() {
  uint8_t *back_buffer = (uint8_t *)malloc(64000);
  uint8_t drop_index = 0;

  set_mcga();              // Switch to VGA mode
  set_water_palette();     // Set blue gradient
  clear_buffer(0x00, back_buffer); // Clear the back buffer

  while (!kbhit()) { // Continue until a key is pressed

    // Randomly reset a drop
    if ((rand() % 10) == 0) {
      reset_drop(&drops[drop_index]);

      drop_index++;
      drop_index %= N_DROPS;
    }

    // Process and draw each drop
    for (int i = 0; i < N_DROPS; i++) {
      advance_drop(&drops[i]);
      draw_drop(&drops[i], back_buffer);
    }

    blur_buffer(back_buffer);   // Apply the blur effect

    wait_vsync();               // Synchronize with vertical refresh
    copy_buffer(vga, back_buffer); // Copy back buffer to screen
    clear_buffer(0x00, back_buffer); // Clear back buffer for next frame
  }

  free(back_buffer);
  set_text(); // Return to text mode

  return 0;
}
{% endhighlight %}

# Conclusion

This water droplet effect combines simple algorithms with creative use of VGA mode 13h to create a visually stunning effect. By leveraging circular ripples, energy fading, and a blur filter, we replicate the mesmerizing motion of water.

You can find the complete code on GitHub [as a gist](https://gist.github.com/tuttlem/e4546f09252406624615283bd871d67e).

Try it out, tweak the parameters, and share your own effects! There’s a lot of joy in creating beautiful visuals with minimal resources.
