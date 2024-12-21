---
layout: post
title: Waving Flag Animation
date: 2024-12-21
comments: false
categories: [ "" ]
---

# Introduction

In a [previous post]({% post_url 2024-12-19-water-droplet-demo %}) we made a simple water droplet demonstration. This 
is all built on the [vga work]({% post_url 2024-12-19-vga-routines-from-watcom-c %}) that we've already done. 

In today's post, we'll use this information again and put a waving flag demo together. 

The effect that we're looking to produce should look something like this:

<video muted autoplay loop width="800">
    <source src="{{ site.url }}/assets/flag.mp4" type="video/mp4">
</video>

# The Idea

The waving flag effect simulates a piece of fabric rippling in the wind. Here’s the high-level approach:

1. **Flag Bitmap**: Create a bitmap with alternating horizontal stripes of green and white.
2. **Wave Dynamics**: Use trigonometric functions to displace pixels horizontally and vertically, simulating waves.
3. **Buffering**: Use an offscreen buffer to avoid flickering during rendering.

# Building the Flag

The flag is a simple bitmap composed of horizontal stripes alternating between green and white. Here’s how it’s 
generated:

{% highlight c %}
uint8_t* make_flag() {
  int width = 200, height = 100;
  uint8_t *buffer = (uint8_t*) malloc(width * height);

  for (int y = 0; y < height; y++) {
    uint8_t col = (y / 10) % 2 == 0 ? 2 : 15; // Green and white stripes
    memset(buffer + (y * width), col, width);
  }

  return buffer;
}
{% endhighlight %}

Each stripe spans 10 rows, and the alternating colors give the flag a distinctive look.

# Adding the Wave Effect

The waving effect is achieved by modifying the position of each pixel based on sine and cosine functions. Here's the 
core logic:

{% highlight c %}
void draw_flag(double theta, int x, int y, int width, int height, uint8_t *flag, uint8_t *buffer) {
  double t = 0;
  for (int xx = 0; xx < width; xx ++) {
    t = theta;
    double wave_offset = 5 * sin(theta + xx * 0.1);
    for (int yy = 0; yy < height; yy ++) {
      uint16_t px = xx + wave_offset;
      uint16_t py = yy + (2 * sin((theta + xx) * 0.2));
      
      uint16_t o = x + (px + x) + ((py + y) << 8) + ((py + y) << 6);
      uint16_t f = xx + ((yy << 6) + (yy << 5) + (y << 2) << 1);

      uint8_t col = 25 + (3 * sin(t)) + (6 * cos(t));
      
      buffer[o] = flag[f];
      buffer[o-1] = flag[f];
      buffer[o+1] = flag[f];
      buffer[o-320] = flag[f];
      buffer[o+320] = flag[f];

      buffer[o-319] = flag[f];
      buffer[o-321] = flag[f];
      buffer[o+319] = flag[f];
      buffer[o-321] = flag[f];
      t += 0.1;
    }
  }
}
{% endhighlight %}

### Key Features:
- **Wave Dynamics**: The `wave_offset` creates a horizontal ripple effect based on `sin(theta + xx * 0.1)`. A secondary vertical ripple adds realism.
- **Boundary Checks**: Ensures pixels remain within the screen bounds.
- **Direct Pixel Copy**: Pixels are copied from the flag bitmap to the appropriate buffer position.
- **Redundant Pixel Render**: We make sure we render to all surrounding cells so we don't experience tearing

# Main Loop

The main loop ties everything together, handling synchronization, rendering, and input:

{% highlight c %}
int main() {
  uint8_t *back_buffer = (uint8_t *)malloc(64000);
  uint8_t *flag = make_flag();
  double theta = 0;

  set_mcga();
  clear_buffer(0x00, back_buffer);

  while (!kbhit()) {
    draw_flag(theta, 20, 20, 200, 100, flag, back_buffer);

    wait_vsync();
    copy_buffer(vga, back_buffer);
    clear_buffer(0x00, back_buffer);

    theta += 0.1; // Animate the wave
  }

  free(flag);
  free(back_buffer);
  set_text();

  return 0;
}
{% endhighlight %}

### Highlights:

1. **Synchronization**: The `wait_vsync()` call ensures smooth updates.
2. **Animation**: The `theta` value incrementally changes, creating continuous movement.
3. **Keyboard Interrupt**: The `kbhit()` function allows the user to exit gracefully.

# Conclusion

This waving flag effect combines simple algorithms with creative use of VGA mode 13h to create a visually stunning 
effect. By leveraging trigonometry, palette manipulation, and efficient buffer handling, we replicate the mesmerizing motion of a flag in the wind.

You can find the complete code on GitHub [as a gist](https://gist.github.com/tuttlem/5c06e30b4faecbd0a7e4245f693bbb14).

Try it out, tweak the parameters, and share your own effects! There’s a lot of joy in creating beautiful visuals with 
minimal resources.