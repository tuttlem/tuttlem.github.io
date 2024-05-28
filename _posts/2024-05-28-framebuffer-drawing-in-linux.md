---
layout: post
title: Framebuffer Drawing in Linux
date: 2024-05-28
comments: false
categories: [ "" ]
---

### Introduction

Some of my favourite graphics programming is done simply with a framebuffer pointer. The simplicity of accessing pixels directly can be alot of fun. In today's article, I'll walk through a couple of different ways that you can achieve this inside of Linux.

### /dev/fb*

Probably the easiest way to get started with writing to the [framebuffer](https://en.wikipedia.org/wiki/Linux_framebuffer) is to start working directly with the `/dev/fb0` device.

{% highlight shell %}
cat /dev/urandom > /dev/fb0
{% endhighlight %}

If your system is anything like mine, this results in `zsh: permission denied: /dev/fb0`. To get around this, add yourself to the "video" group.

{% highlight shell %}
sudo adduser $USER video
{% endhighlight %}

You can now fill your screen with garbage by sending all of those bytes from `/dev/urandom`.

This works, but it's not the best way to get it done.

### Xlib

Next up we'll try again using [Xlib](https://en.wikipedia.org/wiki/Xlib). This isn't exactly what we're after, but I've included this one for completeness.

{% highlight c %}
#include <X11/Xlib.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main() {
    Display *display;
    Window window;
    XEvent event;
    int screen;

    // Open connection to X server
    display = XOpenDisplay(NULL);
    if (display == NULL) {
        fprintf(stderr, "Unable to open X display\n");
        exit(1);
    }

    screen = DefaultScreen(display);

    // Create a window
    window = XCreateSimpleWindow(
      display, 
      RootWindow(display, screen), 
      10, 10, 
      800, 600, 
      1,
      BlackPixel(display, screen), 
      WhitePixel(display, screen)
    );

    // Select kind of events we are interested in
    XSelectInput(display, window, ExposureMask | KeyPressMask);

    // Map (show) the window
    XMapWindow(display, window);

    // Create a simple graphics context
    GC gc = XCreateGC(display, window, 0, NULL);
    XSetForeground(display, gc, BlackPixel(display, screen));

    // Allocate a buffer for drawing
    XImage *image = XCreateImage(
      display, 
      DefaultVisual(display, screen), DefaultDepth(display, screen), 
      ZPixmap, 
      0, 
      NULL, 
      800, 
      600, 
      32, 
      0
    );

    image->data = malloc(image->bytes_per_line * image->height);

    // Main event loop
    while (1) {
        XNextEvent(display, &event);
        if (event.type == Expose) {
            // Draw something to the buffer
            for (int y = 0; y < 600; y++) {
                for (int x = 0; x < 800; x++) {
                    unsigned long pixel = ((x ^ y) & 1) ? 0xFFFFFF : 0x000000; // Simple checker pattern
                    XPutPixel(image, x, y, pixel);
                }
            }

            // Copy buffer to window
            XPutImage(display, window, gc, image, 0, 0, 0, 0, 800, 600);
        }
        if (event.type == KeyPress)
            break;
    }

    // Cleanup
    XDestroyImage(image);
    XFreeGC(display, gc);
    XDestroyWindow(display, window);
    XCloseDisplay(display);

    return 0;
}
{% endhighlight %}

We are performing double-buffering here, but it's only when the event type of `Expose` comes through. This can be useful, but not great if you want to do some animation.

In order to compile this particular example, you need to make sure that you have `libx11-dev` installed.

{% highlight shell %}
sudo apt-get install libx11-dev
gcc -o xlib_demo xlib_demo.c -lX11
{% endhighlight %}

### SDL

For our last example here, we'll use [SDL](https://en.wikipedia.org/wiki/Simple_DirectMedia_Layer) to achieve pixel access to a backbuffer (or framebuffer) by creating an image. In this example we are continouosly flipping the back image onto video memory which allows for smooth animation.

{% highlight c %}
#include <SDL2/SDL.h>
#include <stdio.h>

int main(int argc, char* argv[]) {
    if (SDL_Init(SDL_INIT_VIDEO) < 0) {
        fprintf(stderr, "Could not initialize SDL: %s\n", SDL_GetError());
        return 1;
    }

    SDL_Window *window = SDL_CreateWindow(
      "SDL Demo", 
      SDL_WINDOWPOS_UNDEFINED, 
      SDL_WINDOWPOS_UNDEFINED, 
      800, 
      600, 
      SDL_WINDOW_SHOWN
    );

    if (window == NULL) {
        fprintf(stderr, "Could not create window: %s\n", SDL_GetError());
        SDL_Quit();
        return 1;
    }

    SDL_Renderer *renderer = SDL_CreateRenderer(
      window, 
      -1, 
      SDL_RENDERER_ACCELERATED
    );

    SDL_Texture *texture = SDL_CreateTexture(
      renderer, 
      SDL_PIXELFORMAT_ARGB8888, 
      SDL_TEXTUREACCESS_STREAMING, 
      800, 
      600
    );

    Uint32 *pixels = malloc(800 * 600 * sizeof(Uint32));

    // Main loop
    int running = 1;
    while (running) {
        SDL_Event event;
        while (SDL_PollEvent(&event)) {
            if (event.type == SDL_QUIT) {
                running = 0;
            }
        }

        // Draw something to the buffer
        for (int y = 0; y < 600; y++) {
            for (int x = 0; x < 800; x++) {
                pixels[y * 800 + x] = ((x ^ y) & 1) ? 0xFFFFFFFF : 0xFF000000; // Simple checker pattern
            }
        }

        SDL_UpdateTexture(texture, NULL, pixels, 800 * sizeof(Uint32));
        SDL_RenderClear(renderer);
        SDL_RenderCopy(renderer, texture, NULL, NULL);
        SDL_RenderPresent(renderer);

        SDL_Delay(16); // ~60 FPS
    }

    free(pixels);
    SDL_DestroyTexture(texture);
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit();

    return 0;
}
{% endhighlight %}

Before being able to compile and run this, you need to make sure you have SDL installed on your system.

{% highlight shell %}
sudo apt-get install libsdl2-dev
gcc -o sdl_demo sdl_demo.c -lSDL2
{% endhighlight %}

That's been a few different framebuffer options, all depending on your appetite for dependencies or ease of programming.

