---
layout: post
title: Drawing with Cairo
date: 2015-02-08
comments: false
categories: [ "cairo", "c", "graphics" ]
---

[Cairo](http://cairographics.org/) is a cross platform 2D graphics library. It's got a wide range of features that are exposed through a solid and easy to use API. From the website:

> The cairo API provides operations similar to the drawing operators of PostScript and PDF. Operations in cairo including stroking and filling cubic Bézier splines, transforming and compositing translucent images, and antialiased text rendering. All drawing operations can be transformed by any affine transformation (scale, rotation, shear, etc.)

In today's post, I'm going to re-implement a very simple version of the old windows screensaver, [Mystify](https://www.youtube.com/watch?v=RrYQK6jSbY4). In fact, it's not even going to look as cool as the one in the video but it will take you through basic drawing with Cairo and animation using [GTK+](http://www.gtk.org/) and [GDK](https://developer.gnome.org/gdk3/unstable/).

### Getting your feet wet

If you don't want to dive right into doing animation with Cairo, I suggest that you take a look at the [FAQ](http://cairographics.org/FAQ/). Up there is a section on [what a minimal C program looks like](http://cairographics.org/FAQ/#minimal_C_program). For reference, I have included it below. You can see that it's quite static in nature; writing a PNG of the result at the end:

{% highlight c %}
#include <cairo.h>

int main (int argc, char *argv[]) {
  cairo_surface_t *surface =
     cairo_image_surface_create(
      CAIRO_FORMAT_ARGB32, 
      240, 
      80);

  cairo_t *cr =
     cairo_create(surface);

  cairo_select_font_face(
    cr, 
    "serif", 
    CAIRO_FONT_SLANT_NORMAL, 
    CAIRO_FONT_WEIGHT_BOLD
  );

  cairo_set_font_size(cr, 32.0);
  cairo_set_source_rgb(cr, 0.0, 0.0, 1.0);
  cairo_move_to(cr, 10.0, 50.0);
  cairo_show_text(cr, "Hello, world");

  cairo_destroy(cr);
  cairo_surface_write_to_png(
    surface, 
    "hello.png"
  );
  cairo_surface_destroy(surface);
  return 0;
}
{% endhighlight %}

### Building Cairo applications

There's a shopping list of compiler and linker switches when building with Cairo, GTK+ and GDK. [pkg-config](http://www.freedesktop.org/wiki/Software/pkg-config/) has been a great help here, so here are the `CFLAGS` and `LFLAGS` definitions from my `Makefile`:

{% highlight text %}
CFLAGS := -g -Wall `pkg-config --cflags gtk+-3.0 gdk-3.0 cairo`
LFLAGS := `pkg-config --libs gtk+-3.0 gdk-3.0 cairo`
{% endhighlight %}

### Setting up the UI

First job is to create a window that will host our drawing. This is all pretty standard boilerplate for any GTK+ application. 

{% highlight c %}
int main(int argc, char *argv[]) {
  GtkWidget *window;

  gtk_init(&argc, &argv);
  init_verts();

  window = gtk_window_new(
    GTK_WINDOW_TOPLEVEL
  );

  darea = gtk_drawing_area_new();
    gtk_container_add(
    GTK_CONTAINER(window), 
    darea
  );

  g_signal_connect(
    G_OBJECT(darea), 
    "draw", 
    G_CALLBACK(on_draw_event), 
    NULL
  );

  g_signal_connect(
    window, 
    "destroy", 
    G_CALLBACK(gtk_main_quit), 
    NULL
  );  

  gtk_window_set_position(
    GTK_WINDOW(window), 
    GTK_WIN_POS_CENTER
  );

  gtk_window_set_default_size(
    GTK_WINDOW(window), 
    WIN_WIDTH, 
    WIN_HEIGHT
  );

  gtk_window_set_title(
    GTK_WINDOW(window), 
    "Lines"
  );

  gtk_widget_show_all(window);

  (void)g_timeout_add(
    33, 
    (GSourceFunc)mystify_animate, 
    window
  );

  gtk_main();

  return 0;
}
{% endhighlight %}

The parts to really take note of here is the creation of our drawable, and it getting connected to the window:

{% highlight c %}
darea = gtk_drawing_area_new();
gtk_container_add(
  GTK_CONTAINER(window), 
  darea
);
{% endhighlight %}

Attaching our custom draw function to the `draw` signal with [g_signal_connect](https://developer.gnome.org/gobject/unstable/gobject-Signals.html#g-signal-connect):

{% highlight c %}
g_signal_connect(
  G_OBJECT(darea), 
  "draw", 
  G_CALLBACK(on_draw_event), 
  NULL
);
{% endhighlight %}

Setting up a timer with [g_timeout_add](https://developer.gnome.org/glib/stable/glib-The-Main-Event-Loop.html#g-timeout-add) to continually call the animation function:

{% highlight c %}
(void)g_timeout_add(
  33, 
  (GSourceFunc)mystify_animate, 
  window
);
{% endhighlight %}

### Drawing

The Mystify effect is just a handful of vertices bouncing around the screen with lines connecting them. Really quite simple and we can get away with a basic data structure to defined this:

{% highlight c %}
struct tag_mystify_vert {
  int x, y;     /* x and y positions */
  int vx, vy;   /* x and y velocities */
  double r, g, b;  /* colour components */
};

typedef struct tag_mystify_vert mystify_vert;

mystify_vert verts[N_VERTS];
{% endhighlight %}

Drawing the structure is just enumerating over the array defined above and drawing the lines:

{% highlight c %}
static gboolean on_draw_event(
GtkWidget *widget, 
cairo_t *cr, 
gpointer user_data) {
  int n;

  /* clear the background to black */
  cairo_set_source_rgb(cr, 0, 0, 0);
  cairo_paint(cr);

  /* draw lines between verts */
  for (n = 0; n < (N_VERTS - 1); n ++) {
    cairo_set_source_rgb(
      cr, 
      verts[n].r, 
      verts[n].g, 
      verts[n].b
    );

    cairo_move_to(
      cr, 
      verts[n].x, 
      verts[n].y
    );
    cairo_line_to(
      cr, 
      verts[n + 1].x, 
      verts[n + 1].y
    );

    cairo_set_line_width(cr, 1);
    cairo_stroke(cr);
  }

  /* draw a line between the first and last vert */
  n = N_VERTS - 1;

  cairo_set_source_rgb(
    cr, 
    verts[n].r, 
    verts[n].g, 
    verts[n].b
  );

  cairo_move_to(
    cr, 
    verts[n].x, 
    verts[n].y
  );

  cairo_line_to(
    cr, 
    verts[0].x, 
    verts[0].y
  );

  cairo_set_line_width(cr, 1);
  cairo_stroke(cr);

  return FALSE;
}
{% endhighlight %}

Pretty basic. We set our draw colour with [cairo_set_source_rgb](http://www.cairographics.org/manual/cairo-cairo-t.html#cairo-set-source-rgb), [cairo_move_to](http://www.cairographics.org/manual/cairo-Paths.html#cairo-move-to) and [cairo_line_to](http://www.cairographics.org/manual/cairo-Paths.html#cairo-line-to) handle our start and end points for the lines. [cairo_stroke](http://www.cairographics.org/manual/cairo-cairo-t.html#cairo-stroke) makes the doughnuts.

### Animating

Finally, we animate the structure. Nothing really of interest in this code block except for how we notify the UI that we need a redraw.

{% highlight c %}
gboolean mystify_animate(GtkWidget *window) {
  animate_verts();

  gtk_widget_queue_draw_area(
  window, 
  0, 0, 
  WIN_WIDTH, WIN_HEIGHT
  );

  return TRUE;
}
{% endhighlight %}

[gtk_widget_queue_draw_area](https://developer.gnome.org/gtk3/stable/GtkWidget.html#gtk-widget-queue-draw-area) invalidates the defined region and forces the redraw for us.

### Putting it all together

The source for this most unexciting version of the Mystify effect can be found [here](https://gist.github.com/tuttlem/99f563e8f5b7cb04cf3d).