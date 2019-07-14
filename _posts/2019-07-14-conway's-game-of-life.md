---
layout: post
title: Conway's Game of Life
date: 2019-07-14
comments: false
categories: [ "c", "game of life", "conway" ]
---

Game of life is a cellular simulation; not really a game that is played, but more a sequence that is observed. In today's article, I'll walk through the rules and a simple C implementation.

### How to play

There are some simple rules that the game must abide by. 

The universe that it is played within is an orthogonal cartesian grid of squares that define if a cell is dead or if it's alive. The lifespan of the cells is determined by the following rules:
* Any cell that's alive with fewer than two live neighbours dies (underpopulation)
* Any cell that's alive with two or three live neighbours lives on to the next generation
* Any cell that's alive with more than three live neighbours dies (overpopulation)
* Any dead cell that has three live neighbours becomes alive (reproduction)

And, that's it.

### Implementation

The pitch where the game is played would be a pretty simple buffer of 1's and 0's. 1 would define "alive", and 0 would define "dead":

{% highlight c %}
#define UV_WIDTH 64
#define UV_HEIGHT 32

unsigned char *universe;


universe = (unsigned char *) malloc(UV_HEIGHT * UV_WIDTH * sizeof(unsigned char));
{% endhighlight %}

So, a buffer of memory for a defined width and height will do the job.

The remainder of the process could be split into the following:

* Seed the universe
* Permute the universe
* Render the universe

### Seed

Chicken or the egg isn't asked here. We just use `srand` and `rand` to play god for us:

{% highlight c %}
void universe_seed(unsigned char *u, int width, int height) {
  int i;

  for (i = 0; i < (width * height); i ++) {
    u[i] = rand() % 9 == 0;
  }
}
{% endhighlight %}

For every cell, we'll get a random number from `rand`. If that number is divisible by 9, we'll mark the cell as alive.

There are much more clever ways to seed the universe, in such a way that the rules of the game keep the generations running for ever with very clever patterns.

### Permute

Actually making the universe kick along between frames, is simply applying the rules to a buffer of states. This buffer of states needs to be considered all in the same move; so we can't mutate the original buffer sequentially.

{% highlight c %}
void universe_permute(unsigned char *u, int width, int height) {
  int x, y, n, l;
  unsigned char *r = (unsigned char *)malloc(width * height);

  memcpy(r, u, width * height);

  for (y = 0; y < height; y ++) {
    for (x = 0; x < width; x ++) {
      l = u[x + (y * width)];
      n = count_live_neighbours(u, width, height, x, y);

      if (l & ((n < 2) || (n > 3))) {
        r[x + (y * width)] = 0;
      } else if (!l & (n == 3)) {
        r[x + (y * width)] = 1;
      }
    }
  }

  memcpy(u, r, width * height);
  free(r);
}
{% endhighlight %}

A copy of the game buffer is made, first up. This is what we'll actually write the *next* buffer states to; leaving the current buffer intact.

Following the rules of the game:

{% highlight c %}
  if (l & ((n < 2) || (n > 3))) {
    r[x + (y * width)] = 0;
  } else if (!l & (n == 3)) {
    r[x + (y * width)] = 1;
  }
{% endhighlight %}

Here, we see the overpopulation, underpopulation, and reproduction rules in action.

The number of neighbours, is counted with a difference:

{% highlight c %}
int count_live_neighbours(unsigned char *u, int width, int height, int x, int y) {
  /* clip the bounds */
  int x1 = (x - 1) % width;
  int x2 = (x + 1) % width;
  int y1 = (y - 1) % height;
  int y2 = (y + 1) % height;

  return u[x1 + (y1 * width)] +
         u[x1 + (y2 * width)] +
         u[x2 + (y1 * width)] +
         u[x2 + (y2 * width)] +
         u[x  + (y1 * width)] +
         u[x  + (y2 * width)] +
         u[x1 + (y  * width)] +
         u[x2 + (y  * width)];
}
{% endhighlight %}

The `x` and `y` values are clipped to the `width` and `height` values. This means that if you fall off the right-hand side of the universe, you'll magically appear back on the left-hand side. In the same way - top to bottom, etc.

A neighbour check must look at all 8 cells that surround the cell in question. If a cell is alive, it's value will be `1`; this gives us a really simple hack of adding all of these values together. This now tells us the number of neighbours to this cell.

### Rendering

To the terminal.

Always, to the terminal.

You can render anywhere you want. For my example implementation, I've used the console.

{% highlight c %}
void universe_render(unsigned char *u, int width, int height) {
  int x, y;

  erase();

  for (y = 0; y < height; y ++) {
    for (x = 0; x < width; x ++) {
      if (u[x + (y * width)]) {
        mvprintw(y + 1, x, "*");
      } else {
        mvprintw(y + 1, x, ".");
      }
    }
  }
}
{% endhighlight %}

### Finishing up

Here's a grab of the console.

{% highlight text %}
Alive: 81    Dead: 1967
........................*.......................................
.......................*.*.............***......................
.......................*.*...........*.***......................
........................*...........*...........................
...................................**...........................
..........***.......................**.........**...............
.....................................*.........**...............
........*.....*.................................................
........*.....*.......................................*.........
........*.....*.......................................*.........
.***..................................................*.........
..........***...................................................
................**..............................................
...............*..*.............................................
................**..............................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
................................................................
..........................*.....................................
.........................***....................................
........................***.*...................................
.......................*....**..................................
......................**....*...................................
.......................*.**.*...................................
*...........................*..................................*
.*.......................***.........................**........*
*.........................*.........................*..*........
.....................................................**.........
{% endhighlight %}

The full code for this article can be found [here](https://github.com/tuttlem/conway).

