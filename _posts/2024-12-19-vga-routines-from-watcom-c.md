---
layout: post
title: VGA routines from Watcom C
date: 2024-12-19
comments: false
categories: [ "vga", "watcom", "oldschool" ]
---

# Introduction

The VGA era was all about getting the most out of limited hardware. It required clever tricks to push pixels and make 
things move. To make it easier to work with VGA and related concepts, I put together a library called freak.

This library includes tools for VGA handling, keyboard input, vector and matrix math, and 
fixed-point math. In this post, I’ll go through each part of the library and explain how it works, with examples.

Be warned - this stuff is old! You'll need a Watcom Compiler as well as a dos-like environment to be able to run any 
of your code. I've previously written about getting Watcom up and running with DosBox. If you want to get this running 
you can read the following:

* [32bit Dos Development with Open Watcom]({% post_url 2015-10-04-32bit-dos-development-with-open-watcom %})
* [Inline Assembly with Watcom]({% post_url 2015-10-04-inline-assembly-with-watcom %})
* [Using wcc386 and TASM Together]({% post_url 2015-10-08-using-wcc386-and-tasm-together %})

The code that this article outlines is available [here](https://github.com/tuttlem/freak).

# Video routines

First of all, we're going to take care of shifting in and out of old [mode 13](https://en.wikipedia.org/wiki/Mode_13h).

## Setting a video mode

To shift in and out of video modes we use the `int 10h` bios interrupt.

{% highlight c %}
#define BIOS_VIDEO_80x25            0x03
#define BIOS_VIDEO_320x200x256      0x13

void freak_set_video(uint8_t mode);
#pragma aux freak_set_video = \
  "mov    ah, 0"              \
  "int    0x10"               \
  parm [al];

/** Sets the video to 320x240x256 */
inline void freak_set_mcga() { freak_set_video(BIOS_VIDEO_320x200x256); }

/** Sets the video to 80x25 text */
inline void freak_set_text() { freak_set_video(BIOS_VIDEO_80x25); }
{% endhighlight %}

Passing the video mode into `al` and setting `ah` to `0` allows us to change modes.

We also need to define where we want to draw to. VGA maps to `A000:0000` in real mode. Because we're in protected mode
(thanks to [DOS/4G](https://en.wikipedia.org/wiki/DOS/4G)) we set our pointer to `0xA0000`.

{% highlight c %}
// header
extern uint8_t *freak_vga;

// source
uint8_t *freak_vga = (uint8_t *)0x0a0000;
{% endhighlight %}

## Working with buffers

We defined the pointer `freak_vga` as a location in memory. From that point in memory for the next 64,000 bytes (we're 
using `320x200x8` which is 64k) are all of the pixels on the screen.

That means we can treat the screen like any old memory buffer. That also means that we can define virtual buffers as 
long as we have 64k to spare; which we do.

You could imagine doing something like this pretty easily:

{% highlight c %}
uint8_t *back_buffer = (uint8_t *)malloc(64000);
{% endhighlight %}

We could use `memset` and `memcpy` to work with these buffers; or we would write our own optimised implementations to 
use instructions to move a double at a time (like `movsd` and `stosd`):

{% highlight c %}
/** Clears a buffer with a value */
void freak_clear_buffer(uint8_t c, uint8_t *buf);
#pragma aux freak_clear_buffer = \
  "mov  ah, al"                  \
  "mov  bx, ax"                  \
  "shl  eax, 16"                 \
  "mov  ax, bx"                  \
  "mov  ecx, 16000"              \
  "rep  stosd"                   \
  modify [eax ebx ecx]           \
  parm [al] [edi]; 

/** Copies a buffer onto another */
void freak_copy_buffer(uint8_t *dest, uint8_t *src);
#pragma aux freak_copy_buffer = \
  "mov  ecx, 16000"             \
  "rep  movsd"                  \
  modify [ecx]                  \
  parm [edi] [esi];
{% endhighlight %}

Before flipping a back buffer onto the vga surface, we wait for the vsync to complete. This removes any flicker.

{% highlight c %}
/** Waits for a vertical sync to occur */
void freak_wait_vsync();
#pragma aux freak_wait_vsync = \
  "mov  dx, 03dah"             \
  "@@vsync1:"                  \
  "in   al, dx"                \
  "test al, 08h"               \
  "jz   @@vsync1"              \
  "@@vsync2:"                  \
  "in   al, dx"                \
  "test al, 08h"               \
  "jnz  @@vsync2"              \
  modify [ax dx];
{% endhighlight %}

## Colours

In mode13, we are given 256 colour slots to where we can control the red, green, and blue component. Whilst the default 
palette does provide a vast array of different colours; it kinda sucks.

In order to set the r, g, b components of a colour we first need to write the colour index out to port `0x3c8`. We then 
write the r, g, and b components sequentially out to `0x3c9`.

{% highlight c %}
void freak_set_palette(uint8_t c, uint8_t r, uint8_t g, uint8_t b);
#pragma aux freak_set_palette = \
  "mov  dx, 0x3c8"              \
  "out  dx, al"                 \
  "mov  dx, 0x3c9"              \
  "mov  al, bh"                 \
  "out  dx, al"                 \
  "mov  al, bl"                 \
  "out  dx, al"                 \
  "mov  al, ch"                 \
  "out  dx, al"                 \
  parm [al] [bh] [bl] [ch]      \
  modify [dx];
{% endhighlight %}

To read the current r, g, b values of a colour slot, we write the colour index out to `0x3c7`. Then we can sequentially 
read the values from `0x3c9`.

{% highlight c %}
void freak_get_palette(uint8_t c, uint8_t *r, uint8_t *g, uint8_t *b);
#pragma aux freak_get_palette = \
  "mov  dx, 0x3c7"              \
  "out  dx, al"                 \
  "mov  dx, 0x3c9"              \
  "in   al, dx"                 \
  "mov  [ebx], al"              \
  "in   al, dx"                 \
  "mov  [ecx], al"              \
  "in   al, dx"                 \
  "mov  [edi], al"              \
  parm [dx] [ebx] [ecx] [edi]   \
  modify [dx];
{% endhighlight %}

# Keyboard

Working with the keyboard is another BIOS service in `16h` that we can call on.

Simple re-implementations of `getch` and `kbhit` can be produced with a little assembly language:

{% highlight c %}
unsigned short freak_getch();
#pragma aux freak_getch = \
  "mov    ah, 0"          \
  "int    0x16"           \
  value [ax];

unsigned char freak_kbhit();
#pragma aux freak_kbhit = \
  "mov    ah, 1"          \
  "int    0x16"           \
  "jz     @@kbhit_no_key" \
  "mov    al, 1"          \
  "jmp    @@kbhit_done"   \
  "@@kbhit_no_key:"       \
  "xor    al, al"         \
  "@@kbhit_done:"         \
  value [al];
{% endhighlight %}

# Fixed point math

The [fixed point article]({% post_url 2015-01-10-fixed-point-numbers %}) that I had previously written walks you through 
the basic mechanics of the topic. The bit lengths of the whole and fractional parts are pretty small; and unusable. So 
we're going to use this technique, but scale it up.

## Conversions

First of all, we need to be able to go from the "C type world" (the world of `int` and `double`, for instance) into the 
"fixed point world". We also need to make our way back:

{% highlight c %}
#define MATH_FRAC_BITS    16
#define MATH_FRAC_MAG     65536.0

#define int_to_fixed(x)       ((x) << MATH_FRAC_BITS)
#define double_to_fixed(x)    ((fixed)(x * MATH_FRAC_MAG + 0.5))
#define fixed_to_int(x)       ((x) >> MATH_FRAC_BITS)
#define fixed_to_double(x)    (((double)x) / MATH_FRAC_MAG)

/* fixed-point data type */
typedef long fixed;
{% endhighlight %}

These macros as just simple helpers to clean up our code when defining numbers. 

## Constants

Next up, we define some important constants:

{% highlight c %}
#define ONE             int_to_fixed(1)
#define FIXED_ZERO      0
#define FIXED_ONE       int_to_fixed(1)
#define FIXED_NEGONE    int_to_fixed(-1)
#define FIXED_PI        205887L
#define FIXED_2PI       411775L
#define FIXED_E         178144L
#define FIXED_ROOT2      74804L
#define FIXED_ROOT3     113512L
#define FIXED_GOLDEN    106039L
{% endhighlight %}

Each of these comes in handy for different mathematical operations that we'll soon walk through.

## Trig

We need trigonometry to do fun stuff. To speed up our code, we pre-compute a sine and cosine table that is already in 
our fixed-point number format:

{% highlight c %}
// header
extern fixed _fixed_sin[];
extern fixed _fixed_cos[];

// theta radian (fixed) to angle index
#define fixed_to_theta(t)     (fixed_to_int(fixed_mul(fixed_div(t, FIXED_2PI), int_to_fixed(1024))) & 1023)

#define fixed_sin(t)          (_fixed_sin[t & 1023])
#define fixed_cos(t)          (_fixed_cos[t & 1023])
#define fixed_tan(t)          (fixed_div(fixed_sin(t) << 16, fixed_cos(t)) >> 16)

// definitions
fixed _fixed_sin[1024] = {
             0,        402,        804,       1206,       1608,       2010, 
          2412,       2814,       3216,       3617,       4019,       4420, 
                .    .    .
                .    .    .
                .    .    .
};

fixed _fixed_cos[1024] = {
         65536,      65535,      65531,      65525,      65516,      65505, 
         65492,      65476,      65457,      65436,      65413,      65387, 
                .    .    .
                .    .    .
                .    .    .
};
{% endhighlight %}

Our trig tables are based around a nerd number of `1,024` making this a little easier to reason about and giving us an 
acceptable level of precision between fractions of radians for what we need.

These are then nicely wrapped up in macros.

## Operations

The fixed multiply is a very simple integer-based operation (by design):

{% highlight c %}
fixed fixed_mul(fixed a, fixed b);
#pragma aux fixed_mul =   \
  "imul   edx"            \
  "add    eax, 8000h"     \
  "adc    edx, 0"         \
  "shrd   eax, edx, 16"   \
  parm caller [eax] [edx] \
  value [eax]             \
  modify [eax edx];
{% endhighlight %}

Division is also quite similar:

{% highlight c %}
fixed fixed_div(fixed a, fixed b);
#pragma aux fixed_div =   \
  "xor    eax, eax"       \
  "shrd   eax, edx, 16"   \
  "sar    edx, 16"        \
  "idiv   ebx"            \
  parm caller [edx] [ebx] \
  value [eax]             \
  modify [eax ebx edx];
{% endhighlight %}

Square roots come in two flavours. A quicker by less precise version ("fast") or the longer iterative approach. 

{% highlight c %}
fixed fixed_sqrt_fast(fixed n);
#pragma aux fixed_sqrt_fast = \
  "xor  eax, eax"             \
  "mov  ebx, 40000000h"       \
  "sqrtLP1: "                 \
  "mov edx,  ecx"             \
  "sub  edx, ebx"             \
  "jl   sqrtLP2"              \
  "sub  edx, eax"             \
  "jl   sqrtLP2"              \
  "mov  ecx, edx"             \
  "shr  eax, 1"               \
  "or   eax, ebx"             \
  "shr  ebx, 2"               \
  "jnz  sqrtLP1"              \
  "shl  eax, 8"               \
  "jmp  sqrtLP3"              \
  "sqrtLP2: "                 \
  "shr  eax, 1"               \
  "shr  ebx, 2"               \
  "jnz  sqrtLP1"              \
  "shl  eax, 8"               \
  "sqrtLP3: "                 \
  "nop"                       \
  parm caller [ecx]           \
  value [eax]                 \
  modify [eax ebx ecx edx];

fixed fixed_sqrt(fixed n);
#pragma aux fixed_sqrt =   \
  "xor  eax, eax"          \
  "mov  ebx, 40000000h"    \
  "sqrtHP1: "              \
  "mov  edx, ecx"          \
  "sub  edx, ebx"          \
  "jb   sqrtHP2"           \
  "sub  edx, eax"          \
  "jb   sqrtHP2"           \
  "mov  ecx,edx"           \
  "shr  eax, 1"            \
  "or   eax, ebx"          \
  "shr  ebx, 2"            \
  "jnz  sqrtHP1"           \
  "jz   sqrtHP5"           \
  "sqrtHP2: "              \
  "shr  eax, 1"            \
  "shr  ebx, 2"            \
  "jnz  sqrtHP1"           \
  "sqrtHP5:"               \
  "mov  ebx, 00004000h"    \
  "shl  eax, 16"           \
  "shl  ecx, 16"           \
  "sqrtHP3: "              \
  "mov  edx, ecx"          \
  "sub  edx, ebx"          \
  "jb   sqrtHP4"           \
  "sub  edx, eax"          \
  "jb   sqrtHP4"           \
  "mov  ecx, edx"          \
  "shr  eax, 1"            \
  "or   eax, ebx"          \
  "shr  ebx, 2"            \
  "jnz  sqrtHP3"           \
  "jmp  sqrtHP6"           \
  "sqrtHP4: "              \
  "shr  eax, 1"            \
  "shr  ebx, 2"            \
  "jnz  sqrtHP3"           \
  "sqrtHP6: "              \
  "nop"                    \
  parm caller [ecx]        \
  value [eax]              \
  modify [eax ebx ecx edx];
{% endhighlight %}

Finally, some helpers that are usages of existing code that we've written are squaring a number, and putting a number 
under `1`:

{% highlight c %}
fixed fixed_sqr(fixed n);
#pragma aux fixed_sqr = \
  "imul   eax"          \
  "add    eax, 8000h"   \
  "adc    edx, 0"       \
  "shrd   eax, edx, 16" \
  parm caller [eax]     \
  value [eax]           \
  modify [eax edx];

fixed fixed_one_over(fixed n);
#pragma aux fixed_one_over = \
  "xor    eax, eax"          \
  "mov    edx, 1"            \
  "idiv   ebx"               \
  parm caller [ebx]          \
  value [eax]                \
  modify [eax ebx edx];
{% endhighlight %}

# Vectors 

A 3-space vector has an x, y, and z component. 

$$ \mathbf{v} = \begin{bmatrix} x \\ y \\ z \end{bmatrix} $$

For convenience, we define ths in a `union` so that it can be addressed 
using the `x`, `y`, and `z` members or as an array through the `v` member:

{% highlight c %}
union _vec3 {
  fixed v[3];

  struct {
    fixed x, y, z;
  };
};

typedef union _vec3 vec3;
{% endhighlight %}

Setting an zero'ing out one of these structures is really just a basic data-movement problem:

{% highlight c %}
void vec3_zero(vec3 *v);
#pragma aux vec3_zero =  \
  "xor eax, eax"         \
  "mov ecx, 3"           \
  "rep stosd"            \
  parm [edi]             \
  modify [eax ecx];

void vec3_set(vec3* v, fixed x, fixed y, fixed z);
#pragma aux vec3_set =                       \
  "mov [edi], eax"                           \
  "add edi, 4"                               \
  "mov [edi], ebx"                           \
  "add edi, 4"                               \
  "mov [edi], ecx"                           \
  parm [edi] [eax] [ebx] [ecx];
{% endhighlight %}

Basic arithmetic is achieved using the fixed math primitives defined earlier:

## Negate

$$ -\mathbf{v} = \begin{bmatrix} -x \\ -y \\ -z \end{bmatrix} $$

{% highlight c %}
inline void vec3_neg(vec3 *c) {
  c->x = fixed_mul(c->x, FIXED_NEGONE);
  c->y = fixed_mul(c->y, FIXED_NEGONE);
  c->z = fixed_mul(c->z, FIXED_NEGONE);
}
{% endhighlight %}

## Addition

Given two 3-vectors:

$$ \mathbf{u} = \begin{bmatrix} u_x \\ u_y \\ u_z \end{bmatrix}, \quad \mathbf{v} = \begin{bmatrix} v_x \\ v_y \\ v_z \end{bmatrix} $$

Their sum is:

$$ \mathbf{u} + \mathbf{v} = \begin{bmatrix} u_x + v_x \\ u_y + v_y \\ u_z + v_z \end{bmatrix} $$

{% highlight c %}
inline void vec3_add(vec3 *c, vec3 *a, vec3 *b) {
  c->x = a->x + b->x;
  c->y = a->y + b->y;
  c->z = a->z + b->z;
}
{% endhighlight %}

## Subtraction

Given two 3-vectors:

$$ \mathbf{u} = \begin{bmatrix} u_x \\ u_y \\ u_z \end{bmatrix}, \quad \mathbf{v} = \begin{bmatrix} v_x \\ v_y \\ v_z \end{bmatrix} $$

Their difference is:

$$ \mathbf{u} - \mathbf{v} = \begin{bmatrix} u_x - v_x \\ u_y - v_y \\ u_z - v_z \end{bmatrix} $$

{% highlight c %}
inline void vec3_sub(vec3 *c, vec3 *a, vec3 *b) {
  c->x = a->x - b->x;
  c->y = a->y - b->y;
  c->z = a->z - b->z;
}
{% endhighlight %}

## Multiplly by Scalar

Multiplying it by a scalar $$ c $$ results in:

$$ c \cdot \mathbf{v} = \begin{bmatrix} c \cdot x \\ c \cdot y \\ c \cdot z \end{bmatrix} $$

{% highlight c %}
inline void vec3_mul(vec3 *c, vec3 *a, fixed f) {
  c->x = fixed_mul(a->x, f);
  c->y = fixed_mul(a->y, f);
  c->z = fixed_mul(a->z, f);
}
{% endhighlight %}

## Divide by Scalar

Dividing it by a scalar $$ c $$ (where $$ c \neq 0 $$) results in:

$$ \frac{\mathbf{v}}{c} = \begin{bmatrix} \frac{x}{c} \\ \frac{y}{c} \\ \frac{z}{c} \end{bmatrix} $$

{% highlight c %}
inline void vec3_div(vec3 *c, vec3 *a, fixed f) {
  c->x = fixed_div(a->x, f);
  c->y = fixed_div(a->y, f);
  c->z = fixed_div(a->z, f);
}
{% endhighlight %}

## Length Squared

The length squared (magnitude squared) of the vector is:

$$ \|\mathbf{v}\|^2 = x^2 + y^2 + z^2 $$

{% highlight c %}
inline fixed vec3_len_sqr(vec3 *v) {
  return fixed_mul(v->x, v->x) +
    fixed_mul(v->y, v->y) +
    fixed_mul(v->z, v->z);
}
{% endhighlight %}

## Length

The length (magnitude) of a 3-vector is the square root of the length squared:

$$ \|\mathbf{v}\| = \sqrt{x^2 + y^2 + z^2} $$

{% highlight c %}
inline fixed vec3_len(vec3 *v) {
  return fixed_sqrt(vec3_len_sqr(v));
}
{% endhighlight %}

## Normalise

To normalise a vector (make it unit length), divide each component by its length. Given:

$$ \mathbf{v} = \begin{bmatrix} x \\ y \\ z \end{bmatrix} $$

The normalised vector is:

$$ \hat{\mathbf{v}} = \frac{\mathbf{v}}{\|\mathbf{v}\|} = \begin{bmatrix} \frac{x}{\|\mathbf{v}\|} \\ \frac{y}{\|\mathbf{v}\|} \\ \frac{z}{\|\mathbf{v}\|} \end{bmatrix}, \quad \text{where } \|\mathbf{v}\| \neq 0 $$

{% highlight c %}
inline void vec3_normalize(vec3 *v) {
  fixed inv_len = fixed_div(FIXED_ONE, vec3_len(v));
  v->x = fixed_mul(v->x, inv_len);
  v->y = fixed_mul(v->y, inv_len);
  v->z = fixed_mul(v->z, inv_len);
}
{% endhighlight %}

# Matricies

A 4x4 matrix is how we store all of our vector transformations. We define it like this:

$$
\mathbf{M} = \begin{bmatrix}
m_{11} & m_{12} & m_{13} & m_{14} \\
m_{21} & m_{22} & m_{23} & m_{24} \\
m_{31} & m_{32} & m_{33} & m_{34} \\
m_{41} & m_{42} & m_{43} & m_{44}
\end{bmatrix}
$$

Again, we provide both component based access as well as array based access:

{% highlight c %}
union _mat44 {

  fixed m[16];

  struct {
    fixed e00, e01, e02, e03;
    fixed e10, e11, e12, e13;
    fixed e20, e21, e22, e23;
    fixed e30, e31, e32, e33;
  };
  
};

typedef union _mat44 mat44;
{% endhighlight %}

## Identity

The identity matrix is a special 4x4 matrix where the diagonal elements are 1, and all others are 0:

$$
\mathbf{I} = \begin{bmatrix}
1 & 0 & 0 & 0 \\
0 & 1 & 0 & 0 \\
0 & 0 & 1 & 0 \\
0 & 0 & 0 & 1
\end{bmatrix}
$$

{% highlight c %}
// definition
extern mat44 _mat44_identity;

// instancing
mat44 _mat44_identity = {
  FIXED_ONE , FIXED_ZERO, FIXED_ZERO, FIXED_ZERO,
  FIXED_ZERO, FIXED_ONE , FIXED_ZERO, FIXED_ZERO,
  FIXED_ZERO, FIXED_ZERO, FIXED_ONE , FIXED_ZERO,
  FIXED_ZERO, FIXED_ZERO, FIXED_ZERO, FIXED_ONE
};
{% endhighlight %}

We then simply set a matrix to be an identity matrix, by copying this:

{% highlight c %}
void mat44_identity(mat44 *m);
#pragma aux mat44_identity =   \
  "lea esi, _mat44_identity"   \
  "mov ecx, 0x10"              \
  "rep movsd"                  \
  parm [edi]                   \
  modify [esi ecx];
{% endhighlight %}

## Multiply by Matrix

One of the most important primitives is being able to multiply a matrix by another matrix.

To multiply two 4x4 matrices $$ \mathbf{A} $$ and $$ \mathbf{B} $$:

$$ \mathbf{C} = \mathbf{A} \cdot \mathbf{B}, \quad c_{ij} = \sum_{k=1}^4 a_{ik} \cdot b_{kj} $$

To tidy up the implementation we define a macro `mrm` here.

{% highlight c %}
#define mrm(l,r,i,j) (fixed_mul(l->m[i], r->m[j]))

inline void mat44_mul(mat44 *m, mat44 *l, mat44 *r) {

  mat44_set(m,
            mrm(l,r,0,0) + mrm(l,r,4,1) + mrm(l,r,8,2) + mrm(l,r,12,3),
            mrm(l,r,1,0) + mrm(l,r,5,1) + mrm(l,r,9,2) + mrm(l,r,13,3),
            mrm(l,r,2,0) + mrm(l,r,6,1) + mrm(l,r,10,2) + mrm(l,r,14,3),
            mrm(l,r,3,0) + mrm(l,r,7,1) + mrm(l,r,11,2) + mrm(l,r,15,3),

            mrm(l,r,0,4) + mrm(l,r,4,5) + mrm(l,r,8,6) + mrm(l,r,12,7),
            mrm(l,r,1,4) + mrm(l,r,5,5) + mrm(l,r,9,6) + mrm(l,r,13,7),
            mrm(l,r,2,4) + mrm(l,r,6,5) + mrm(l,r,10,6) + mrm(l,r,14,7),
            mrm(l,r,3,4) + mrm(l,r,7,5) + mrm(l,r,11,6) + mrm(l,r,15,7),

            mrm(l,r,0,8) + mrm(l,r,4,9) + mrm(l,r,8,10) + mrm(l,r,12,11),
            mrm(l,r,1,8) + mrm(l,r,5,9) + mrm(l,r,9,10) + mrm(l,r,13,11),
            mrm(l,r,2,8) + mrm(l,r,6,9) + mrm(l,r,10,10) + mrm(l,r,14,11),
            mrm(l,r,3,8) + mrm(l,r,7,9) + mrm(l,r,11,10) + mrm(l,r,15,11),

            mrm(l,r,0,12) + mrm(l,r,4,13) + mrm(l,r,8,14) + mrm(l,r,12,15),
            mrm(l,r,1,12) + mrm(l,r,5,13) + mrm(l,r,9,14) + mrm(l,r,13,15),
            mrm(l,r,2,12) + mrm(l,r,6,13) + mrm(l,r,10,14) + mrm(l,r,14,15),
            mrm(l,r,3,12) + mrm(l,r,7,13) + mrm(l,r,11,14) + mrm(l,r,15,15));            
  
}
{% endhighlight %}

## Transforming a vector

Given a 4x4 matrix $$ \mathbf{M} $$ and a 4D vector $$ \mathbf{v} = \begin{bmatrix} x \\ y \\ z \\ w \end{bmatrix} $$, 
the result is:

$$
\mathbf{M} \cdot \mathbf{v} = \begin{bmatrix}
m_{11}x + m_{12}y + m_{13}z + m_{14}w \\
m_{21}x + m_{22}y + m_{23}z + m_{24}w \\
m_{31}x + m_{32}y + m_{33}z + m_{34}w \\
m_{41}x + m_{42}y + m_{43}z + m_{44}w
\end{bmatrix}
$$

We are ignoring $$ w $$ by assuming it is 0 in our implementation. We have 3D vectors for simplicity.

{% highlight c %}
inline void mat44_mul_vec(vec3 *v, mat44 *l, vec3 *r) {

  v->x = fixed_mul(l->m[0], r->x) + fixed_mul(l->m[1], r->y) + fixed_mul(l->m[2],  r->z) + l->m[3];
  v->y = fixed_mul(l->m[4], r->x) + fixed_mul(l->m[5], r->y) + fixed_mul(l->m[6],  r->z) + l->m[7];
  v->z = fixed_mul(l->m[8], r->x) + fixed_mul(l->m[9], r->y) + fixed_mul(l->m[10], r->z) + l->m[11];
  
}
{% endhighlight %}

## Translation

The translation matrix is responsible for moving vectors away from the origin by a given amount.

To translate by $$ (t_x, t_y, t_z) $$, the translation matrix is:

$$
\mathbf{T} = \begin{bmatrix}
1 & 0 & 0 & t_x \\
0 & 1 & 0 & t_y \\
0 & 0 & 1 & t_z \\
0 & 0 & 0 & 1
\end{bmatrix}
$$

{% highlight c %}
inline void mat44_translation(mat44 *m, vec3 *v) {
  mat44_set(m,
            FIXED_ONE, 0        , 0        , v->x,
            0        , FIXED_ONE, 0        , v->y,
            0        , 0        , FIXED_ONE, v->z,
            0        , 0        , 0        , FIXED_ONE);
}
{% endhighlight %}

## Scale

The scale matrix will make a point move away from the origin by a given factor.

To scale by $$ (s_x, s_y, s_z) $$, the scaling matrix is:

$$
\mathbf{S} = \begin{bmatrix}
s_x & 0 & 0 & 0 \\
0 & s_y & 0 & 0 \\
0 & 0 & s_z & 0 \\
0 & 0 & 0 & 1
\end{bmatrix}
$$

{% highlight c %}
inline void mat44_scale(mat44 *m, vec3 *v) {
  mat44_set(m,
            v->x, 0   , 0   , 0,
            0   , v->y, 0   , 0,
            0   , 0   , v->z, 0,
            0   , 0   , 0   , FIXED_ONE);
}
{% endhighlight %}

## Rotation Around an Arbitrary Axis

To rotate around an arbitrary axis $$ \mathbf{a} = \begin{bmatrix} a_x \\ a_y \\ a_z \end{bmatrix} $$ by an angle $$ \theta $$, 
the rotation matrix is defined as:

$$ \mathbf{R} = \mathbf{I} \cdot \cos(\theta) + (\mathbf{a} \otimes \mathbf{a}) \cdot (1 - \cos(\theta)) + \mathbf{K} \cdot \sin(\theta) $$

Where:
- $$ \mathbf{I} $$ is the identity matrix.
- $$ \mathbf{a} \otimes \mathbf{a} $$ is the outer product of the axis vector with itself.
- $$ \mathbf{K} $$ is the skew-symmetric matrix derived from $$ \mathbf{a} $$:

$$
\mathbf{K} = \begin{bmatrix}
0 & -a_z & a_y \\
a_z & 0 & -a_x \\
-a_y & a_x & 0
\end{bmatrix}
$$

Expanding this into the full 4x4 matrix:

$$
\mathbf{R} = \begin{bmatrix}
\cos(\theta) + a_x^2(1 - \cos(\theta)) & a_x a_y(1 - \cos(\theta)) - a_z \sin(\theta) & a_x a_z(1 - \cos(\theta)) + a_y \sin(\theta) & 0 \\
a_y a_x(1 - \cos(\theta)) + a_z \sin(\theta) & \cos(\theta) + a_y^2(1 - \cos(\theta)) & a_y a_z(1 - \cos(\theta)) - a_x \sin(\theta) & 0 \\
a_z a_x(1 - \cos(\theta)) - a_y \sin(\theta) & a_z a_y(1 - \cos(\theta)) + a_x \sin(\theta) & \cos(\theta) + a_z^2(1 - \cos(\theta)) & 0 \\
0 & 0 & 0 & 1
\end{bmatrix}
$$

{% highlight c %}
void mat44_axis_rot(mat44 *m, vec3 *axis, fixed angle) {
  fixed ca = fixed_cos(angle),
    sa = fixed_sin(angle);

  fixed nca = FIXED_ONE - ca;

  fixed
    a00 = ca + fixed_mul3(axis->x, axis->x, nca),
    a01 = fixed_mul3(axis->x, axis->y, nca) - fixed_mul(axis->z, sa),
    a02 = fixed_mul3(axis->x, axis->z, nca) + fixed_mul(axis->y, sa),
    a10 = fixed_mul3(axis->y, axis->x, nca) + fixed_mul(axis->z, sa),
    a11 = ca + fixed_mul3(axis->y, axis->y, nca),
    a12 = fixed_mul3(axis->y, axis->z, nca) - fixed_mul(axis->x, sa),
    a20 = fixed_mul3(axis->z, axis->x, nca) - fixed_mul(axis->y, sa),
    a21 = fixed_mul3(axis->z, axis->y, nca) + fixed_mul(axis->x, sa),
    a22 = ca + fixed_mul3(axis->z, axis->z, nca);
                                                    
  mat44_set(m,
            a00, a01, a02, 0,
            a10, a11, a12, 0,
            a20, a21, a22, 0,
            0  , 0  , 0  , FIXED_ONE);
  
}
{% endhighlight %}

## Perspective Projection

For a perspective projection with a field of view $$ fov $$, aspect ratio $$ a $$, near plane $$ n $$, and 
far plane $$ f $$, the matrix is:

$$
\mathbf{P} = \begin{bmatrix}
\frac{1}{a \cdot \tan(fov/2)} & 0 & 0 & 0 \\
0 & \frac{1}{\tan(fov/2)} & 0 & 0 \\
0 & 0 & \frac{f + n}{n - f} & \frac{2 \cdot f \cdot n}{n - f} \\
0 & 0 & -1 & 0
\end{bmatrix}
$$

{% highlight c %}
void mat44_perspective(mat44 *m, fixed fov, fixed aspect, fixed near_z, fixed far_z) {

  fixed _fov = fixed_div(fixed_mul(FIXED_PI, fov), int_to_fixed(180));
  fixed _f = fixed_div(FIXED_ONE, fixed_tan(fixed_mul(_fov, double_to_fixed(0.5))));

  fixed nf = fixed_div((far_z + near_z), (near_z - far_z));
  fixed nfr = fixed_div(fixed_mul(fixed_mul(int_to_fixed(2), far_z), near_z), (near_z - far_z));
  
  mat44_set(m,
            fixed_div(_f, aspect), 0 , 0           , 0  ,
            0                    , _f, 0           , 0  ,
            0                    , 0 , nf          , nfr,
            0                    , 0 , FIXED_NEGONE, 0);
  
}
{% endhighlight %}

# Conclusion

The freak library is my attempt to distill the essence of classic VGA programming into a modern, accessible toolkit. By 
combining essential building blocks like graphics handling, input, and math operations, it provides everything you need 
to recreate the magic of the demoscene or explore retro-style programming.

I hope this article inspires you to dive into the world of low-level programming and experiment with the techniques that 
defined a generation of creativity. Whether you’re building your first polygon renderer or optimizing an effect with 
fixed-point math, freak is here to make the journey both rewarding and fun.

Let me know what you think or share what you build—there’s nothing quite like seeing new creations come to life with 
tools like these!