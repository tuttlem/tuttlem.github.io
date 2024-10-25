---
layout: post
title: Double Buffering with Windows GDI in MASM
date: 2024-10-20
comments: false
categories: [ "" ]
---

# Introduction

In a [previous post]({% post_url 2024-10-17-double-buffering-with-windows-gdi %}) , I detailed a double-buffering 
implementation written in C. The idea behind double buffering is to draw graphics off-screen, then quickly swap 
(or "flip") this off-screen buffer with the visible screen. This technique reduces flickering and provides smoother 
rendering. While the C implementation was relatively straightforward using GDI functions, I decided to challenge myself 
by recreating it in assembly language using MASM32. 

There are some slight differences that I'll go through.

The full code is [available as a gist](https://gist.github.com/tuttlem/17796f7590d7627d4d237f8f4dac8ce9) to follow along with.

# Macros

First up, this module defines some macros that are just helpful blocks of reusable code.

* `szText` defines a string inline
* `m2m` performs value assignment from a memory location, to another
* `return` is a simple analog for the `return` keyword in c
* `rgb` encodes 8 bit RGB components into the `eax` register

{% highlight nasm %}
; Defines strings in an ad-hoc fashion
szText MACRO Name, Text:VARARG
    LOCAL lbl
    jmp lbl
    Name db Text,0
lbl:
ENDM

; Assigns a value from a memory location into another memory location
m2m MACRO M1, M2
    push M2
    pop M1
ENDM

; Syntax sugar for returning from a PROC
return MACRO arg
    mov eax, arg
    ret
ENDM

rgb MACRO r, g, b
    xor eax, eax
    mov ah, b
    mov al, g
    rol eax, 8
    mov al, r
ENDM
{% endhighlight %}

# Setup

The setup is very much like its C counterpart with a registration of a class first, and then the creation of the window.

{% highlight nasm %}
szText szClassName, "DoubleBufferClass"

mov wc.cbSize, sizeof WNDCLASSEX
mov wc.style, CS_HREDRAW or CS_VREDRAW or CS_BYTEALIGNWINDOW
mov wc.lpfnWndProc, offset WndProc
mov wc.cbClsExtra, NULL
mov wc.cbWndExtra, NULL
m2m wc.hInstance, hInst
mov wc.hbrBackground, NULL
mov wc.lpszMenuName, NULL
mov wc.lpszClassName, offset szClassName

invoke LoadIcon, NULL, IDI_APPLICATION
mov wc.hIcon, eax

invoke LoadCursor, NULL, IDC_ARROW
mov wc.hCursor, eax

mov wc.hIconSm, 0

invoke RegisterClassEx, ADDR wc
{% endhighlight %}

The `szClassName` gives us a reference to the name of the class to use.

{% highlight nasm %}
invoke CreateWindowEx, WS_EX_APPWINDOW, ADDR szClassName, ADDR szDisplayName, WS_OVERLAPPEDWINDOW,
                       CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, NULL, NULL, hInst, NULL
{% endhighlight %}

# Message pump

We continue to render out to the window in a loop:

{% highlight nasm %}
StillRunning:
    invoke PeekMessage, ADDR msg, NULL, 0, 0, PM_REMOVE

    cmp eax, 0
    je RenderFrame

    invoke DispatchMessage, ADDR msg

RenderFrame:

    invoke DrawRandomShape
    invoke InvalidateRect, hWnd, NULL, 0

    cmp bRunning, 1
    je StillRunning
{% endhighlight %}

Using `InvalidateRect` tells the window that there is an update to draw. This is then propagated through the `WM_PAINT` 
message.

# Window proc

Each of the window messages is handled in a switch/case like arrangement with a series of `cmp` and `je` instructions.

In higher level MASM this can be handled using the `.IF` syntax.

{% highlight nasm %}
    cmp uMsg, WM_CREATE
    je CreateMessage

    cmp uMsg, WM_DESTROY
    je DestroyMessage

    cmp uMsg, WM_ERASEBKGND
    je EraseBackgroundMessage

    cmp uMsg, WM_CLOSE
    je CloseMessage

    cmp uMsg, WM_SIZE
    je SizeMessage

    cmp uMsg, WM_PAINT
    je PaintMessage

    jmp DefaultWindowHandler
{% endhighlight %}

We use the `WM_CREATE`, `WM_SIZE`, and `WM_DESTROY` messages to control when we create and destroy our back buffer.

{% highlight nasm %}
CreateMessage:
SizeMessage:
    invoke RecreateBackBuffer, hWin
    jmp DefaultWindowHandler

DestroyMessage:

    invoke DestroyBackBuffer, hWin
    invoke PostQuitMessage, 0

    xor eax, eax
    ret
{% endhighlight %}

`WM_PAINT` only needs to worry about drawing the backbuffer to the window.

{% highlight nasm %}
PaintMessage:
    invoke FlipBackBuffer, hWin
    mov eax, 1
    ret
{% endhighlight %}

# Handling the buffer

The routine that handles the back buffer construction is called `RecreateBackBuffer`. It's a routine that will clean 
up before it trys to create the back buffer saving the program from memory leaks:

{% highlight nasm %}
    invoke DestroyBackBuffer, hWin
    
    invoke GetClientRect, hWin, ADDR clientRect
    invoke GetDC, hWin
    mov winDC, eax

    invoke CreateCompatibleDC, winDC
    mov memDC, eax

    mov eax, clientRect.right
    sub eax, clientRect.left
    mov ebx, clientRect.bottom
    sub ebx, clientRect.top

    invoke CreateCompatibleBitmap, winDC, eax, ebx
    mov memBitmap, eax
    invoke SelectObject, memDC, memBitmap
    mov memOldBitmap, eax
    xor eax, eax
{% endhighlight %}

`DestroyBackBuffer` being the first thing called here; it's just a basic clean up:

{% highlight nasm %}
    cmp memDC, NULL
    je MemoryDCDeallocated

    invoke SelectObject, memDC, memOldBitmap
    invoke DeleteObject, memBitmap
    invoke DeleteDC, memDC

    mov memDC, NULL
    mov memOldBitmap, NULL
    mov memBitmap, NULL

MemoryDCDeallocated:

    cmp winDC, NULL
    je WindowDCReleased

    invoke ReleaseDC, hWin, winDC
    mov winDC, NULL

WindowDCReleased:

    xor eax, eax
    ret
{% endhighlight %}

# Flipping

When we want to draw that back buffer to the window, we just use `BitBlt` from the GDI:

{% highlight nasm %}
    LOCAL hDC:HDC
    LOCAL ps:PAINTSTRUCT

    invoke BeginPaint, hWin, ADDR ps
    mov hDC, eax

    mov eax, clientRect.right
    sub eax, clientRect.left
    mov ebx, clientRect.bottom
    sub ebx, clientRect.top

    invoke BitBlt, hDC, 0, 0, eax, ebx, memDC, 0, 0, SRCCOPY
    invoke EndPaint, hWin, ADDR ps

    xor eax, eax
    ret
{% endhighlight %}

# Conclusion

This is just a different take on the same application written in C. Some of the control structures in assembly language 
can seem a little hard to follow, but there is something elegant about their simplicity. 