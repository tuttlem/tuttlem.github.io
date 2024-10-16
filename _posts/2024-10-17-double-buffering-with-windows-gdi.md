---
layout: post
title: Double Buffering with the Windows GDI
date: 2024-10-17
comments: false
categories: [ "c", "win32", "windows", "graphics" ]
---

# Introduction

Flickering can be a common problem when drawing graphics in a Windows application. One effective way to prevent this is by using a 
technique called **double buffering**. In this article, we'll walk through creating a simple Win32 application that uses double 
buffering to provide smooth and flicker-free rendering.

## Getting Started

First, let's create a basic Win32 window and set up the message loop.

{% highlight c %}
#include <Windows.h>

LRESULT CALLBACK WindowProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

int running = 1;

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nShowCmd) {

    WNDCLASSEX wc = {
        sizeof(WNDCLASSEX), CS_HREDRAW | CS_VREDRAW | CS_OWNDC,
        WindowProc, NULL, NULL,
        hInstance,
        LoadIcon(hInstance, IDI_APPLICATION),
        LoadCursor(hInstance, IDC_ARROW),
        NULL, NULL, L"DoubleBufferClass", NULL
    };

    RegisterClassEx(&wc);

    HWND hWnd = CreateWindowEx(WS_EX_APPWINDOW, L"DoubleBufferClass", L"Double Buffer",
        WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
        NULL, NULL, hInstance, NULL);

    ShowWindow(hWnd, SW_SHOWDEFAULT);
    UpdateWindow(hWnd);

    MSG msg;

    while (running) {

        if (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE)) {
            TranslateMessage(&msg);
            DispatchMessage(&msg);
        }

    }

    return (int)msg.lParam;
}
{% endhighlight %}

In this code, we define a `WinMain` function, which is the entry point for a Windows desktop application. We define a window class 
and register it with the system, then create the window using `CreateWindowEx`.

The message loop waits for input messages, like key presses or mouse movements, and dispatches them to the appropriate window 
procedure. We check for messages using `PeekMessage` so the loop remains responsive and can handle user input without blocking.

## Creating the Buffer

Now, let's modify the program to set up the back buffer for double buffering. We'll do this by implementing the window procedure 
(`WindowProc`) and handling key messages like `WM_CREATE`, `WM_SIZE`, and `WM_DESTROY`.

{% highlight c %}
HDC memDC = NULL, winDC = NULL;
HBITMAP memBitMap = NULL;
HBITMAP memOldMap = NULL;

LRESULT CALLBACK WindowProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam) {

    switch (uMsg) {
        case WM_CLOSE:
            running = 0;
            break;

        case WM_ERASEBKGND:
            return 1;

        case WM_DESTROY: 
            DestroyBackBuffer(hWnd);
            PostQuitMessage(0);
            return 0;

        case WM_CREATE:
            RecreateBackBuffer(hWnd);
            break;

        case WM_SIZE:
            RecreateBackBuffer(hWnd);
            break;

        case WM_PAINT:

            PAINTSTRUCT ps;
            RECT r;

            GetClientRect(hWnd, &r);
            FillRect(memDC, &r, CreateSolidBrush(RGB(0, 255, 0)));

            HDC hdc = BeginPaint(hWnd, &ps);
            BitBlt(hdc, 0, 0, r.right - r.left, r.bottom - r.top, memDC, 0, 0, SRCCOPY);
            EndPaint(hWnd, &ps);

            break;
    }

    return DefWindowProc(hWnd, uMsg, wParam, lParam);
}
{% endhighlight %}

The `WindowProc` function handles window events such as creating the back buffer (`WM_CREATE`), resizing it (`WM_SIZE`), 
and destroying it (`WM_DESTROY`). We also override `WM_ERASEBKGND` to prevent flickering by blocking the default 
background erase.

Next, in the `WM_PAINT` handler, we use `BitBlt` to copy the contents of the memory device context (`memDC`) to the 
window's device context, effectively flipping the buffer and rendering the scene.

## Drawing and Flipping

Now, we'll define the `RecreateBackBuffer` and `DestroyBackBuffer` functions that manage the lifecycle of the buffer.

{% highlight c %}
void DestroyBackBuffer(HWND hWnd) {

    if (memDC != NULL) {
        SelectObject(memDC, memOldMap);
        DeleteObject(memBitMap);
        DeleteDC(memDC);

        memDC = NULL;
        memOldMap = memBitMap = NULL;
    }

    if (winDC != NULL) {
        ReleaseDC(hWnd, winDC);
        winDC = NULL;
    }

}

void RecreateBackBuffer(HWND hWnd) {

    DestroyBackBuffer(hWnd);

    RECT client;

    GetClientRect(hWnd, &client);
    winDC = GetDC(hWnd);
    
    memDC = CreateCompatibleDC(winDC);
    memBitMap = CreateCompatibleBitmap(winDC, client.right - client.left, client.bottom - client.top);
    memOldMap = (HBITMAP)SelectObject(memDC, memBitMap);

}
{% endhighlight %}

The `RecreateBackBuffer` function creates a new off-screen bitmap whenever the window is resized or created. The bitmap 
is selected into the memory device context (`memDC`), which is used for all the off-screen drawing.

The `DestroyBackBuffer` function cleans up the memory device context, releasing the resources used by the back 
buffer when the window is destroyed or the buffer is resized.

## Animation Loop

To animate, we need to redraw the back buffer continually. Instead of relying solely on `WM_PAINT`, we can create 
an animation loop that forces the screen to refresh at regular intervals.

A simple way to do this is to use `SetTimer` or a manual loop that invalidates the window periodically. Here's 
how you could structure the loop:

{% highlight c %}
while (running) {
    if (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE)) {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    } else {
        // Animation logic here
        InvalidateRect(hWnd, NULL, FALSE);
        Sleep(16); // Roughly 60 FPS
    }
}
{% endhighlight %}

This change redraws the window about 60 times per second, perfect for smooth animations.

## Conclusion

Double buffering is a powerful technique that enhances the visual quality of graphical applications by eliminating flickering 
during rendering. By using an off-screen buffer to draw content before displaying it on the screen, we can ensure smooth 
transitions and animations. In this article, we walked through setting up a basic Win32 window, creating and managing the 
back buffer, and implementing a simple animation loop using double buffering.

With this foundation, you can now explore more complex drawing routines or incorporate this technique into larger projects 
for better performance and visual appeal.
