---
layout: post
title: Windows programs with masm32
date: 2015-09-14
comments: false
categories: [ "windows", "win32", "masm32", "assembly" ]
---

In a [previous post]({% post_url 2015-09-12-windows-development-masm %}) I wrote about the basics of getting a program written and compiled using [masm32](http://www.masm32.com/) for the windows platform. In today's post, I'm going to walkthrough the basic anatomy of a windows program that play the key roles in your application.

This information is language agnostic. It's standard windows programming knowledge that I'll present in the form of assembly programming with **masm32**. By the end of this blog post, you'll have a good starting point (in boilerplate code) for any program that you want to write with **masm32**.

### The basic pieces

In any windows program (using the Win32 API) there are some basic pieces that you'll see most of the time.

* Main entry point (WinMain)
* Window creation
* Window handler procedure (WNDPROC)
* Message pump

These basic building blocks that you'll see in just about all windows applications.

### WinMain

This is your application's main entry point. It's analogous to the `main` function that you use when writing applicatioons in C/C++. From [Microsoft's site](https://msdn.microsoft.com/en-us/library/windows/desktop/ms633559%28v=vs.85%29.aspx):

> The user-provided entry point for a graphical Windows-based application.

Its function signature takes the following form:

{% highlight c %}
int CALLBACK WinMain(
  _In_ HINSTANCE hInstance,
  _In_ HINSTANCE hPrevInstance,
  _In_ LPSTR     lpCmdLine,
  _In_ int       nCmdShow
);
{% endhighlight %}

This definition needs to be translated from C into assembly language. The `HINSTANCE`, `LPSTR` and `int` data types all reduce down to `DWORD` easily, so:

{% highlight asm %}
WinMain proc 	hInst 		:dword, 
				hPrevInst 	:dword,
				szCmdLine 	:dword,
				nShowCmd 	:dword
{% endhighlight %}

Being at the assembly layer, we're exposed a little earlier in the process than what `WinMain` affords us, so we have extra work to do in terms of getting our application running. We as the implementers of the program need to invoke `WinMain` directly as this is not done for us.

{% highlight asm %}
invoke 	GetModuleHandle, NULL
mov		hInstance, eax

invoke	GetCommandLine
mov		lpszCmdLine, eax

invoke 	WinMain, hInstance, NULL, lpszCmdLine, SW_SHOWDEFAULT
invoke	ExitProcess, eax
{% endhighlight %}

`GetModuleHandle` allows us to fill the `hInstance` parameter and `GetCommandLine` gives us `lpszCmdLine`. `hPrevInstance`, according [to the documentation](https://msdn.microsoft.com/en-us/library/windows/desktop/ms633559%28v=vs.85%29.aspx) is always `NULL` and `nShowCmd` is an `SW_` series value that controls how our main application window is shown.

### Window creation

The creation of a window is broken down into a few smaller steps:

* Register a window class
* Create the window
* Show the window

Registering a window class is just filling out the `WNDCLASSEX` structure and calling `RegisterClassEx`. Registering a windows class establishes base or common attributes about a window that you can re-use in subsequent calls to `CreateWindow`.

{% highlight asm %}
local 	wc 		:WNDCLASSEX
local 	msg 	:MSG
local 	hWnd 	:HWND

szText	szClassName, "BasicWindow"
szText	szWindowTitle, "First Window"

mov		wc.cbSize, sizeof WNDCLASSEX
mov		wc.style, CS_HREDRAW or CS_VREDRAW or CS_BYTEALIGNWINDOW
mov 	wc.lpfnWndProc, WndProc
mov 	wc.cbClsExtra, NULL
mov		wc.cbWndExtra, NULL

push	hInst
pop 	wc.hInstance

mov		wc.hbrBackground, COLOR_BTNFACE + 1
mov		wc.lpszMenuName, NULL
mov 	wc.lpszClassName, offset szClassName

invoke	LoadIcon, hInst, IDI_APPLICATION
mov		wc.hIcon, eax
mov		wc.hIconSm, eax

invoke	LoadCursor, hInst, IDC_ARROW
mov		wc.hCursor, eax

invoke	RegisterClassEx, addr wc
{% endhighlight %}

The macro `szText` is setup to allow you to ad-hoc define string variables where ever you need to. The rest of this is mainly filling out the structure and finally registering it. Setting `lpfnWndProc` has special significance to us here as we'll go on to describe `WNDPROC` and its role in the application.

### WindowProc callback function

When an application receives messages from the operating system, it handles this information through the window procedure. The window procedure interface is defined as:

{% highlight c %}
LRESULT CALLBACK WindowProc(
  _In_ HWND   hwnd,
  _In_ UINT   uMsg,
  _In_ WPARAM wParam,
  _In_ LPARAM lParam
);
{% endhighlight %}

Again, translating this into assembly:

{% highlight asm %}
WndProc proc 	hWin 	:dword,
				uMsg 	:dword,
				wParam 	:dword,
				lParam 	:dword
{% endhighlight %}

### The message pump

Finally, the thing that's keeping our application alive is the message pump. It's a loop of `GetMessage`, `TranslateMessage` and `DispatchMessage`.

{% highlight asm %}
MessagePump:

	invoke 	GetMessage, addr msg, NULL, 0, 0

	cmp 	eax, 0
	je 		MessagePumpEnd

	invoke	TranslateMessage, addr msg
	invoke	DispatchMessage, addr msg

	jmp 	MessagePump

MessagePumpEnd:

{% endhighlight %}

### All together now

The following gist is all of the pieces assembled in a basic application that's runnable (once assembled and linked).

{% gist da9416c4ee9588331da4 %}

