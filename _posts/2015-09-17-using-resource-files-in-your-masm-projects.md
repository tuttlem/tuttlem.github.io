---
layout: post
title: Using resource files in your masm projects
date: 2015-09-17
comments: false
categories: [ "windows", "win32", "masm32", "assembly", "resource" ]
---

In a [previous post]({% post_url 2015-09-14-windows-programs-with-masm32 %}) we built a boilerplate program that we can start to work with. For today's post, we're going to go through the development of a [resource file](https://msdn.microsoft.com/en-us/library/cc194804.aspx) that you can incorporate into your own projects.

### What are resource files?

You can use resource files to organise your external resources (images, audio, text, etc.) into a compilable (and ultimately linkable) resource. The items that you add to your resource file are then statically added to your resulting executable, ready for you to reference.

From the [Microsoft](https://msdn.microsoft.com/en-us/library/cc194804.aspx) site:

> You should place every element of the user interface that needs to be localized in a Windows resource file, including pictures, strings, messages, menus, dialog boxes, and version information. The table below lists the individual resource elements defined by Windows.

|Resource Type | Element | File Format | Comment/ Description |
|--------------|---------|-------------|----------------------|
|RT_CURSOR 	     | Cursor                  | .CUR 	     | #include in .RC file |
|RT_BITMAP 	     | Bitmap or toolbar       | .BMP        | #include in .RC file |
|RT_ICON 	     | Icon                    | .ICO        | #include in .RC file |
|RT_MENU 	     | Menu or pop up menu     | .RC 	     | #include in .RC file |
|RT_DIALOG 	     | Dialog                  | .DLG or .RC | #include .DLG file in .RC file |
|RT_STRING 	     | String                  | .RC         | |
|RT_FONTDIR      | Font                    | .FNT        | |
|RT_FONT         | Font                    | .FNT        | |
|RT_ACCELERATORS | Accelerator             | .RC         | | 	 
|RT_RCDATA 	     | User-defined resource   | .RC         | Can use for constants or application specific structures |  	  	  	 
|RT_MESSAGETABLE | Messages                | .MC 	     | #include compiled message table in .RC file | 	  	  	 
|RT_GROUP_CURSOR | Cursor                  | N/A         | Generated internally by resource compiler to provide Windows with information about cursor's resolution and type | 	  	  	 
|RT_GROUP_ICON   | Icon                    | N/A         |Generated internally by resource compiler to provide Windows with information about icon's resolution and type |	  	  	 
|RT_VERSION      | Version information     | .RC         | | 	  	  	  	 
|RT_DLGINCLUDE   | Header file that contains menu and dialog box #define statements | .RC | Used by resource editing tools; Visual C++ uses its own mechanism tools; |

### What does a resource file look like?

The `.RC` file itself is just text. There are IDs that are defined throughout that you can create symbolic constants for in your code, just so you're not doing so much "magic number" work. Here's a simple menu:

{% highlight text %}
600 MENUEX MOVEABLE IMPURE LOADONCALL DISCARDABLE
BEGIN
    POPUP "&File", , , 0
    BEGIN
        MENUITEM "&Exit", 1000
    END
    POPUP "&Help", , , 0
    BEGIN
        MENUITEM "&About", 1900
    END
END
{% endhighlight %}

`600` in this example is the ID of the menu. Referencing this menu in your code is as simple as passing `600` to the menu name parameter of a `LoadMenu` call:

{% highlight asm %}
invoke LoadMenu, hInstance, 600
invoke SetMenu, hWnd, eax
{% endhighlight %}

### How do I compile one?

`rc.exe` is the resource compiler that comes along with masm32. You give it your resource script and it'll give you back a compiled `.RES` file. 

{% highlight text %}
rc /v rsrc.rc
{% endhighlight %}

You then take the `.RES` file that was output; in this case it was `rsrc.res` and feed it into another utility `cvtres.exe`. 

{% highlight text %}
cvtres /machine:ix86 rsrc.res
{% endhighlight %}

This utility converts your compiled `.RES` file into object code. It produces you an `.OBJ` file that you can then link into your exe.

