---
layout: post
title: Mode X.. plus some!
date: 2012-11-29
comments: false
categories: [ "ModeX", "Mode 13", "assembly", "programming" ]
---

So, not much of tutorial here - just a neat layout of setup code for a few mode X modes I've come across now and then.

First of all, just some formalities. Setting Mode X tweaks to the VGA is a massive port-out exercise. Creating the following macro cut down on my code heaps.

{% highlight nasm %}
outp MACRO port, value
	mov dx, port
	mov al, value
	out dx, al
ENDM
{% endhighlight %}

So you can see, it just gives sending a byte out to a port a bit of syntactical sugar in assembly language. Easy.

The only other piece of formality is, you must have set the video display into MCGA (mode 13) first:

{% highlight nasm %}
set_mcga:
    mov ax, 0013h
    int 10h
	
    ret
{% endhighlight %}

On to the modes.

{% highlight nasm %}
tweak_160x120:
    ; --------------------------
    ; 160x120
    ;
    ; pages = 13
    ; line size = 40
    ; page size = 19200
    ; --------------------------
	
    outp 03d4h, 011h
	
    mov dx, 03d5h
    in  al, dx
    and al, 07fh
    mov bl, al
	
    outp 03d4h, 011h
    outp 03d5h, bl
    outp 03c2h, 0e3h
    outp 03d4h, 000h
    outp 03d5h, 032h
    outp 03d4h, 001h
    outp 03d5h, 027h
    outp 03d4h, 002h
    outp 03d5h, 028h
    outp 03d4h, 003h
    outp 03d5h, 020h
    outp 03d4h, 004h
    outp 03d5h, 02bh                  
    outp 03d4h, 005h
    outp 03d5h, 070h                  
    outp 03d4h, 006h
    outp 03d5h, 00dh                  
    outp 03d4h, 007h
    outp 03d5h, 03eh                  
    outp 03d4h, 008h
    outp 03d5h, 000h                 
    outp 03d4h, 009h
    outp 03d5h, 043h                 
    outp 03d4h, 010h
    outp 03d5h, 0eah                  
    outp 03d4h, 011h
    outp 03d5h, 0ach                 
    outp 03d4h, 012h
    outp 03d5h, 0dfh                  
    outp 03d4h, 013h
    outp 03d5h, 014h                  
    outp 03d4h, 014h
    outp 03d5h, 000h                  
    outp 03d4h, 015h
    outp 03d5h, 0e7h                 
    outp 03d4h, 016h
    outp 03d5h, 006h                  
    outp 03d4h, 017h
    outp 03d5h, 0e3h                  
    outp 03c4h, 001h
    outp 03c5h, 001h                  
    outp 03c4h, 003h
    outp 03c5h, 000h                  
    outp 03c4h, 004h
    outp 03c5h, 006h                 
    outp 03ceh, 005h
    outp 03cfh, 040h                
    outp 03ceh, 006h
    outp 03cfh, 005h

    mov dx, 03dah
    in  al, dx

    outp 03c0h, 010h or 020h
    outp 03c0h, 041h

    mov dx, 03dah
    in  al, dx

    outp 03c0h, 011h or 020h
    outp 03c0h, 0

    mov dx, 03dah
    in  al, dx

    outp 03c0h, 012h or 020h
    outp 03c0h, 0fh

    mov dx, 03dah
    in  al, dx

    outp 03c0h, 013h or 020h
    outp 03c0h, 0

    mov dx, 03dah
    in  al, dx

    outp 03c0h, 014h or 020h
    outp 03c0h, 0

    outp 03d4h, 011h
    
    mov dx, 03d5h
    in  al, dx
    and al, 80h
    mov bl, al

    outp 03d4h, 011h
    outp 03d5h, bl

    ret

tweak_296x220:
    ; --------------------------
    ; 296x220
    ;
    ; pages = 4
    ; line size = 74
    ; page size = 65120
    ; --------------------------

    outp 03d4h, 011h

    mov dx, 03d5h
    in  al, dx
    and al, 7fh
    mov bl, al
	
    outp 03d4h, 011h
    outp 03d5h, bl

    outp 03c2h, 0e3h
    outp 03d4h, 000h
    outp 03d5h, 05fh
    outp 03d4h, 001h
    outp 03d5h, 049h
    outp 03d4h, 002h
    outp 03d5h, 050h
    outp 03d4h, 003h
    outp 03d5h, 082h
    outp 03d4h, 004h
    outp 03d5h, 053h
    outp 03d4h, 005h
    outp 03d5h, 080h
    outp 03d4h, 006h
    outp 03d5h, 00dh
    outp 03d4h, 007h
    outp 03d5h, 03eh
    outp 03d4h, 008h
    outp 03d5h, 000h
    outp 03d4h, 009h
    outp 03d5h, 041h
    outp 03d4h, 010h
    outp 03d5h, 0d7h
    outp 03d4h, 011h
    outp 03d5h, 0ach
    outp 03d4h, 012h
    outp 03d5h, 0b7h
    outp 03d4h, 013h
    outp 03d5h, 025h
    outp 03d4h, 014h
    outp 03d5h, 000h
    outp 03d4h, 015h
    outp 03d5h, 0e7h
    outp 03d4h, 016h
    outp 03d5h, 006h
    outp 03d4h, 017h
    outp 03d5h, 0e3h
    outp 03c4h, 001h
    outp 03c5h, 001h
    outp 03c4h, 004h
    outp 03c5h, 006h
    outp 03ceh, 005h
    outp 03cfh, 040h
    outp 03ceh, 006h
    outp 03cfh, 005h

    mov dx, 03dah
    in  al, dx

    outp 03c0h, 010h or 020h
    outp 03c0h, 041h

    mov dx, 03dah
    in  al, dx
	
    outp 03c0h, 013h or 020h
    outp 03c0h, 0

    outp 03d4h, 011h

    mov dx, 03d5h
    in  al, dx
    and al, 80h
    mov bl, al
	
    outp 03d4h, 011h
    outp 03d5h, bl

    ret

tweak_320x200:
    ; --------------------------
    ; 320x200
    ;
    ; pages = 4
    ; line size = 80
    ; page size = 64000
    ; --------------------------

    outp 03c4h, 04h
    outp 03c5h, 06h

    outp 03d4h, 017h
    outp 03d5h, 0E3h

    outp 03d4h, 014h
    outp 03d5h, 0
	
    ret

tweak_320x240:
    ; --------------------------
    ; 320x240
    ;
    ; pages = 3
    ; line size = 80
    ; page size = 76800
    ; --------------------------

    outp 03d4h, 11h

    mov dx, 03d5h
    in  al, dx
    and al, 7fh
    mov bl, al

    outp 03d4h, 11h
    outp 03d5h, bl

    outp 03c2h, 0e3h
    outp 03d4h, 000h
    outp 03d5h, 05fh
    outp 03d4h, 001h
    outp 03d5h, 04fh
    outp 03d4h, 002h
    outp 03d5h, 050h
    outp 03d4h, 003h
    outp 03d5h, 082h
    outp 03d4h, 004h
    outp 03d5h, 054h
    outp 03d4h, 005h
    outp 03d5h, 080h
    outp 03d4h, 006h
    outp 03d5h, 00dh
    outp 03d4h, 007h
    outp 03d5h, 03eh
    outp 03d4h, 008h
    outp 03d5h, 000h
    outp 03d4h, 009h
    outp 03d5h, 041h
    outp 03d4h, 010h
    outp 03d5h, 0eah
    outp 03d4h, 011h
    outp 03d5h, 0ach
    outp 03d4h, 012h
    outp 03d5h, 0dfh
    outp 03d4h, 013h
    outp 03d5h, 028h
    outp 03d4h, 014h
    outp 03d5h, 000h
    outp 03d4h, 015h
    outp 03d5h, 0e7h
    outp 03d4h, 016h
    outp 03d5h, 006h
    outp 03d4h, 017h
    outp 03d5h, 0e3h
    outp 03c4h, 001h
    outp 03c5h, 001h
    outp 03c4h, 004h
    outp 03c5h, 006h
    outp 03ceh, 005h
    outp 03cfh, 040h
    outp 03ceh, 006h
    outp 03cfh, 005h

    mov dx, 03dah
    in  al, dx
	
    outp 03c0h, 010h or 020h
    outp 03c0h, 041h

    mov dx, 03dah
    in  al, dx
	
    outp 03c0h, 013h or 020h
    outp 03c0h, 0

    outp 03d4h, 011h

    mov dx, 03d5h
    in  al, dx
    or  al, 80h
    mov bl, al

    outp 03d4h, 11h
    outp 03d5h, bl

    ret

tweak_320x400:
    ; --------------------------
    ; 320x400
    ;
    ; pages = 2
    ; line size = 80
    ; page size = 128000
    ; --------------------------

    outp 03c4h, 004h
    outp 03c5h, 006h
    outp 03d4h, 017h
    outp 03d5h, 0E3h
    outp 03d4h, 014h
    outp 03d5h, 000h
    outp 03d4h, 009h
    outp 03d5h, 040h

    ret

tweak_360x360:
    ; --------------------------
    ; 360x360
    ;
    ; pages = 2
    ; line size = 90
    ; page size = 129600
    ; --------------------------

    outp 03d4h, 011h

    mov dx, 03d5h
    in  al, dx
    and al, 7fh
    mov bl, al

    outp 03d4h, 011h
    outp 03d5h, bl

    outp 03c2h, 067h
    outp 03d4h, 000h
    outp 03d5h, 06bh
    outp 03d4h, 001h
    outp 03d5h, 059h
    outp 03d4h, 002h
    outp 03d5h, 05ah
    outp 03d4h, 003h
    outp 03d5h, 08eh
    outp 03d4h, 004h
    outp 03d5h, 05eh
    outp 03d4h, 005h
    outp 03d5h, 08ah
    outp 03d4h, 006h
    outp 03d5h, 0bfh
    outp 03d4h, 007h
    outp 03d5h, 01fh
    outp 03d4h, 008h
    outp 03d5h, 000h
    outp 03d4h, 009h
    outp 03d5h, 040h
    outp 03d4h, 010h
    outp 03d5h, 088h
    outp 03d4h, 011h
    outp 03d5h, 085h
    outp 03d4h, 012h
    outp 03d5h, 067h
    outp 03d4h, 013h
    outp 03d5h, 02dh
    outp 03d4h, 014h
    outp 03d5h, 000h
    outp 03d4h, 015h
    outp 03d5h, 06dh
    outp 03d4h, 016h
    outp 03d5h, 0bah
    outp 03d4h, 017h
    outp 03d5h, 0e3h
    outp 03c4h, 001h
    outp 03c5h, 001h
    outp 03c4h, 004h
    outp 03c5h, 006h
    outp 03ceh, 005h
    outp 03cfh, 040h
    outp 03ceh, 006h
    outp 03cfh, 005h

    mov dx, 03dah
    in  al, dx
	
    outp 03c0h, 010h or 020h
    outp 03c0h, 041h

    mov dx, 03dah
    in  al, dx

    outp 03c0h, 013h or 020h
    outp 03c0h, 0

    outp 03d4h, 011h

    mov dx, 03d5h
    in  al, dx
    or  al, 80h
    mov bl, al
	
    outp 03d4h, 011h
    outp 03d5h, bl	

    ret

tweak_400x300:
    ; --------------------------
    ; 400x300
    ;
    ; pages = 2
    ; line size = 100
    ; page size = 120000
    ; --------------------------
	
    outp 03d4h, 011h

    mov dx, 03d5h
    in  al, dx
    and al, 7fh
    mov bl, al

    outp 03d4h, 011h
    outp 03d5h, bl	

    outp 03c2h, 0e7h                    
    outp 03d4h, 000h 
    outp 03d5h, 071h                    
    outp 03d4h, 001h 
    outp 03d5h, 063h                    
    outp 03d4h, 002h 
    outp 03d5h, 064h                    
    outp 03d4h, 003h 
    outp 03d5h, 092h                    
    outp 03d4h, 004h 
    outp 03d5h, 067h                   
    outp 03d4h, 005h 
    outp 03d5h, 082h                    
    outp 03d4h, 006h 
    outp 03d5h, 046h                    
    outp 03d4h, 007h 
    outp 03d5h, 01fh                    
    outp 03d4h, 008h 
    outp 03d5h, 000h                    
    outp 03d4h, 009h 
    outp 03d5h, 040h                    
    outp 03d4h, 010h 
    outp 03d5h, 031h                    
    outp 03d4h, 011h 
    outp 03d5h, 080h                     
    outp 03d4h, 012h 
    outp 03d5h, 02bh                    
    outp 03d4h, 013h 
    outp 03d5h, 032h                    
    outp 03d4h, 014h 
    outp 03d5h, 000h                    
    outp 03d4h, 015h 
    outp 03d5h, 02fh                    
    outp 03d4h, 016h 
    outp 03d5h, 044h                    
    outp 03d4h, 017h 
    outp 03d5h, 0e3h                    
    outp 03c4h, 001h 
    outp 03c5h, 001h                    
    outp 03c4h, 002h 
    outp 03c5h, 00fh                    
    outp 03c4h, 004h 
    outp 03c5h, 006h                    
    outp 03ceh, 005h 
    outp 03cfh, 040h                    
    outp 03ceh, 006h 
    outp 03cfh, 005h 
                       
    mov dx, 03dah
    in  al, dx
	
    outp 03c0h, 010h or 020h
    outp 03c0h, 041h
    
    mov dx, 03dah
    in  al, dx

    outp 03c0h, 013h or 020h
    outp 03c0h, 0
    
    outp 03d4h, 011h
	
    mov dx, 03d5h
    in  al, dx
    or  al, 80h
    mov bl, al	
	
    outp 03d4h, 011h
    outp 03d5h, bl

    ret
{% endhighlight %}

I will have an update to this post. There are some nuances that I'd much prefer explain to you with a couple of nice code blocks rather than how I'm just going to throw it into the page.

These chunks will be helpful in page selection and optimizing page draws to multiple pages at once.

{% highlight text %}
; setting a page
; activeOffset = vgapage + (page * pageSize / 4);
; enable all planes
; outp 03c4h, 02h
; outp 03c5h, 0fh
{% endhighlight %}

Some really good references on this topic are

* [Mode X on Wikipedia](http://en.wikipedia.org/wiki/Mode_X)
* [Bob Pendleton's Mode X Tutorial](http://www.gameprogrammer.com/3-tweak.html)

Well, that's it for now.
