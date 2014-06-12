---
layout: post
title: Function Pointers, the MacGyver of C
date: 2012-12-08
comments: false
---

### Introduction<

It's a very well studied topic in the C field. Many people have written tutorials on how to use function pointers and what they are. [The function pointer tutorial](http://www.newty.de/fpt/fpt.html) is always a good read for the uninitiated or seasoned professional who's function pointer theory is a little rusty.

This write up isn't going to try and fill the shoes of the basic-square-1-hello-world style tutorial. I just wanted to talk about how I've used them to better a piece of code and make a system a bit more extensible overall.

### What are they?

Like any data pointer a function pointer is an address. It's an address that will point to the entry-point of some code. It's that simple. The complexity comes into the argument when you start to add C syntax. C likes to be very descriptive with the data that you're feeding functions and function parameters are no exception.

{% highlight c %}
/* a function that takes no arguments and returns no value called "func1" */
void (*func1)() = NULL;

/* a function that returns an integer and takes two integer arguments called "func2" */
int (*func2)(int, int) = NULL;
{% endhighlight %}

Ok, simple example time over. It's onto what this article is really about.

### Problem description

I have a game that has a few game states. They look like this:

* Introduction credits
* Game title
* Menu
* Game play
* Game over
* High score board

Each of these states all have functionality that they need to accomplish. The basic specification for a game state goes as follows:

* Needs to be able to be initialized
* Needs to be able to have its resources freed (tear down)
* Needs to be able to update its internal logic
* Needs to be able to render its internal state
* Needs to be able to respond to events (keyboard, etc)

Good. We've established that we have a set of game states and they all attract a set of actions that we'd like them to perform.

### Getting down to it

So, let's define the actions first (as function pointers) and we can then fill in the implementation later:

{% highlight c %}
/* function signature for initializing this state */    
typedef void (*init_func)();                            
                                                        
/* function signature for tearing down this state */    
typedef void (*teardown_func)();                        
                                                        
/* function signature for the render function */        
typedef void (*render_func)();                          
                                                        
/* function signature for the update function */        
typedef void (*update_func)(Uint32, Uint32);            
                                                        
/* function signature for the keyboard input event */   
typedef void (*keyboard_func)(SDL_KeyboardEvent*);      
{% endhighlight %}

Slightly different from above is the use of the "typedef" keyword. This allows us to define these function pointers as types so that we can treat them just as we do any other type. These types that we've defined are the outline to what we need. We still have the colour in, but we've got a framework ready to go. Lets define our function pointer variables using the types we defined above.

{% highlight c %}
/* we need to define the pointers themselves so that
   the game loop can read them and the game module can
   write to them */
init_func game_init          = NULL;
teardown_func game_teardown  = NULL;
render_func game_render      = NULL;   
update_func game_update      = NULL;   
keyboard_func game_keyboard  = NULL;   
{% endhighlight %}

Simple enough. Now that we have these variables define (close-by to our game loop), we can write a game loop that will look a little something like this:

{% highlight c %}
/* runs the game loop */
void run_game(void) {
    SDL_Event event;

    while (!game_finished) {

        /* process events here */

        /* if we've got a keyboard event process
           and we've got a valid keyboard handler */
        if (game_keyboard != NULL) {
            game_keyboard(event.key);
        }

        /* if we've got a valid update function, update
           the game logic */
        if (game_update != NULL) {
            game_update(frame, total);
        }

        /* if we've got a valid render function, render
           the game state to screen */
        if (game_render != NULL) {
            game_render();
            /* flip backbuffer to front */
        }
    }
}
{% endhighlight %}

This is psuedo code really. There's a whole heap of infrastructure that you'll need in order to make this work. You can see that only 3 of the functions we require are being are being used at the moment. We still need to get the init and teardown involved. The game module also needs a way to shift between game states. Thankfully, both of these functions come together in the one function:

{% highlight c %}
/**                                                
 * Sets the handlers for the render, update and    
 * keyboard */                                     
void set_game_state(int state) {                   
                                                  
   /* if we have a valid teardown function to call,
      call it now */
   if (game_teardown != NULL)
      game_teardown();
 
   /* based on the requested state */              
   switch (state) {                                
      case GAME_STATE_INGAME:                           
                                                   
         /* assign the game handlers over */      
         game_init = init_game;
         game_teardown = teardown_game;
         game_render = render_game;               
         game_update = update_game;               
         game_keyboard = keyboard_handler_game;   
                                                   
         break;                                    
                                                   
      default:                                     
                                                   
         /* clear out any of the game state */     
         game_init = NULL;
         game_teardown = NULL;
         game_render = NULL;                      
         game_update = NULL;                      
         game_keyboard = NULL;                    
                                                   
         break;                                    
                                                   
   }                                               
      
   /* if we've got a valid initialization routine,
      call it now */
   if (game_init != NULL) {
      game_init();
   }
                                             
}                                                  
{% endhighlight %}

So, this function tearsdown the existing game state (if we have a state), and initializes if we initialize to a valid state. Easy!! Well, this has been just one possible use for function pointers. You can do so much, much more with them, but I thought I'd share my use of them here!