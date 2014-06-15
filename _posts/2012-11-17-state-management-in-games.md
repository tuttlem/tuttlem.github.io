---
layout: post
title: State Management in Games
date: 2012-11-17
comments: false
categories: [ "Programming", "State", "State Management", "Games", "C++" ]
---

The title of this post does mention "Games". In all honesty, with very minor modification you could change this implementation to suit any state requisition situation.

It's the boring stuff that stops me half-way through developing a computer game. Building a menu, a title screen, a high-score chart.. you know, the stuff that takes you away from that big eye-candy feature that's in the game.

Anyway, the implementation starts with a base class that manages some state. It's a class that has provision to allow any derivatives of it to provide reaction code. The state manager will actually cause these "reactions" to occur. You'll see this one shortly.

Here's the class itself:

{% highlight cpp %}	
class state {
  public:
     state(void) : _finished(false) {
        std::cout << "constructed" << std::endl;
     }
 
     virtual ~state(void) {
        std::cout << "destructed" << std::endl;
     }
 
     virtual bool init(void) {
        std::cout << "initializing" << std::endl;
        return true;
     }
 
     virtual bool teardown(void) {
        std::cout << "tearing down" << std::endl;
        return true;
     }
 
     virtual void progress(void) { }
 
     const bool finished(void) { return _finished; }
     state* next(void) { return _next.get(); }
     state& next_state(void) { return *_next.get(); }
  protected:
     boost::shared_ptr<state> _next;
     bool  _finished;
};
{% endhighlight %}

Of course, this is all nice and abstract. It doesn't really do anything. Here's a simple derivative. Don't expect too much, it really doesn't do that much at all:

{% highlight c++ %}	
class first_state : public state {
  public:
     first_state(void) : state() { }
 
     virtual bool init(void) {
        return state::init();
     }
 
     virtual bool teardown(void) {
        return state::teardown();
     }
 
     virtual void progress(void) {
        std::cout << "progressing ..." << std::endl;
        _next.reset();
        _finished = true;
     }
};
{% endhighlight %}

Finally, we have the state machine. It is the kernel, the guy "in charge". This will progress execution through the chain of states to ensure the story-board is presented to the user in such a way that they have a nice feeling of continuity throughout the game:

{% highlight c++ %}
class state_machine {
  public:
     state_machine(void) { }
     state_machine(state *s) : _current(s) { }
     ~state_machine(void) { }
 
     void run(void) {
 
        if (_current)
           _current->init();
 
        while (_current) {
 
           _current->progress();
 
           if (_current->finished()) {
              _current->teardown();
              _current.reset(_current->next());
 
              if (_current) {
                 _current->init();
              }
           }
 
        }
 
     }
 
  private:
     boost::shared_ptr<state> _current;
 
};
{% endhighlight %}

To really make this of immediate use to a games programmer, I suggest the reader takes this code as an example and implements the following necessities:

### Time management

Giving the programmer the ability to tell how long it was between frame renders or even being able to control the number of frames processed per second is a requirement. This is the heartbeat/lifeblood to giving your program the ability to exist in a human (or non-human) consistent time scale.

### Eventing

Most media libraries define some sort of eventing to allow the library to interact with the hosting operating system. These interrupts or notion of "important incoming information" needs to be embraced into the state framework as one of the most important side-effects can come from external interation (keyboards, mouse, etc).

Enjoy.