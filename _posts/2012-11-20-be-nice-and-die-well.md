---
layout: post
title: Be nice and die well
date: 2012-11-20
comments: false
---

It's a morbid title, I agree but it is quite important. Unix daemons really need to listen to the messages (read as: signals) being sent to them and responding correctly. At the very least, if every daemon implemented the following it'd be a breath of fresh air:

{% highlight c %}
/** Responds to signals of interest */                                                                    
void daemon_signalhandler(int sig) {                                      
                                                                            
   switch (sig) {                                                         
                                                                            
      case SIGHUP:                                                        
         /* this should just refresh configs and restart */                                      
         break;                                                           
                                                                            
      case SIGTERM:                                                       
         /* mark the server as no longer running */                                               
         break;                                                           
   }                                                                      
                                                                            
}                                                                         
{% endhighlight %}

This block of code by itself is pretty useless. The other half of the equation is attaching this function to the signals of interest as well as ignoring signals we're not interested in (or that we're not interested in those signals turning our daemon to toast).

{% highlight c %}
/* attach the signal handlers now */                                  
signal(SIGCHLD, SIG_IGN);                                             
signal(SIGTSTP, SIG_IGN);                                             
signal(SIGTTOU, SIG_IGN);                                             
signal(SIGTTIN, SIG_IGN);                                             
signal(SIGHUP, daemon_signalhandler);                                 
signal(SIGTERM, daemon_signalhandler);                                
{% endhighlight %}

Well, that's it really and remember - die well.