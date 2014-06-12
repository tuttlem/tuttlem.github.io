---
layout: post
title: OpenGL, GLUT & Haskell
date: 2013-01-02
comments: false
---

### Introduction

We're into some of the real good stuff here. One of my favourite topics in computer programming is graphics (2d and 3d). In this post, I get to make a start on some fun stuff and learn some Haskell at the same time. We're going to open an OpenGL window, clear the back buffer out and flip.

![Screenshot](http://1.bp.blogspot.com/-P3UGaGgWX0c/UOQmSYep7wI/AAAAAAAAAkQ/Bfl8-egapPc/s400/Screen+Shot+2013-01-02+at+10.20.45+PM.png)

### The code

{% highlight haskell %}
import Graphics.Rendering.OpenGL                
import Graphics.UI.GLUT                         
                                                
main :: IO ()                                   
main = do                                       
   -- initialize openGL                         
   (progname, _) <- getArgsAndInitialize        
   -- open up the application window            
   createWindow "Hello World"                   
   -- set the render proc and run the main loop 
   displayCallback $= display                   
   mainLoop                                     
                                                
display :: IO ()                                
display = do                                    
   -- clear the buffer and flip                 
   clear [ ColorBuffer ]                        
   flush                                        
{% endhighlight %}

Ok, ok, fair enough - this is almost a direct-rip from the [Haskell Wiki](http://www.haskell.org/haskellwiki/OpenGLTutorial1_, I'm not claiming it as my code. It's more of a bookmark as a good foot-hold in the OpenGL-Haskell marriage!

### Compilation

Getting your application compiled is cake, just make sure you specify GLUT as a package.

{% highlight bash %}
$ ghc -package GLUT Ogl.hs -o Ogl
{% endhighlight %}

That's what it's all about!
