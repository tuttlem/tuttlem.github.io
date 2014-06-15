---
layout: post
title: Getting started with GLUT in Linux
date: 2013-12-28
comments: false
categories: [ "Linux", "C", "Programming", "GLUT", "OpenGL" ]
---

### Introduction

[GLUT](http://www.opengl.org/resources/libraries/glut/) is the OpenGL Utility Toolkit which is a standard set of APIs that you should be able to use on any platform to write OpenGL programs. It takes care of the boilerplate code that your applications would need to integrate with the host windowing system. More can be found about GLUT on its [website](http://www.opengl.org/resources/libraries/glut/).

Today's post, I'll focus on getting your Linux environment up to speed to start writing programs with this framework.

### Installation

In order to write programs using this library, you'll need to install the development library. Using your favorite package manager, you'll need to install freeglut.

{% highlight bash %}
$ sudo apt-get install freeglut3-dev
{% endhighlight %}

After that's finished, it's time to write a test application to make sure everything went to plan.

### A Simple Example

The following program will just open a window and continually clear the window.

{% highlight c %}
#include <GL/glut.h>

void resize(int width, int height) {

   // avoid div-by-zero
   if (height == 0) {
      height = 1;
   }

   // calculate the aspect ratio
   float ratio = width * 1.0 / height;

   // put opengl into projection matrix mode
   glMatrixMode(GL_PROJECTION);

   // reset the matrix
   glLoadIdentity();
   // set the viewport
   glViewport(0, 0, width, height);
   // set the perspective
   gluPerspective(45.0f, ratio, 0.1f, 100.0f);

   // put opengl back into modelview mode
   glMatrixMode(GL_MODELVIEW);

}

void render(void) {

   // just clear the buffers for now
   glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

   // flip the buffers
   glutSwapBuffers();

}


int main(int argc, char *argv[]) {

   // initialize glut
   glutInit(&argc, argv);

   // setup a depth buffer, double buffer and rgba mode
   glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);

   // set the windows initial position and size
   glutInitWindowPosition(50, 50);
   glutInitWindowSize(320, 240);

   // create the window
   glutCreateWindow("Test Glut Program");

   // register the callbacks to glut
   glutDisplayFunc(render);
   glutReshapeFunc(resize);
   glutIdleFunc(render);

   // run the program
   glutMainLoop();

   return 0;
   
}
{% endhighlight %}

Putting this code into "test.c", we built it into a program with the following command:

{% highlight bash %}
$ gcc test.c -lGL -lGLU -lglut -o test
{% endhighlight %}

That's it! Run "test" at the command prompt and if everything has gone to plan, you've installed freeglut correctly!