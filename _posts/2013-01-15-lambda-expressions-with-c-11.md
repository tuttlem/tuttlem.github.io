---
layout: post
title: Lambda Expressions with C++11
date: 2013-01-15
comments: false
categories: [ "C++", "C++11", "Lambda" ]
---

### Introduction

A whole raft of goodness has been delivered with the most recent C++ standard, [C++11](http://en.wikipedia.org/wiki/C%2B%2B11). One of these features is the inclusion of lambda expressions. Today's post will take you through the basic syntax for lambdas in C++.

### Basic Assignment

You can assign a lambda to a variable quite simply with the following syntax.

{% highlight cpp %}
#include <iostream>

int main(int argc, char *argv[]) {
  // assignment to an "auto" variable
  auto f1 = [] { std::cout << "Hello, World" << std::endl; };

  f1();

  return 0;
}
{% endhighlight %}

I agree that this is a pretty ass-about-face way of printing "Hello, World" to the screen - but it's done through C++11's lambda syntax. Passing variables into a lambda expression and getting return values is quite trivial also.

{% highlight cpp %}
// implicit return types (handled by the complier)
auto add_imp = [] (int a, int b) { return a + b; };

// explicit return types (specified by user)
auto add_exp = [] (int a, int b) -> int { return a + b };
{% endhighlight %}

You can nest lambdas pretty easily also. Heres is a multiply-then-divide example where the division operation is the nested operation. Multiplication occurs at the top-level lambda.

{% highlight cpp %}
auto muldiv = [] (float a, float x, float y) {
  return [] (float v, float u) {             
    return v / u;                           
  }(a * x, y);                               
};                                            
{% endhighlight %}

This syntax also allows you to define higher-order functions, so that you can return function object back to the caller for later use. Here, I've made a multiplier factory. You give it one side of the multiplication and it'll hand you back a function that will multiply by that number.

{% highlight cpp %}
auto mulBy = [](int x) {
  return [=](int y) { return x * y; };
};

auto mulBy2 = mulBy(2);
auto mulBy10 = mulBy(10);
{% endhighlight %}

We've done something a little bit different here. You can see that we've used a term inside the square brackets for the returned function. C++ having a major focus on performance gives the developer as much flexibility as possible when handling values. The information specified within the square braces tells the lambda closure how to handle variables referenced within.

### Handling outside state within a lambda

The developer describes to the lambda how she wants variables captured by making specifications within the square brackets. Some examples of what you might see look like this.

| Specification | Meaning
|---------------|----------------------------------------------------
| `[]`			| Don't capture anything
| `[&]`			| Capture any variable by reference
| `[=]`			| Capture any variable used making a copy of it
| `[=, &x]`		| Capture any variable used making a copy of it except for `x`. Capture `x` by reference.
| `[y]`			| Capture `y` by making a copy but nothing else.
| `[this]`		| Capture the enclosing class' pointer

So, we can be quite specific in telling the compiler how we want referenced variables handled within our lambda closure. Finally, I want to present some code on using lambdas with existing constructs. In this example, I'll reduce a list of integers by accumulating them into a variable referenced outside of a closure.

{% highlight cpp %}
#include <iostream>                              
#include <vector>                                
#include <algorithm>                             

int main(int argc, char *argv[]) {               

  // vector to reduce
  std::vector<int> l;                           
  l.push_back(1);                               
  l.push_back(2);                               
  l.push_back(3);                               

  // reduced result
  int i = 0;                                    

  // reduction by accumulation
  std::for_each(l.begin(), l.end(),             
    [&i](int n) { i += n; }                    
  );                                            

  std::cout << "reduced to: " << i << std::endl;

  return 0;                                     
}                                                
{% endhighlight %}

You can see that is is quite a fluent style for writing lambdas. This post only scratches the surface. Applying these in a real project is going to be key to discovering the depths of lambdas, but they're alive and well in C++(11) land, that's for sure.
