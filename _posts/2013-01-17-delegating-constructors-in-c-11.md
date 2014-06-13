---
layout: post
title: Delegating Constructors in C++11
date: 2013-01-17
comments: false
---

One of the annoyances I've always had with C++ was its lack of elegant handling of constructors. You'd always be forced to swallow the bitter pill of an initializer function that each of your constructors would call. It always felt clunky to me. With C++11 introducing Delegating Constructors, this has all gone now. Take this simple `person` class, for example. No magic going on here, just a straight forward class and is how we'd implement multiple constructors without the use of an initialiser.

{% highlight cpp %}
class person {                                                     
  public:        
    /* blank person */
    person(void) 
      : first_name(""), middle_name(""), last_name("") { }                                                          
                      
    /* person with no middle name */                  
    person(const string& fn, const string& ln) 
      : first_name(fn), middle_name(""), last_name(ln) { }                                                          
    
    /* person with full name */
    person(const string& fn, const string& mn, const string& ln) 
      : first_name(fn), middle_name(mn), last_name(ln) { }                                                          
                                                                   
   public:                                                         
      string first_name,                                           
             middle_name,                                          
             last_name;                                            
};                                                                 
{% endhighlight %} 

Here we have a `person` class with three constructors all providing their own implementation of how a person object should initialized. This just isn't nice for a couple of reasons. The first is violation of the [DRY](http://en.wikipedia.org/wiki/Don't_repeat_yourself) principle, we're repeating ourselves the whole time which goes hand in hand with the second - each constructor initializes the object in its own way. So, the previous answer to this problem was to implement an initializer function which did this work for us.

{% highlight cpp %}
class person {                                                     
  public:     
    /* initializes the fields of the person class */
    void init(const string& fn, const string& mn, const string& ln) {
      this->first_name = fn; this->middle_name = mn; this->last_name = ln;
    }

    /* blank person */
    person(void) { 
      this->init("", "", "");
    }                                                          
                      
    /* person with no middle name */                  
    person(const string& fn, const string& ln) { 
      this->init(fn, "", ln);
    }                                                          
    
    /* person with full name */
    person(const string& fn, const string& mn, const string& ln) { 
      this->init(fn, mn, ln);
    }                                                          
                                                                   
   public:                                                         
      string first_name,                                           
             middle_name,                                          
             last_name;                                            
};                                                                 
{% endhighlight %}

This is better. We've got one way to initialize our class, we're not repeating ourselves. Life is good. But now (in C++11), there's a better way. Here I'll show you how to re-implement this class using delegating constructors for the most elegant of solutions.

{% highlight cpp %}
class person {                                                    
  public:                                                        
    person(void) 
      : person("", "", "") { }                                 
                                                                  
    person(const string& fn, const string& ln) 
      : person(fn, "", ln) { }                                 
                                                                  
    person(const string& fn, const string& mn, const string& ln)
      : first_name(fn), middle_name(mn), last_name(ln) { }     
                                                                  
  public:                                                        
    string first_name,                                          
           middle_name,                                         
           last_name;                                           
};                                                                
{% endhighlight %}

We've got our most general case constructor (in this case the construct taking in all three names) actually doing the work. The remaining two constructors then just leverage off the functionality defined in the general case. No re-implementation, no initialization function needed.