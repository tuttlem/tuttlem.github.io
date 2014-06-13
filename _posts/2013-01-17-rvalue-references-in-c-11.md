---
layout: post
title: Rvalue references in C++11
date: 2013-01-17
comments: false
---

This is the biggest change (shift anyway) in C++'s thinking with the new standard. This preview of Rvalue references probably isn't pitched at the novice and a bit of prior knowledge is assumed. You'll want to know the differences between an [Lvalue and an Rvalue](http://publib.boulder.ibm.com/infocenter/comphelp/v7v91/index.jsp?topic=%2Fcom.ibm.vacpp7a.doc%2Flanguage%2Fref%2Fclrc05lvalue.htm), you'll also want to understand how [temporary objects work](http://publib.boulder.ibm.com/infocenter/lnxpcomp/v8v101/index.jsp?topic=%2Fcom.ibm.xlcpp8l.doc%2Flanguage%2Fref%2Fcplr382.htm) but most importantly why they're evil in some scenarios. With all of that information under our belt, I can see that the Rvalue reference came to fruition to resolve the copy problem. Returning a temporary object from a function that you'd use in your program would cause an expensive copy operation, now this is is resolved with move semantics. Rather than that expensive copy operation, the value that's returned is pilfered to the calling code and the temporary object is left empty, ergo a move occurs. An Rvalue reference is defined in code like so.

{% highlight cpp %}
person&& p = get_temp_person();
{% endhighlight %}

The double ampersand `&&` tells us that it's an Rvalue reference. We can give our own classes the ability to "move" a value by introducing a move constructor and operator like so.

{% highlight cpp %}
class person {                       
   public:                           
    person(person&& p);            
                                     
    person&& operator=(person&& p);
                                     
  public:                           
    string first_name,             
           middle_name,            
           last_name;              
};                                   
{% endhighlight %}

The STL containers have all been optimised to use move syntax, so you'll pick up this copy-free functionality on the way.