---
layout: post
title: Uniform Initialization with C++11
date: 2013-01-17
comments: false
---

Initialization of variables types has received a face lift with the new version of the C++ standard. Previously trying to initialize a populated list was impossible and you were left writing helper functions to transform array values into other container types, or worse you'd end up with code that looked like this.

{% highlight cpp %}
vector<int> i;

// populate the vector
i.push_back(1);
i.push_back(2);
i.push_back(3);
i.push_back(4);
{% endhighlight %}

C++11 includes a feature called "Uniform Initialization" which aims to solve this problem. The above code now turns into this.

{% highlight cpp %}
vector<int> i {1, 2, 3, 4};
{% endhighlight %}

Much better. This isn't where the convenience ends though. Take the following example. Because the `person` class defines a constructor taking in `first_name`, `last_name` and `age` we are able to use uniform initialization to setup these variables.

{% highlight cpp %}
// declare the person class
class person {
  public:
    person(void) = default;
    person(const string& fn, const string &ln, const int a) 
      : first_name(fn), last_name(ln), age(a) { }
    virtual ~person(void) = default;

  private:
    string first_name, last_name;
    int age;
};

// initialize a person
person p { "John", "Smith", 25 };

// initialize a vector of people
vector<person> people { {"Mary", "Brown", 21},
                        {"Joe", "Jones", 35},
                        p,
                        {"Sally", "Green", 32} };
{% endhighlight %}

The same syntax works with any of the enumerated containers. Using the `person` class from above, we can make a map of employee records by doing the following.

{% highlight cpp %}
map<int, person> employees {
  {1, {"Mary", "Brown", 21}},
  {2, {"Joe", "Jones", 35}},
  {3, {"John", "Smith", 25}},
  {4, {"Sally", "Green", 32}}
};
{% endhighlight %}

You can see how this syntax makes the initialization process a much more pleasant experience in C++ now. That's all for this one.