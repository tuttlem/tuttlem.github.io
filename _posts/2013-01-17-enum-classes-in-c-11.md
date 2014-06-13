---
layout: post
title: Enum Classes in C++11
date: 2013-01-17
comments: false
---

Enums in C/C++ have traditionally just been a pretty face for an integer value. That hasn't changed so much, but a new breed of enumeration has been added with C++11: enter the `enum` class. Enum classes have been introduced as a "strongly typed" enum. This gives you type safety in your enums so that you don't perform comparisons between enum types and if you do you'll need to explicitly define how the two should be compared. The other major benefit is improved scoping. Enum class values must always mention the enumeration that they belong to in order to be used. Here's an example of an `enum` class.

{% highlight cpp %}
enum class Suit { HEART, DIAMOND, CLUB, SPADE };
{% endhighlight %}

Immediately the only difference that you'll see here is the addition of the word `class`. You can still explicitly type and value your enumerations. Here's the same example above only written more verbosely.

{% highlight cpp %}
enum class Suit : unsigned char {
  HEART = 0, DIAMOND = 1, CLUB = 2, SPADE = 3
};
{% endhighlight %}

An example usage of these enum classes goes as follows.

{% highlight cpp %}
void show(const Suit s) {
  switch (s) {
    case Suit::HEART:
      cout << "hearts";
      break;

    case Suit::DIAMOND:
      cout << "diamonds";
      break;

    case Suit::CLUB:
      cout << "clubs";
      break;

    case Suit::SPADE:
      cout << "spades";
      break;
  }
}
{% endhighlight %} 

Well, there you have it. A brief tour of `enum` classes in C++11.