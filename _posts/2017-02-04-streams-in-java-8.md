---
layout: post
title: Streams in Java 8
date: 2017-02-04
comments: false
categories: [ "java", "functional", "stream" ]
---

The [functional programming paradigm](https://en.wikipedia.org/wiki/Functional_programming) has certainly seen a lot of attention of late, where some of the features that can be exploited from it have properties that assist in scale programming.

In today's post, I'll walk through [Java 8 streams](https://docs.oracle.com/javase/8/docs/api/java/util/stream/package-summary.html) which allow you to treat collection data structures in a functional way.

## What is a stream?

A stream prepares a collection of elements in such a way that you can operate on it functionally. The higher-order functions that you'd commonly see in a functional arrangement are:

* [map](https://en.wikipedia.org/wiki/Map_(higher-order_function))
* [filter](https://en.wikipedia.org/wiki/Filter_(higher-order_function))
* [reduce](https://en.wikipedia.org/wiki/Fold_(higher-order_function))

## An example

To start with, we need to define some test data. We're going to work with artists and songs:

{% highlight java %}
/** Song.java */
public class Song {
  public String name;
  public int year;
  public int sales;

  public Song(String name, int year, int sales) {
    this.name = name;
    this.year = year;
    this.sales = sales;
  }

  public String getName() {
    return name;
  }

  public int getYear() {
    return year;
  }

  public int getSales() {
    return sales;
  }
}

/* Artist.java */
public class Artist {
  private String name;
  private int age;
  private List<Song> songs;

  public Artist(String name, int age, List<Song> songs) {
    this.name = name;
    this.age = age;
  }

  public String getName() {
    return name;
  }

  public int getAge() {
    return age;
  }

  public List<Song> getSongs() {
    return songs;
  }
}
{% endhighlight %}

Now that we have a basic structure of artists and songs, we can define some test data to work with.

Don't judge me.

{% highlight java %}
Artist rickAstley = new Artist(
  "Rick Astley",
  50,
  Arrays.asList(
    new Song(
      "Never Gonna Give You Up",
      1987,
      500000
    )
  )
);

Artist bonJovi = new Artist(
  "Jon Bon Jovi",
  54,
  Arrays.asList(
    new Song(
      "Livin' on a Prayer",
      1986,
      3400000
    ),
    new Song(
      "Wanted Dead or Alive",
      1987,
      4000000
    )
  )
);

List<Artist> artists = Arrays.asList(bonJovi, rickAstley);
{% endhighlight %}

Ok, now that the boilerplate is out of the way; we can start the fun stuff.

## Map

[map](https://en.wikipedia.org/wiki/Map_(higher-order_function)) is a function that allows you to apply a function to each element of a list; transforming and returning it. We can grab just the artist's names with the following:

{% highlight java %}
artists.stream()
  .map(Artist::getName)
  .collect(toList())
{% endhighlight %}

`toList` comes out of the `java.util.stream.Collectors` class. So you can `import static`:

{% highlight java %}
import static java.util.stream.Collectors.toList;
{% endhighlight %}

## Mapping deeper

`flatMap` allows you to perform the `map` process on arrays of arrays. Each artist has an array of songs, so we can flat map (at the artist level) to emit a flat list of songs:

{% highlight java %}
artists.stream()
  .flatMap(a -> a.getSongs().stream())
  .map(Song::getName)
  .collect(toList())
{% endhighlight %}

## Filter

We only want artists over the age of 52.

{% highlight java %}
artists.stream()
  .filter(a -> a.getAge() > 52)
  .map(Artist::getName)
  .collect(toList())
{% endhighlight %}

## Reduce

We can aggregate all of the sales figures into a single value:

{% highlight java %}
artists.stream()
  .flatMap(a -> a.getSongs().stream())
  .mapToInt(Song::getSales)
  .sum()
{% endhighlight %}


Ok, so this has been a whirlwind tour of the Java 8 functional interface.