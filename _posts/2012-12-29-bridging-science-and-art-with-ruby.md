---
layout: post
title: Bridging Science and Art with Ruby
date: 2012-12-29
comments: false
---

### Introduction

Having a love for music and technology at the same time can be a dangerous business. You can really fool yourself into thinking that you can boil art down into algebraic or procedural recipes that you can just turn the handle on. The frustration sets in when it's just not that black-and-white. In this post, I've put some Ruby code together that takes in an array of [musical intervals](http://en.wikipedia.org/wiki/Interval_(music)) and attempts to give that array of intervals a name. 

In music, this is better known as a [chord](http://en.wikipedia.org/wiki/Chord_(music)) or [arpeggio](http://en.wikipedia.org/wiki/Arpeggio).

### Assumptions

As above, I want an array of intervals in so this is going to take shape in the form of an integer array. These integers will be the individual distances (in semi-tones) from the root which will start at 0. For reference, here's a chart that I used during development.

First Octave|Second Octave|Interval Names
------------|-------------|------------------
<em>0</em>	| <em>12</em> | Root/Unison
1			| 13		  | Minor Second/Flat Nine
2			| 14		  | Major Second/Nine
3			| <em>15</em> | Minor Third
4			| <em>16</em> | Major Third
5			| 17		  | Perfect Fourth/Eleventh
6			| 18		  | Tritone/Sharp Eleven
7		 	| <em>19<em>  | Perfect Fifth
8			| 20		  | Minor Sixth/Flat Thirteen
9			| 21		  | Major Sixth/Thirteenth
10			| <em>22</em> | Minor Seventh
11			| <em>23</em> | Major Seventh

</div><div><br />Intervals that are <em>emphasised</em>, didn't really factor into the overall solution as they add nothing to the chord's quality or extension. Well, this may not be entirely true, some jazz-heads have probably already got their cross-hairs locked onto me, ready to flame away.

### Gathering facts

First of all, we need the array of intervals into our class. We'll always assume that there is a root and if there isn't one, we'll pop one into the array for good measure.

{% highlight ruby %}
class Chord

   def initialize(intervals)
      if not intervals.include?(0)
         intervals = [0] + intervals
      end

      @intervals = intervals
   end
   
end
{% endhighlight %}

We're now managing an array of intervals. What we need to do is ask questions of that array so we can gather information about our chord. The following methods ask the important questions in the first octave.

{% highlight ruby %}
def minor?
   @intervals.include?(3)
end

def major?
   @intervals.include?(4)
end

def has_fourth?
   @intervals.include?(5)
end

def has_tritone?
   @intervals.include?(6)
end

def has_augmented_fifth?
   @intervals.include?(8)
end

def has_sixth?
   @intervals.include?(9)
end

def dominant?
   @intervals.include?(10)
end

def has_major_seven?
   @intervals.include?(11)
end
{% endhighlight %}

With just this base information we can find a lot out about the chord, but it's not nearly enough. We need to be sure. The best part about putting these basic building blocks in place is that our methods are going to read a little more human from now on. Here are some more fact finders.

{% highlight ruby %}
def diminished?
   self.minor? and self.has_tritone?
end

def augmented?
   self.major? and self.has_augmented_fifth?
end

def has_third?
   self.minor? or self.major?
end

def suspended?
   not self.has_third? and (self.has_second? or self.has_fourth?)
end

def has_seventh?
   self.dominant? or self.has_major_seven?
end
{% endhighlight %}

Finally we have a couple more tests that we need to conduct on the array in the upper octave, otherwise none of the jazz-guys are going to get their chords! These again will form syntactic sugar for the main feature, `to_s`.

{% highlight ruby %}
def has_minor_ninth?
   @intervals.include?(13)
end

def has_ninth?
   @intervals.include?(14)
end

def has_augmented_ninth?
   @intervals.include?(15)
end

def has_eleventh?
   @intervals.include?(17)
end

def has_sharp_eleventh?
   @intervals.include?(18)
end

def has_minor_thirteenth?
   @intervals.include?(20)
end

def has_thirteenth?
   @intervals.include?(21)
end
{% endhighlight %}

### Piecing it together

It's good that we know so much about our chord. It means that we can ask questions and make decisions based on the outcomes of these questions. Upon reflection, I did have another idea on assembling those fact-finding methods into a more flexible but highly-unhuman set of methods that when you asked about a particular interval, you'd get back either `:flat`, `:natural` or `:sharp`. Perhaps I'll try it in another revision; I digress. All the facts are in front of us, let's construct a string from these facts. Now here's where the awkward bridge between science and art starts to burn a little, then a lot. Questions must not only be asked of the array, but they have to be asked in the right order otherwise you'll get a different result.</div><div><br /></div><div>That's naming for you though. It doesn't read pretty, but here it is.

{% highlight ruby %}
def to_s

   quality = "Major " if self.major?
   quality = "Minor " if self.minor?
   quality = "Augmented " if self.augmented? and not self.has_seventh?
   quality = "Diminished " if self.diminished? and not self.has_seventh?

   extensions = ""

   if not self.suspended?

      if (self.major? and self.has_major_seven?) or
         (self.minor? and self.dominant?)
         extensions = "Seventh "
      else
         if self.dominant?
            quality = "Seventh "
         end
      end

   else
      quality = "Suspended "
      extensions = "Second " if self.has_second?
      extensions = "Fourth " if self.has_fourth?
   end

   if self.has_sixth?
      if self.has_tritone?
         quality = "Diminished "
         extensions = "Seventh "
      else
         extensions += "Sixth "
      end
   end

   if not self.diminished? or self.has_seventh?
      extensions += "Flat Five " if self.has_tritone?
   end

   if not self.augmented? or self.has_seventh?
      extensions += "Sharp Five " if self.has_augmented_fifth?
   end

   if self.has_seventh?
      extensions += "Flat Nine " if self.has_minor_ninth?

      if self.has_ninth?
         if self.dominant?
            quality = ""
            extensions = "Ninth "
         else
            extensions += "Nine"
         end
      end

      if self.has_eleventh?
         if self.dominant?
            quality = ""
            extensions = "Eleventh "
         else
            extensions += "Eleven"
         end
      end

      if self.has_thirteenth?
         if self.dominant?
            quality = ""
            extensions = "Thirteenth "
         else
            extensions += "Thirteen"
         end
      end

      extensions += "Sharp Nine " if self.has_augmented_ninth?
      extensions += "Sharp Eleven " if self.has_sharp_eleventh?
   end

   extensions = extensions.strip()
   chord_name = quality.strip()
   chord_name += " " if chord_name.length > 0 and extensions.length > 0
   chord_name += extensions if extensions.length > 0

   chord_name
end
{% endhighlight %}

Woosh. That was a lot of if-treeing. There has to be a better way of doing this, but by my calculations using this code I've covered the following use-cases off:

* Major
* Minor
* Diminished
* Diminished Seventh
* Augmented
* Seventh
* Minor Seventh
* Major Seventh
* Suspended Second
* Suspended Fourth
* Seventh Flat 5
* Seventh Sharp 5
* Major Seventh Flat 5
* Major Seventh Sharp 5
* Minor Seventh Flat 5
* Minor Seventh Sharp 5
* Ninth
* Eleventh
* Thirteenth

There's a few chords there, but there are just so many more and this code may work for them. Those other cases just haven't made it into my unit tests yet.

### Plans

I really want to make this part of a bigger application that I'm writing at the moment. I was motivated to write a unit like this because I didn't want chord definitions just sitting in a database, I wanted the application to do the think work. Possibilities from here also reach into inversions and slash chords. It would be easy to permute the list of intervals and enumerate all of the inversion scenarios.

Anyway, until next time!