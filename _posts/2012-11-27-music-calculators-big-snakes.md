---
layout: post
title: Music, Calculators & Big Snakes
date: 2012-11-27
comments: false
---

### Introduction

I play a lot of music. I mean <strong>a lot</strong> of music. Guitar, Bass, Piano, Drums, Harp -- what ever I can get my hands on, I'll give it a go. So, the neurons in charge of keeping my technical side in check may have made their way into the creative side of my brain.

The net result was me creating a project that mashes mathematics, data analysis and music theory into a few classed called [musica](https://github.com/tuttlem/musica).

### So what?

Yeah, basically.. But, I am who I am - so it' more of a matter of "why not?".

I'd previously tried building a Ruby on Rails application to give me graphical representations of musically related elements on screen. A scale is a pattern of intervals realised once a root note is put in place. This scale can then be harmonised at every step of the scale by employing every second note from the specific step. How complex these harmonised chords are will depend on how many steps you include in the chords that you build.

The most interesting part of this library, I think, is its distinct lack of database. It has made the code quite verbose in parts but I intend to fix this, even if it does mean employing an intermediary database at a later stage.

### What can it do?

The major, harmonic & melodic minor scales (and all of their modes) are statically provisioned. You can use these in code to perform operations like voicing a scale when used in conjunction with a Note object.

Immediately, we can get information about notes:

{% highlight pycon %}
>>> Notes.by_distance(0)
<musica.notes.Note object at 0x10f961090>
>>> Notes.by_distance(0).__unicode__()
'c'
{% endhighlight %}

Information about scales:

{% highlight pycon %}
>>> Scales.scales[0]
<musica.scales.Scale object at 0x10f963090>
>>> Scales.scales[0].name
'Ionian'
>>> Scales.scales[0].intervals
[<musica.intervals.Interval object at 0x10f961950>, <musica.intervals.Interval object at 0x10f9619d0>, <musica.intervals.Interval object at 0x10f961a50>, <musica.intervals.Interval object at 0x10f961a90>, <musica.intervals.Interval object at 0x10f961b10>, <musica.intervals.Interval object at 0x10f961b90>, <musica.intervals.Interval object at 0x10f961c10>]
{% endhighlight %}

Voicing a scale (in this instance voicing Ionian (major) over C) 

{% highlight pycon %}
>>> steps = Scales.scales[0].voice(Notes.by_distance(0))
>>> for s in steps:
...     print 'note: ' + s['note'].__unicode__() + ', interval: ' + s['interval'].short_name
... 
note: c, interval: PU
note: d, interval: M2
note: e, interval: M3
note: f, interval: P4
note: g, interval: P5
note: a, interval: M6
note: b, interval: M7
{% endhighlight %}

### Still so much to do ...

Still, we need to get chords harmonising from these voiced scales. These are just patterns after all. An intelligent chord builder would also be of value I think. Something where we don't have to explicitly map out chord names to interval sets statically. Just knowing that a root, major third and perfect fifth are a major chord.

That'd be a cool calculator, I think.