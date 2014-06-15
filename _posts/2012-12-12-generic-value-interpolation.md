---
layout: post
title: Generic value interpolation
date: 2012-12-12
comments: false
categories: [ "C++", "Programming", "Interpolation" ]
---

### Introduction

The core of animation in software (not necessarily graphical) is movement. That movement is between known points to achieve and overall effect. Such well-known movements might be a human moving their legs alternatively so that they can <strong>walk </strong> or a sound constantly moving from low to high to make a <strong>siren</strong> or moving your mouse cursor <strong>automatically</strong>. All of these things share the same premise. An attribute of the object needs to have its position interpolated from a starting point to an ending point over a period of time.

Some of these interpolations repeat, some do not.

Today's post is about interpolating values between two points, but it's about doing it with any type and with any interpolation scheme. Let's break these parts down.

### Doing it with any type

The idea here is that anything (within reason) can be interpolated. In real C/C++ terms, I mean `short`, `int`, `long`, `float`, `double`, `char`, etc. A little more interestingly when dealing with spatial objects, we could define our own Cartesian co-ordinate object - give this object X and Y attributes and then interpolate this co-ordinate between two points.

### Interpolation schemes + some math

It's the small details that matter. Linear interpolation is the simplest form of interpolation leaning on the whole "the shortest distance between two points is a straight line". Linear interpolation will take its sequence values for progressing the object through its interpolation life using:

> y = c + mx

Which looks dangerously close to the standard formula you would use to draw a straight line. In this case `c` would be our starting point, `m` would be the distance (or length) between the start and end points and `x` defines how far along our interpolation progression we are taking the form in range of 0.0 up to 1.0.

Another interesting scheme is trigonometric in nature. Using a soft curve to get between two points can be a softer approach. Trigonometric interpolation will take its sequence values for progressing the object through its interpolation life using:

> y = c + (sin(x * (PI / 2)) * m)

Again, we have alike named variables in that `c` is our start point, `x` is our progress through the interpolated progression and `m` is the distance. Notice how our progress, `x` is multiplied by `(PI / 2)` only talking us 1 quarter through the progress of the curve. This is simply because we're at 0% progress at 0 degress and 100% at 90 degrees.

### Show me some code

First off, we have a base "tweener" object that will manage all of the mundane tasks that this interpolater will need to undergo in order to realize these tweened-values. You'll notice that it is in fact a templated C++ class.

{% highlight cpp %}
/** Provides a base interface for value interpolation */
template<class T>
class tweener {
public:
    /** Construction */
    tweener(T start, T end, const bool repeat, const Uint32 ticks);

    /** Destruction */
    virtual ~tweener(void);

    /** Resets the state of this tweener */
    void reset(void);

    /** Gets the value of this tweener */
    T value(void);

    /** Determines if the tweener has finished */
    const bool finished(void) { return _finished; }

protected:
    /** User supplied value getter */
    virtual T value_internal(const float prog) = 0;

protected:
    bool        _repeat, _finished;

    T          _start, _end;
    Uint32     _ticks, _prog_ticks;

    timer       *_timer;
};
{% endhighlight %}

Ignore the timer class for now. It's there just so we can make the interpolations happen with respect to time. We move ourselves along the interpolation progression just with some simple math with regards to time. To be a little more polite about things, we detect when we're at the start and just send the start value and detect when we're at the end and just send the end value - rather than calculating this over and over.

{% highlight cpp %}
/** Gets the value of this tweener */
template<class T>
T tweener<T>::value(void) {

    // make sure the timer has started
    if (!_timer->started()) {
        _timer->start();
        return _start;
    }

    // if we've already finished,
    // no more processing
    if (_finished) {
        return _end;
    }

    // accumulate split timing
    _prog_ticks += _timer->split();

    // need to handle boundaries if we're repeating
    if (_repeat) {
        // have we overflowed?
        if (_prog_ticks > _ticks) {
            _prog_ticks %= _ticks;
        }
    } else {
        // have we finished?
        if (_prog_ticks > _ticks) {
            _finished = true;
        }
    }

    // return the calculated value
    return this->value_internal((float)_prog_ticks/(float)_ticks);
}
{% endhighlight %}

The custom implementations for getting actual values (from types of interpolation) are as simple as this:

{% highlight cpp %}
/** Linear interpolation calculation */
template<class T>
T ltweener<T>::value_internal(const float prog) {
    return _start + (prog * _length);
}

/** Trigonometric interpolation calculation */
template<class T>
T ttweener<T>::value_internal(const float prog) {
    return _start + (sinf(prog * M_PI_2) * _length);
}
{% endhighlight %}

So you can see that these custom implementations marry very closely to the formula we specified above. I think that this design documents the implementer's intention very clearly by not having to worry about the progression code. The choice of using C++ templates also allows future implementations to target specific types. Whilst the implementations given in this article will work well for scalar values, they won't translate very well to complex class types that don't correctly implement operator overloading with mathematical correctness in mind. That being said, if the scope of implementation is beyond what the generic base provides, it's only a matter of specifying the specific type when implementing your own `value_internal` method.

Enjoy.

### A few tweeners more

Just playing around with the code set that I have here, I've been able to make logarithmic and parabolic tweeners relatively easy. It would be great to be able to control the co-effecients in the parabolic formula, but for the time being it's a quadratic class:

> y = x<sup>2</sup> + c

{% highlight cpp %}
/** Construction */
template<class T>
log_tweener<T>::log_tweener(T start, T end,
                   const bool repeat, const Uint32 ticks) :
tweener<T>(start, end, repeat, ticks) {
    // calculate the linear delta
    _length = (end - start);
}

/** User supplied value getter */
template<class T>
T log_tweener<T>::value_internal(const float prog) {
    return _start + (log10f(1.0f + (prog * 9.0f)) * _length);
}

/** Construction */
template<class T>
parab_tweener<T>::parab_tweener(T start, T end,
                   const bool repeat, const Uint32 ticks) :
tweener<T>(start, end, repeat, ticks) {
    // calculate the linear delta
    _length = (end - start);
}

/** User supplied value getter */
template<class T>
T parab_tweener<T>::value_internal(const float prog) {
    return _start + ((prog * prog) * _length);
}
{% endhighlight %}
