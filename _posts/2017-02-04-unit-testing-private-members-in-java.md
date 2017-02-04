---
layout: post
title: Unit testing private members in Java
date: 2017-02-04
comments: false
categories: [ "unit", "testing", "java", "reflection" ]
---

Sometimes, you need to be able to look at the private members of your classes in order to *test* that something has gone to plan. [Unit testing](https://en.wikipedia.org/wiki/Unit_testing) is one scenario where this makes sense.

{% highlight java %}
import java.lang.reflect.Field;

. . .

private static byte[] getAddressSpace(Memory m) {
    try {
        Field field = Memory.class.getDeclaredField("addressSpace");
        field.setAccessible(true);
        return (byte[]) field.get(m);
    } catch (Exception e) {
        fail(e.getMessage());
    }

    return null;
}
{% endhighlight %}

By using the `getDeclaredField` method, passing the name of the field; the reflection framework will send back the definition. This field gets executed through the use of the `get` method, passing in the object instance.

To finish the picture here, we can also get access on methods that are private as well:

{% highlight java %}
Method method = targetClass.getDeclaredMethod(methodName, argClasses);
method.setAccessible(true);
return method.invoke(targetObject, argObjects);
{% endhighlight %}

Done.