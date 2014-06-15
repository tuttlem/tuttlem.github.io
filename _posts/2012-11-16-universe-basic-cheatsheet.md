---
layout: post
title: UniVerse BASIC cheatsheet
date: 2012-11-16
comments: false
categories: [ "UniVerse", "Database", "BASIC", "Cheatsheet", "Development" ]
---

A cheatsheet for writing UniVerse BASIC programs

### Program Management

Creating a directory for programs

{% highlight text %}
> CREATE.FILE BP 19
{% endhighlight %}

Creating a file to store a program

{% highlight text %}
> ED BP PROGRAM.NAME
{% endhighlight %}

Compiling a program

{% highlight text %}
> BASIC BP PROGRAM.NAME
{% endhighlight %}

Running a program

{% highlight text %}
> RUN BP PROGRAM.NAME 
{% endhighlight %}

### Sourcecode Editing

Formatting sourcecode

{% highlight text %}
----: FORMAT 
{% endhighlight %}

### File IO

Opening a file

{% highlight text %}
0001= OPEN 'filename' TO FILE.VAR ELSE STOP "Couldn't open filename" 
{% endhighlight %}

Reading a record from a file

{% highlight text %}
0001= READ RECORD.VAR FROM FILE.VAR, 'RELLEVEL' ELSE RECORD.VALUE = '' 
{% endhighlight %}

Writing a record to a file

{% highlight text %}
0001= WRITE RECORD.VAR TO FILE.VAR, 'TEST.WRITE' 
{% endhighlight %}

Reading a record from a file into an array

{% highlight text %}
0001= MATREAD RECORD.VAR FROM FILE.VAR, 'RELLEVEL' ELSE MAT RECORD.VAR = '' 
{% endhighlight %}

Writing a record from an array into a file

{% highlight text %}
0001= MATWRITE RECORD.VAR TO FILE.VAR, 'TEST.WRITE' 
{% endhighlight %}

Reading a single field from a file

{% highlight text %}
0001= READV TYPE.DESC FROM FILE.VAR, 'RELLEVEL', 1 ELSE TYPE.DESC = '' 
{% endhighlight %}

Writing a single field to a file

{% highlight text %}
0001= WRITEV TYPE.DESC TO FILE.VAR, 'TEST.WRITE', 1 
{% endhighlight %}

Deleting a record from a file

{% highlight text %}
0001= DELETE FILE.VAR, 'RELLEVEL' 
{% endhighlight %}

Locking records for update

{% highlight text %}
0000= READU RECORD.VAR FROM FILE.VAR, 'RELLEVEL' LOCKED 
0000= MATREADU RECORD.VAR FROM FILE.VAR, 'RELLEVEL' LOCKED 
0000= READVU TYPE.DESC FROM FILE.VAR, 'RELLEVEL' LOCKED 
{% endhighlight %}

Locking records from read

{% highlight text %}
0000= READL RECORD.VAR FROM FILE.VAR, 'RELLEVEL' 
0000= MATREADL RECORD.VAR FROM FILE.VAR, 'RELLEVEL' 
0000= READVL TYPE.DESC FROM FILE.VAR, 'RELLEVEL' 
{% endhighlight %}

An example record read

{% highlight text %}
0001= OPEN 'VOC' TO VOC.FILE ELSE STOP "Can't open VOC" 
0002= READ VOC.RECORD FROM VOC.FILE, 'RELLEVEL' ELSE VOC.RECORD = '' 
0003= PRINT VOC.RECORD 
{% endhighlight %}

An example record read to an array

{% highlight text %}
0001= DIM VOC.RECORD(10) 
0002= OPEN 'VOC' TO VOC.FILE ELSE STOP "Can't open VOC" 
0003= MATREAD VOC.RECORD FROM VOC.FILE, 'RELLEVEL' ELSE MAT VOC.RECORD = '' 
0004= FOR FIELD.NUMBER = 1 TO 10 
0005= PRINT VOC.RECORD(FIELD.NUMBER) 
0006= NEXT FIELD.NUMBER 
{% endhighlight %}

An example record read from a select list

{% highlight text %}
0001= OPEN 'VOC' TO VOC.FILE ELSE STOP "Can't open VOC" 
0002= EOF = 0 
0003= EXECUTE 'SELECT VOC WITH TYPE = "M"' 
0004= LOOP 
0005= READNEXT VOC.KEY ELSE EOF = 1 
0006= UNTIL EOF 
0007= PRINT VOC.KEY 
0008= REPEAT 
{% endhighlight %}

References

<a href="http://www.mannyneira.com/universe/">life, the universe, and everything</a>