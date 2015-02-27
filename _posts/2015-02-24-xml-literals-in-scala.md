---
layout: post
title: XML literals in scala
date: 2015-02-24
comments: false
categories: [ "scala", "xml" ]
---

A really handy feature that has been included in the [Scala](http://www.scala-lang.org/) programming language is xml literals. The xml literals feature allows you to declare blocks of xml directly into your Scala code. As you'll see below, you're not limited to static xml blocks and you're also given the full higher-order function architecture to navigate and process your xml data.

### Definition and creation

You can create an xml literal very simply inside of your Scala code:

{% highlight scala %}
val people = 
	<people>
		<person firstName="John" 
				lastName="Smith" 
				age="25" 
				gender="M" />
		<person firstName="Mary" 
				lastName="Brown" 
				age="23" 
				gender="F" />
		<person firstName="Jan" 
				lastName="Green" 
				age="31" 
				gender="F" />
		<person firstName="Peter" 
				lastName="Jones" 
				age="23" 
				gender="M" />
	</people>
{% endhighlight %}

Scala then creates a variable of type [Elem](http://www.scala-lang.org/api/2.11.0/scala-xml/index.html#scala.xml.Elem) for us.

Xml literals can also be constructed <em>or generated</em> from variable sources

{% highlight scala %}
val values = <values>{(1 to 10).map(x => <value number={x.toString} />)}</values>
{% endhighlight %}

<strong>Take note</strong> that the value of `x` needs to be converted to a string in order to be used in an xml literal.

Another form of generation can be accomplished with a for comprehension:

{% highlight scala %}
val names = 
	<names>
	{for (name <- List("Sam", "Peter", "Bill")) yield <name>{name}</name>}
	</names>
{% endhighlight %}

### Working with literals

Once you have defined your xml literal, you can start to interact with the data just like any other [Seq](http://www.scala-lang.org/files/archive/api/2.11.0/#scala.collection.Seq) typed structure.

{% highlight scala %}
val peopleCount = (people \ "person").length
val menNodes = (people \ "person").filter(x => (x \ "@gender").text == "M")
val mensNames = menNodes.map(_ \ "@firstName")

println(s"Number of people: $peopleCount")
println(s"Mens names: $mensNames")
{% endhighlight %}

The usage of `map` and `filter` certainly provide a very familiar environment to query your xml data packets.

### Transform with RewriteRule

The [scala.xml.transform](http://www.scala-lang.org/api/2.10.1/index.html#scala.xml.transform.package) package include a class called [RewriteRule](http://www.scala-lang.org/api/2.10.1/index.html#scala.xml.transform.RewriteRule). Using this class, you can transform <em>(or re-write)</em> parts of your xml document.

Taking the sample <strong>person</strong> data at the top of this post, we can write a transform to remove all of the men out of the set:

{% highlight scala %}
val removeMen = new RewriteRule {
	override def transform(n: Node): NodeSeq = n match {
		case e: Elem if (e \ "@gender").text == "M" => NodeSeq.Empty
		case n => n
	}
}
{% endhighlight %}

We test if the `gender` attribute contains an "M", and if so we empty out the node. To apply this transform to the source data, we use the [RuleTransformer](http://www.scala-lang.org/api/2.10.1/index.html#scala.xml.transform.RuleTransformer) class.

{% highlight scala %}
val noMen = new RuleTransformer(removeMen).transform(people)
{% endhighlight %}

Another rule we can write, would be to remove any person who was over the age of 30:

{% highlight scala %}
val removeOver30s = new RewriteRule {
	override def transform(n: Node): NodeSeq = n match {
		case e: Elem if e.label == "person" && (e \ "@age").text.toInt > 30 => NodeSeq.Empty
		case n => n
	}
}
{% endhighlight %}

Pretty much the same. The only extra complexity is ensuring that we have an `age` attribute and getting it casted to an integer for us to perform arithmetic testing. 

The `RuleTransformer` class accommodates if we want to use these two transforms in conjunction with each other.

{% highlight scala %}
val noMenAndOver30s = new RuleTransformer(removeMen, removeOver30s).transform(people)
{% endhighlight %}

