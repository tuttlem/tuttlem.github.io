---
layout: post
title: Exploring blocks in ruby
date: 2015-01-29
comments: false
categories: [ "ruby", "blocks" ]
---

[Ruby](https://www.ruby-lang.org/en/) has a very cool feature called <strong>code blocks</strong>. Sometimes referred to as <em>closures</em>, code blocks are custom pieces (or blocks) of ruby code that you specify to functions that inject your code block whenever the `yield` keyword is used.

In today's post, I'm just going to present a simple example and usage.

### Source data

The example we're going to use will be a Manager to Employee style relationship. A `Person` class is going to manage an array of `Person` objects that we'll classify as staff for that person. Here's the class definition:

{% highlight ruby %}
class Person

	attr_reader :name, :position, :staff

	def initialize(name, position, staff=[])
		@name = name
		@position = position
		@staff = staff
	end

	def to_s
		@name + ' (' + @position + ') '
	end
end
{% endhighlight %}

So a `Person` will have a `name`, `position` and `staff`. Some sample data using this structure might look as follows:

{% highlight ruby %}
bob = Person.new('Bob', 'Support Officer')
joe = Person.new('Joe', 'Support Officer')
mary = Person.new('Mary', 'Technology Director', [bob, joe])
{% endhighlight %}

### Simple example

`mary` is the manager for `joe` and `bob`. Using the `Array` function `each`, we can use a code block to present each person to screen:

{% highlight ruby %}
mary.staff.each do |p|
	puts "Mary is the manager for #{p}"
end
{% endhighlight %}

That's our first code block. `each` will run our code block for every `Person` in mary's `staff` array.

### Another level

If we introduce 'bill' as the manager of the company:

{% highlight ruby %}
bill = Person.new('Bill', 'Managing Director', [mary])
{% endhighlight %}

We can use `each` to look at Bill's staff, which is just Mary at this stage. More interestingly, we could implement our own function on the `Person` class that shows all of that person's descendants. 

{% highlight ruby %}
def descendants

	@staff.each do |s|
		yield s

		# recurse 
		s.descendants do |q|
			yield q
		end

	end

end
{% endhighlight %}

We're going to call any code block specified to our `descendants` function for each of the staff that are managed by this `Person` object, but we're also going to call each descendant's `descendants` function so that we recurse down the tree.

We could augment this call slightly to also include the manager of the descendants:

{% highlight ruby %}
def descendants

	@staff.each do |s|
		yield s

		# recurse 
		s.descendants do |q|
			yield q, s
		end

	end

end
{% endhighlight %}

This will supply a manager to the calling block, 1 level down from where we specify.

{% highlight ruby %}
bill.descendants do |p, s|
	if s == nil
		puts "#{p} is managed by #{bill}"
	else
		puts "#{p} is managed by #{s}"
	end
end
{% endhighlight %}

This code here emits the following:

{% highlight text %}
Mary (Technology Director)  is managed by Bill (Managing Director) 
Bob (Support Officer)  is managed by Mary (Technology Director) 
Joe (Support Officer)  is managed by Mary (Technology Director) 
{% endhighlight %}

Mary's manager variable in the block comes through as `nil` as she's a direct descendant of `bill`, so we handle this case in the block as opposed to in `descendants`. 

You can specify as many parameters as you want in your `yield`. It's your block's responsibility to do something useful with them!

### Other options

Within your function, you can test the `block_given?` property for a boolean that will determine if the calling code did or didn't specify a block. 

You can also have a parameter specified in your function `&block` which can be handy should you need to pass the block around.

