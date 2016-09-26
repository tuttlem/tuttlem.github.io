---
layout: post
title: Serializing data with Avro
date: 2016-09-26
comments: false
categories: [ "avro", "data", "serialization" ]
---

In order to marshal data between systems in a language and technology agnostic fashion, you'll need to lean on a serialization system that affords you the flexibility as well as the strong contract definition that a serialization system provides. [Avro](https://avro.apache.org/) is such a system. You declare a set of requirements that your data object must exhibit; run a code-generator over that schema file and Avro will generate objects for you to work with.

In today's post, I'll run through the creation of a schema; generation of a code class and basic usage.

### Setup

First up, we'll need the `avro-tools` jar in order to compile schemas into java class files for us. Not only that, the project that will perform serializations will require the `avro` libraries. Add the following dependency to your `pom.xml` to enable Avro's libraries.

{% highlight xml %}
<dependency>
  <groupId>org.apache.avro</groupId>
  <artifactId>avro</artifactId>
  <version>1.8.1</version>
</dependency>
{% endhighlight %}

### Schema

The schema definition is the key part of this entire process. It's the schema that will be the *understanding* or the *strong contract* between two systems that they'll agree on in order to send information back and forth. Without this, it'd be chaos. No one would know what the true shape of a data packet was. The following has been taken from the [Avro page about schemas](http://avro.apache.org/docs/1.8.1/#schemas):

> Avro relies on schemas. When Avro data is read, the schema used when writing it is always present. This permits each datum to be written with no per-value overheads, making serialization both fast and small. This also facilitates use with dynamic, scripting languages, since data, together with its schema, is fully self-describing.

Lets define a car in Avro schema, which is just [JSON](http://json.org) anyway:

{% highlight json %}

{
  "namespace": "autoshop.avro",
  "type": "record",
  "name": "Car",
  "fields": [
    { "name": "make", "type": "string" },
    { "name": "model", "type": "string" },
    { "name": "year", "type": "int" }
  ]
}

{% endhighlight %}

Now that we've defined a schema, we can use the `avro-tools` jar (which is available on Avro's [releases page](http://avro.apache.org/releases.html)). Armed with our Avro schema file named `car.avsc`, we can now generate our Java classes:

{% highlight shell %}
java -jar tools/avro-tools-1.8.1.jar compile schema car.avsc .
{% endhighlight %}

Take a look at `Car.java` now. Avro has filled this class out for you, ready to use. The generation process also honors the namespace asked for:

{% highlight plain %}
├── autoshop
│   └── avro
│       └── Car.java
{% endhighlight %}

### Instance construction

Now that we've generated a class, we can start constructing instances of it. Out of the box, we're given three different construction flavors:

*Default*

{% highlight java %}
Car car1 = new Car();
car1.setMake("Ferarri");
car1.setModel("F40");
car1.setYear(1992);
{% endhighlight %}

*Parameterized*

{% highlight java %}
Car car2 = new Car("Porsche", "911", 1965);
{% endhighlight %}

*Builder*

{% highlight java %}
Car car3 = Car.newBuilder()
              .setMake("McLaren")
              .setModel("650s")
              .setYear(2014)
              .build();
{% endhighlight %}

### Serialization and Deserialization

Now that we've created our three cars, we can write them into a data file:

{% highlight java %}
/* up above */
import org.apache.avro.file.DataFileWriter;
import org.apache.avro.specific.SpecificDatumWriter;
import java.io.*;

/* . . . */

DataFileWriter<Car> carFileWriter = new DataFileWriter<Car>(
  new SpecificDatumWriter<Car>(Car.class)
);

carFileWriter.create(car1.getSchema(), new File("cars.avro"));
carFileWriter.append(car1);
carFileWriter.append(car2);
carFileWriter.append(car3);
carFileWriter.close();
{% endhighlight %}

All of the cars subscribe to the same schema, so it's ok for all of the instances to use the schema from `car1`. `cars.avro` now holds a serialized representation of our cars. We can read them out again using the following:

{% highlight java %}
/* up above */
import org.apache.avro.file.DataFileReader;
import org.apache.avro.specific.SpecificDatumReader;

/* . . . */

DataFileReader<Car> carFileReader = new DataFileReader<Car>(
  new File("cars.avro"), new SpecificDatumReader<Car>(Car.class)
);
Car car = null;

while (carFileReader.hasNext()) {
  car = carFileReader.next(car);
  System.out.println(car);
}

{% endhighlight %}

The output of which will look something like this:

{% highlight json %}
{"make": "Ferarri", "model": "F40", "year": 1992}
{"make": "Porsche", "model": "911", "year": 1965}
{"make": "McLaren", "model": "650s", "year": 2014}
{% endhighlight %}

### Further Reading

In situations where code generation isn't going to be an option, or when you are generally late-binding; you'll still be able to use a class like `GenericRecord` in order to satisfy records. You'll need to use methods like `put` to set internals of the classes, but you won't need to strongly bind to generated code.

