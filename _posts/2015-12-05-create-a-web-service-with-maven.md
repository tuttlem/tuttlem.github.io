---
layout: post
title: Create a web service with maven
date: 2015-12-05
comments: false
categories: [ "java", "maven", "web service", "soap", "jax-ws", "tomcat" ]
---

In today's post, I'm going to walk through a simple [SOAP](https://en.wikipedia.org/wiki/SOAP) [web service](https://en.wikipedia.org/wiki/Web_service) creation using [maven](https://maven.apache.org/), [jax-ws](https://en.wikipedia.org/wiki/Java_API_for_XML_Web_Services) for [java](https://www.java.com/en/). The service will be hosted inside of [Apache Tomcat](http://tomcat.apache.org/) once we're up and running.

### Maven

First off, we start the application off with maven.

{% highlight text %}
$ mvn archetype:generate -DgroupId=org.test.ws -DartifaceId=soap-test
{% endhighlight %}

This creates our project structure and puts all of the project dependencies in place. The `pom.xml` that gets generated for us needs a little extra help for a JAX-WS project. We need to:

* Set the packaging to `war`
* Add the `jaxws-rt` dependencies
* Supply a final name

Your `pom.xml` should look something like this:

{% highlight xml %}
<project xmlns="http://maven.apache.org/POM/4.0.0" 
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">

  <modelVersion>4.0.0</modelVersion>

  <groupId>org.test.ws</groupId>
  <artifactId>soap-test</artifactId>
  <version>1.0-SNAPSHOT</version>
  <packaging>war</packaging>

  <name>soap-test</name>
  <url>http://maven.apache.org</url>

  <properties>
    <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
  </properties>

  <dependencies>
    <dependency>
      <groupId>junit</groupId>
      <artifactId>junit</artifactId>
      <version>3.8.1</version>
      <scope>test</scope>
    </dependency>
    <dependency>
      <groupId>com.sun.xml.ws</groupId>
      <artifactId>jaxws-rt</artifactId>
      <version>2.2</version>
    </dependency>    
    <dependency>
      <groupId>com.sun.istack</groupId>
      <artifactId>istack-commons-runtime</artifactId>
      <version>2.22</version>
    </dependency>    
  </dependencies>

  <build>
    <finalName>HelloService</finalName>
  
    <plugins>
        <plugin>
            <groupId>org.apache.maven.plugins</groupId>
            <artifactId>maven-compiler-plugin</artifactId>
            <version>3.1</version>
            <configuration>
                <source>1.6</source>
                <target>1.6</target>
                <encoding>UTF-8</encoding>
            </configuration>
        </plugin>
    </plugins>
    
  </build>

</project>
{% endhighlight %}

The two references that I had to make were the following were one of `jaxws-rt`

{% highlight xml %}
<dependency>
  <groupId>com.sun.xml.ws</groupId>
  <artifactId>jaxws-rt</artifactId>
  <version>2.2</version>
</dependency>    
{% endhighlight %}

and one for `istack-commons-runtime`.

{% highlight xml %}
<dependency>
  <groupId>com.sun.istack</groupId>
  <artifactId>istack-commons-runtime</artifactId>
  <version>2.22</version>
</dependency>    
{% endhighlight %}

### Service implementation

We now write our service implementation. For this purposes of this article will be very simple. I took over the pre-generated `App.java` and renamed it for my purposes to `HelloService.java`.

{% highlight java %}
package org.test.ws;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebService;
import javax.jws.soap.SOAPBinding;

@WebService
@SOAPBinding(style = SOAPBinding.Style.RPC)
public class HelloService 
{
  @WebMethod(operationName = "sayHello")
  public String sayHello(@WebParam(name="guestname") String guestname) {
    if (guestname == null) { return "Hello"; }
    return "Hello " + guestname;
  }
}
{% endhighlight %}

Fairly basic, "hello" type service.

### Endpoints

We instruct the `jaxws` framework that we have a service listening at any particular given endpoint by use of the `sun-jaxws.xml` file. Create this file in `src/main/webapp/WEB-INF`. It should look like this:

{% highlight xml %}
<?xml version="1.0" encoding="UTF-8"?>
<endpoints xmlns="http://java.sun.com/xml/ns/jax-ws/ri/runtime" 
           version="2.0">
  <endpoint name="HelloService" 
            implementation="org.test.ws.HelloService" 
            url-pattern="/helloService" >
  </endpoint>
</endpoints>
{% endhighlight %}

To let Tomcat know from a deployment perspective what this application will handle, we also create a `web.xml` file that will be located in the same directory, `src/main/webapp/WEB-INF`. It looks like this:

{% highlight xml %}
<?xml version="1.0" encoding="UTF-8"?>
<web-app xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
         xmlns="http://java.sun.com/xml/ns/javaee"
         xmlns:web="http://java.sun.com/xml/ns/javaee/web-app_2_5.xsd" 
         xsi:schemaLocation="http://java.sun.com/xml/ns/javaee http://java.sun.com/xml/ns/javaee/web-app_2_5.xsd"
         id="WebApp_ID" 
         version="2.5">
   
  <display-name>jaxwsExample</display-name>
 
  <listener>
    <listener-class>com.sun.xml.ws.transport.http.servlet.WSServletContextListener</listener-class>
  </listener>
  <servlet>
    <servlet-name>helloService</servlet-name>
    <servlet-class>com.sun.xml.ws.transport.http.servlet.WSServlet</servlet-class>
    <load-on-startup>1</load-on-startup>
  </servlet>
  <servlet-mapping>
    <servlet-name>helloService</servlet-name>
    <url-pattern>/helloService</url-pattern>
  </servlet-mapping>
  <session-config>
    <session-timeout>120</session-timeout>
  </session-config>
</web-app>
{% endhighlight %}

### Building and Running

At the console, you can now build this project:

{% highlight bash %}
mvn clean install
{% endhighlight %}

After you have deployed your war file to Tomcat, you service becomes available at [http://localhost:8080/HelloService/helloService](http://localhost:8080/HelloService/helloService), this is if you've deployed locally; that is.

You're offered a WSDL that your clients can use to integrate with your service [http://localhost:8080/HelloService/helloService?WSDL](http://localhost:8080/HelloService/helloService?WSDL).

### Testing

Now that the service is up and running, we really want to test it to make sure it's working. SOAP requests are HTTP POSTS. Sending the following request:

{% highlight text %}
POST http://localhost:8080/HelloService/helloService HTTP/1.1
Accept-Encoding: gzip,deflate
Content-Type: text/xml;charset=UTF-8
SOAPAction: ""
Content-Length: 262
Host: 172.17.42.1:8080
Connection: Keep-Alive
User-Agent: Apache-HttpClient/4.1.1 (java 1.5)
{% endhighlight %}

{% highlight xml %}
<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" 
                  xmlns:ws="http://ws.test.org/">
  <soapenv:Header/>
    <soapenv:Body>
      <ws:sayHello>
        <guestname>Joe</guestname>
      </ws:sayHello>
    </soapenv:Body>
</soapenv:Envelope>
{% endhighlight %}

. . . should get you this response.

{% highlight xml %}
<S:Envelope xmlns:S="http://schemas.xmlsoap.org/soap/envelope/">
  <S:Body>
    <ns2:sayHelloResponse xmlns:ns2="http://ws.test.org/">
      <return>Hello Joe</return>
    </ns2:sayHelloResponse>
  </S:Body>
</S:Envelope>
{% endhighlight %}