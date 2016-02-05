---
layout: post
title: Design patterns
date: 2016-02-05
comments: false
categories: [ "java", "patterns" ]
---

Many books have been written in the past on the topic of [software design patterns](https://en.wikipedia.org/wiki/Software_design_pattern). The [Design Patterns](https://en.wikipedia.org/wiki/Design_Patterns) being one of the best. In today's post, I'm going to skim through these patterns really quickly; just offering hint code *(in java)* for them.

## Index

* [Structural patterns](#structural-patterns)
  * [Adapter](#adapter)
  * [Facade](#facade)
  * [Composite](#composite)
  * [Bridge](#bridge)
  * [Proxy](#proxy)
* [Behavioral patterns](#behavioral-patterns)
  * [Observer](#observer)
  * [Mediator](#mediator)
  * [Chain of Responsibility](#chain-of-responsibility)
  * [Flyweight](#flyweight)
  * [Momento](#momento)
  * [Template](#template)
  * [State](#state)
  * [Strategy](#strategy)
  * [Command](#command)
  * [Interpreter](#interpreter)
  * [Decorator](#decorator)
  * [Iterator](#iterator)
* [Creational patterns](#creational-patterns)
  * [Singleton](#singleton)
  * [Builder](#builder)
  * [Factory Method](#factory-method)
  * [Abstract Factory](#abstract-factory)
  * [Prototype](#prototype)

## <a name="structural-patterns"></a>Structural patterns

Structural patterns are used to demonstrate the relationship between entities clearly.

### <a name="adapter"></a>Adapter

**Adapter** is a structural pattern and its purpose is to provide a bridge between two different interfaces to support a common contract. To illustrate, **we** have **our** way of adding two numbers and how we describe the inputs to a function.

{% highlight java %}
public interface MyAdder {

  public int getFirstNumber();
  public int getSecondNumber();

}
{% endhighlight %}

We need to re-shape this for an external (*or uncontrolled*) contract now. **They** have **their** way of adding two numbers.

{% highlight java %}
public interface TheirAdder {

  public int getLeftHandSide();
  public int getRightHandSide();

}
{% endhighlight %}

These each have their own implementations. We'd control `MyAdderImpl`, but we'd only ever see the interface `TheirAdder`; not **their** implementation.

{% highlight java %}
public class MyAdderImpl implements MyAdder {

  public int getFirstNumber();
  public int getSecondNumber();

}
{% endhighlight %}

To make an `Adapter`, we need to create an object that responds to `TheirAdder` but **adapts** to usage of `MyAdder`. To achieve this, we'll use an adapter which just maps one interface to another. Here's an adapter going from `MyAdder` over to `TheirAdder`.

{% highlight java %}
public class MyAdderToTheirAdderAdapter implements TheirAdder {

  private final MyAdder myAdder;

  public MyAdderToTheirAdderAdapter(MyAdder myAdder) {
    this.myAdder = myAdder;
  }

  @Override
  public int getLeftHandSide() {
    return myAdder.getFirstNumber();
  }

  @Override
  public int getRightHandSide() {
    return myAdder.getSecondNumber();
  }

}
{% endhighlight %}

The usage of these classes and interfaces now looks like this:

{% highlight java %}
public static int executeTheirAdder(TheirAdder theirAdder) {
  return theirAdder.getLeftHandSide() + theirAdder.getRightHandSide();
}

public static void main(String[] args) {

  MyAdderImpl adder = new MyAdderImpl(7, 2);
  MyAdderToTheirAdderAdapter adapter = new MyAdderToTheirAdderAdapter(adder);

  int answer = executeTheirAdder(adapter);

  System.out.println("The answer was " + answer);

}
{% endhighlight %}

We're using **out** adder implementation, but integrating with a function that expects **their** adder. So we adapt the values internally using our adapter, `MyAdderToTheirAdderAdapter`.

### <a name="facade"></a>Facade 

**Facade** is a structural pattern that allows a developer to produce a much simpler (or different) interface on top of a much more complex class. The whole idea is about making a cleaner interface against a complex object. The following class details a few of the *things* that you'd do when starting and stopping a car. Note how verbose these operations are:

{% highlight java %}
public class Car {

  public void putClutchIn() { }
  public void turnKeyOn() { }
  public void pumpFuel() { }
  public void releaseHandbreak() { }

  public void applyHandbreak() { }
  public void turnKeyOff() { }

}
{% endhighlight %}

I have already grouped these methods into two. The top set of methods apply to starting the car, the second are about stopping the car. We can create a much simpler interface across this object with a **facade** class:

{% highlight java %}
public class CarOperationFacade {

  private final Car car;

  public CarOperationFacade(Car car) {
    this.car = car;
  }

  public void start() {
    this.car.putClutchIn();
    this.car.turnKeyOn();
    this.car.pumpFuel();
    this.car.releaseHandbreak();
  }

  public void stop() {
    this.car.applyHandbreak();
    this.car.turnKeyOff();
  }

}
{% endhighlight %}

From here, we no longer need to call `purClutchIn` then `turnKeyOn` then `pumpFuel`, etc. to start the car. We create a `CarOperationFacade` instance (passing in our `Car` instance) and the we can use `start` and `stop`.

### <a name="composite"></a>Composite

The **Composite** pattern is about being able to treat different objects in the same manner. This is useful for when groups of object types need to be treated the same at some level. For this example, we're going to create an expression tree for mathematical expressions. Everything in our expression language will be based off a root concept called an `Atom`. All an `Atom` does is emit its value:

{% highlight java %}
public abstract class Atom {
  public double getValue();
}
{% endhighlight %}

We then create other sub classifications from this class like `LiteralValue` which will just represent a number and `Operation` which will allow us to define two `Atoms` concatenated by an operator:

{% highlight java %}
public class LiteralValue extends Atom {

  private final double value;

  public LiteralValue(double value) {
    this.value = value;
  }

  @Override
  public double getValue() { return this.value; }

}

public class Operation extends Atom {

  public final int ADD = 1;
  public final int SUB = 2;
  public final int DIV = 3;
  public final int MUL = 4;

  private final Atom left;
  private final Atom right;
  private final int op;

  public Operation(Atom left, Atom right, int op) {
    this.left = left;
    this.right = right;
  }

  @Override
  public double getValue() {  

    switch (op) {
      case Operation.ADD:
        return this.left.getValue() + this.right.getValue();
      case Operation.SUB:
        return this.left.getValue() - this.right.getValue();
      case Operation.MUL:
        return this.left.getValue() * this.right.getValue();
      case Operation.DIV:
        return this.left.getValue() / this.right.getValue();
    }

  }
}
{% endhighlight %}

`Operation` holding two further `Atom` references where it's an `Atom` reference itself.

### <a name="bridge"></a>Bridge

The **Bridge** pattern is simply dividing out implementation details from the abstraction. If your class tree (hierarchy) has the potential to be too complex, the bridge pattern can assist in keeping your abstraction as implementation-dependency-free as possible.

{% highlight java %}
public abstract class Drawing {
  public void putPixel(int x, int y, int colour);
}

public class GLDrawing extends Drawing {
  @Override
  public void putPixel(int x, int y, int colour) { 
    /* OpenGL code here */
  }
}

public class DirectXDrawing extends Drawing {
  @Override
  public void putPixel(int x, int y, int colour) { 
    /* DirectX code here */
  }
}
{% endhighlight %}

As more drawing libraries came along, you'd be creating more `Drawing` derivatives directly tying the implementation to your abstraction. If we separate this out though, we can free the `Drawing` class of any derivatives.

{% highlight java %}
public abstract class DrawingImpl {
  public void putPixel(int x, int y, int colour);
}

public class Drawing {
  private final DrawingImpl impl;

  public Drawing(DrawingImpl impl) {
    this.impl = impl;
  }

  public void putPixel(int x, int y, int colour) {
    this.impl.putPixel(x, y, colour);
  }
}

public class GLDrawing extends DrawingImpl {
  @Override
  public void putPixel(int x, int y, int colour) { 
    /* OpenGL code here */
  }
}

public class DirectXDrawing extends DrawingImpl {
  @Override
  public void putPixel(int x, int y, int colour) { 
    /* DirectX code here */
  }
}
{% endhighlight %}

### <a name="proxy"></a>Proxy

The **proxy** pattern is a structural pattern that provides a wrapper to access another object. It comes in a few different flavors:

* Remote
* Virtual
* Protection

**Remote** proxies are used when the object is not in the local system-space; like accessible over the network or as a target of inter-process-communication (i.e. in a different JVM). **Virtual** proxies are used to defer work and make initialization more-lazy and finally the **Protection** proxy is about forming a layer that determines the safety of interacting with an object.

## <a name="creational-patterns"></a>Creational patterns

Creational patterns aim to control the construction phase of an object. Constraining this process allows a developer to control the lifetime of their object system effectively.

### <a name="singleton"></a>Singleton

**Singleton** controls the instance count of a class in your application. These classes come in two different flavors: *eager* and *lazy*. An *eager* **singleton** will instance itself as soon as the class's static members come into scope:

{% highlight java %}
public class Logger {
  private static Logger instance = new Logger();

  private Logger() { }

  public static Logger getInstance() {
    return instance;
  }
}
{% endhighlight %}

One of the important pieces of this pattern is that the class's constructor is marked **private**. This prevents integrating code from creating extra instances. This is exactly what this pattern aims to prevent. 

The singleton reference is acquired through the use of `getInstance`.

A *lazy* variant on the class takes the initialization *(construction)* and places it into the singleton acquisition method `getInstance`.

{% highlight java %}
public class TodoList {
  private static TodoList instance = null;

  private TodoList() { }

  public static TodoList getInstance() {
    if (instance == null) {
      instance = new TodoList();
    }

    return instance;
  }
}
{% endhighlight %}

This is handy when your constructor is performing work that you'd prefer to defer until you actually need the class. As a final note, the aim of this pattern is to control the developer's ability to instantiate this class as such you'll need to control any cloning/rehydration/deserialization techniques that developers may be able to use to inadvertently create another instance of your **singleton**.

### <a name="builder"></a>Builder

The **builder** pattern gives the developer some syntactic sugar over the construction process, cleaning up massive constructor signatures.

{% highlight java %}
public class Person {
  private String firstName;
  private String middleNames;
  private String lastName;
  private int age;

  public static class PersonBuilder {

    private String firstName;
    private String middleNames;
    private String lastName;
    private int age;

    public PersonBuilder() { }

    public PersonBuilder firstName(String firstName) {
      this.firstName = firstName;
      return this;
    }

    public PersonBuilder middleNames(String middleNames) {
      this.middleNames = middleNames;
      return this;
    }

    public PersonBuilder lastName(String lastName) {
      this.lastName = lastName;
      return this;
    }

    public PersonBuilder age(int age) {
      this.age = age;
      return this;
    }

    public Person build() {
      return new Person(this);
    }
  }

  private Person(PersonBuilder builder) {
    this.firstName = builder.firstName;
    this.middleNames = builder.middleNames;
    this.lastName = builder.lastName;
    this.age = builder.age;
  }

  @Override
  public String toString() {
    return String.format(
      "First Name: %s\nMiddle Names: %s\nLast Name: %s\nAge: %d", 
      this.firstName,
      this.middleNames,
      this.lastName,
      this.age
    );
  }
}
{% endhighlight %}

You can now use your **builder** like so:

{% highlight java %}
Person person = new Person.PersonBuilder()
  .firstName("John")
  .lastName("Smith")
  .build();
{% endhighlight %}

### <a name="factory-method"></a>Factory Method

The **factory method** is a construction pattern, where a function provides the instancing of classes deriving from a common base.

{% highlight java %}
public class XmlNode { }
public class XmlAttribute extends XmlNode { }
public class XmlElement extends XmlNode { }
public class XmlDocument extends XmlNode { }

public class XmlParser {

  public static XmlNode parse(String xml) {
    /* parse out 'xml' */

    if (isAttribute(xml)) {
      return XmlAttribute(xml);
    } else if (isElement(xml)) {
      return XmlElement(xml);
    }

    /* etc . . */
  }

}
{% endhighlight %}

`parse` in the `XmlParser` class is our **factory method**.

### <a name="abstract-factory"></a>Abstract Factory

Taking the *factory method* a step further, we can abstract out the construction process for objects in the same category.

{% highlight java %}
public interface VehicleFactory {
  public Vehicle create();
}

public class CarFactory implements VehicleFactory {
  public Vehicle create() {
    return new Car();
  }
}

public class TruckFactory implements VehicleFactory {
  public Vehicle create() {
    return new Truck();
  }  
}

public class AssemblyLine {
  private VehicleFactory factory;

  public AssemblyLine(VehicleFactory factory) {
    this.factory = factory;
  }

  public Vehicle getVehicle() {
    return this.factory.create();
  }
}
{% endhighlight %}

We can then start creating `Vehicle` objects and these will depend on the factory:

{% highlight java %}
// create some cars
AssemblyLine assemblyLine = new AssemblyLine(new CarFactory());
Vehicle car = assemblyLine.getVehicle();
{% endhighlight %}

### <a name="prototype"></a>Prototype

The **prototype** pattern is a creational pattern that focuses on lowering the construction overhead. In order to achieve this in Java, the pattern will lean on java's `Cloneable` interface.

{% highlight java %}
public interface MembershipPrototype extends Cloneable {
  public MembershipPrototype clone() throws CloneNotSupportedException;
}

public class Membership implements MembershipPrototype {
  @Override 
  public Membership clone() {
    return (Membership)super.clone();
  }
}

public class MembershipProvider {
  private static Map<String, Membership> map = new HashMap<String, Membership>();

  static {
    /* TODO: build membership map in this block */
  }

  public static Membership getMembership(String key) {
    Membership m = map.get(key);
    if (m != null) {
      return m.clone();
    }

    return null;
  }
}
{% endhighlight %}

The expensive load part is put in the `static` block above, only executed once. From there, objects are cloned into the sytstem.  

## <a name="behavioral-patterns"></a>Behavioral patterns

Behavioral patterns focus on the relationships between objects and how they communicate with each other.

### <a name="observer"></a>Observer

The **observer** pattern is a behavioral pattern that provides a class the ability to publish and subscribe *(pub/sub)* state changes and messages.

Java already has the **observer** pattern baked in with `Observer` and `Observable` members of the `java.util` namespace. In this example, a cricket game is being played. Two batsmen are being observed by the scoreboard. So we define our batsmen with names and run totals; they also have the ability to score runs:

{% highlight java %}
public static class Batsman extends Observable {
  private final String name;
  private int currentRuns = 0;

  public Batsman(String name) {
    this.name = name;
  }

  public String getName() { return name; }
  public int getRuns() { return currentRuns; }

  public void scoreRuns(int runs) {
    this.currentRuns += runs;

    this.setChanged();
    this.notifyObservers(runs);
  }
}
{% endhighlight %}

There would be heaps of observers, but in this case it's going to be the scoreboard that observes the batsmen. Here's our scoreboard:

{% highlight java %}
public static class Scoreboard implements Observer {
  @Override
  public void update(Observable obs, Object x) {
    Batsman b = (Batsman)obs;
    System.out.println(String.format("%s just scored %d runs and is currently on %d", b.getName(), x, b.getRuns()));
  }  
}
{% endhighlight %}

Each time that a batsmen scores runs with `scoreRuns`, the scoreboard gets notified; we at least it does if we use `addObserver`:

{% highlight java %}
public static void main(String[] args) {
  Scoreboard scoreboard = new Scoreboard();
  Batsman batsman1 = new Batsman("A. Batsman");
  Batsman batsman2 = new Batsman("B. Batsman");

  batsman1.addObserver(scoreboard);
  batsman2.addObserver(scoreboard);

  batsman1.scoreRuns(4);
  batsman1.scoreRuns(1);
  batsman2.scoreRuns(2);
  batsman2.scoreRuns(1);
  batsman1.scoreRuns(6);
  batsman1.scoreRuns(6);
  batsman1.scoreRuns(1);
}
{% endhighlight %}

### <a name="mediator"></a>Mediator

You can use the **mediator** pattern to loosen the coupling of your objects. It's the job of the **mediator** pattern to define how a set of objects interact. 

When you have many objects performing operations on each other, the direct dependency is removed by abstracting your operations out into an interface. The interface is your mediator that satisfies all of the *now* disparate parts of your object system.

In this example, the program is managing a baseball game. The following interface denotes all of the actions that can go on in our game. 

{% highlight java %}
public interface BaseballMediator {
  // pitcher->ball
  public void pitchBall();

  // batter->ball
  public void swingAtBall();

  // fielder->ball
  public void fieldBall();
  public void throwBall();

  // ball->field
  public void foulBall();
  public void hitBall();
  public void homeRunBall();

  // field->batter
  public void adjustScore();
}
{% endhighlight %}

In order for an object to participate within this system, we need to allow it to set a mediator:

{% highlight java %}
public interface BaseballParticipant {
  public void setMediator(BaseballMediator mediator);
}
{% endhighlight %}

All of our system objects now become `BaseballParcipant` derivatives. They, as usual, hold their actions that they manage (batter hits a ball, fielder catches a ball, etc.) but the implementation of these actions enforces its side-effects through the usage of the `BaseballMediator` instance that gets set.

{% highlight java %}
public class Pitcher implements BaseballParticipant {
  private BaseballMediator mediator = null;

  @Override
  public void setMediator(BaseballMediator mediator) {
    this.mediator = mediator;
  }

  public void pitch() {
    this.mediator.pitchBall();
  }
}

public class Ball implements BaseballParticipant {
  private int vx, vy;
  private BaseballMediator mediator = null;

  @Override
  public void setMediator(BaseballMediator mediator) {
    this.mediator = mediator;
  }

  public void setVelocity(int x, int y) {
    vx = x;
    vy = y;
  }    
}

public class Batter implements BaseballParticipant { /* etc */ }
public class Fielder implements BaseballParticipant { /* etc */ }
public class Field implements BaseballParticipant { /* etc */ }
{% endhighlight %}

Finally, the **mediator** gets defined which marshals the interactions between our system objects:

{% highlight java %}
public class BaseballMediatorImpl implements BaseballMediator {

  private final Ball ball;
  private final Pitcher pitcher;
  private final Fielder fielder;
  private final Batter batter;
  private final Field field;

  public BaseballMediatorImpl(Ball ball, Pitcher pitcher, Fielder fielder, Batter batter, Field field) {
    this.ball = ball;
    this.pitcher = pitcher;
    this.fielder = fielder;
    this.batter = batter;
    this.field = field;
  }

  // pitcher->ball
  public void pitchBall() {
    this.pitcher.setAction("pitching");
    this.ball.setVelocity(-1, 0);
  }

  // batter->ball
  public void swingAtBall() {
    this.batter.setAction("swinging");
  }

  public void fieldBall() {  /* etc */ }
  public void throwBall() {  /* etc */ }
  public void foulBall() {  /* etc */ }
  public void hitBall() { /* etc */ }
  public void homeRunBall() { /* etc */ }
  public void adjustScore() { /* etc */ }

}
{% endhighlight %}

### <a name="chain-of-responsibility"></a>Chain of Responsibility

The *chain of responsibility pattern* serves as a request forwarder. A request that you supply to the chain moves through the chain itself until the contents of the request match the implementation. If you needed to perform some processing on a `Executive`, `Manager`, `Supervisor` or `Employee` you would start by creating your processing contract:

{% highlight java %}
public interface StaffHandler {
  public void setHandler(StaffHandler handler);
  public void process(Job job);
}
{% endhighlight %}

The important part of this contract really is the `setHandler`. It's going to to simulate our chain for us:

{% highlight java %}
public class Executive implements StaffHandler {
  private StaffHandler handler;

  @Override
  public void setHandler(StaffHandler handler) {
    this.handler = handler;
  }

  @Override
  public void process(Job job) {
    if (job.getLevel() > 1000) {
      /* perform "Executive implementation" here */
    } else if (this.handler != null) {
      /* move down the chain */
      handler.process(job);
    } else {
      System.out.println("Job type not supported");
    }
  }

}
{% endhighlight %}

`Manager`, `Supervisor` and `Employee` implementations would look very similar to `Executive`.

Building the chain now looks like this:

{% highlight java %}
StaffHandler executive = new Executive();
StaffHandler supervisor = new Supervisor();
StaffHandler manager = new Manager();
StaffHandler employee = new Employee();

executive.setHandler(supervisor);
supervisor.setHandler(manager);
manager.setHandler(employee);

executive.process(new Job());
{% endhighlight %}

### <a name="flyweight"></a>Flyweight

The **flyweight** pattern takes the construction of objects and caches instances to save on the construction process. Really useful when you have a large volume of objects that are quite similar.

{% highlight java %}
public final class StaffFactory {
  private static Map<String, Staff> map = new HashMap<String, Staff>();

  public static synchronized Staff getEmployee(String employeeType) {
    Staff staff = map.get(employeeType);

    if (staff == null) {
      /* construct the staff member */

      map.put(employeeType, staff);
    }

    return staff;
  }
}
{% endhighlight %}


### <a name="momento"></a>Momento

The **momento** pattern is all about taking snapshots of an object so that you can provide `undo` functionality to your class.

{% highlight java %}
public class BallState {
  public double x;
  public double y;
  public double vx;
  public double vy;
}

public class Ball {
  public double x;
  public double y;
  public double vx;
  public double vy;

  private BallStateManager stateManager;

  public void checkpoint(String key) {
    stateManager.save(new BallState(x, y, vx, vy), key);
  }

  public void undo(String key) {
    BallState state = stateManager.retrieve(key);

    this.x = state.x;
    this.y = state.y;
    this.vx = state.vx;
    this.vy = state.vy;
  }
}

public class BallStateManager {
  private final Map<String, BallState> storage = new HashMap<String, BallState>();

  public void save(BallState state, String key) {
    storage.put(key, state);
  }

  public BallState retrieve(String key) {
    return storage.get(key);
  }
}
{% endhighlight %}

### <a name="template"></a>Template

The **template** pattern is about putting a sequence of instructions that developers can hook to:

{% highlight java %}
import java.util.ArrayList;
import java.util.List;

public class TreeNode<T> {

  private List<TreeNode<T>> children;
  private T value;
  
  public TreeNode(T v) {
    this.value = v;
    this.children = new ArrayList<TreeNode<T>>();
  }
  
  public T getValue() { return value; }
  public List<TreeNode<T>> getChildren() { return children; }

  public void addChild(T child) {
    children.add(new TreeNode<T>(child));
  }
  
  public String getLeafText() { return value.toString(); }

  @Override
  public String toString() {
    
    String out = String.format("%s - ", this.getLeafText());
    
    for (TreeNode<T> node : children) {
      out += node.toString();
    }
    
    return out;
  }
}
{% endhighlight %}

In this example, `toString` traverses a tree but offers the developer `getLeafText` to control the leaf's representation in the string.

### <a name="state"></a>State

The **state** pattern will allow you to explicitly manage state changes in your objects by defining the equivalent of a cartesian map of functionality. The `CarState` interface in the following example lays out all of the actions that our objects will perform.

{% highlight java %}
public interface CarState {
  public void turnOn();
  public void drive();
  public void stop();
  public void turnOff();
}
{% endhighlight %}

For each state that we define, we now need a concrete implementation of the `CarState` interface:

{% highlight java %}
public class CarStateTurnOn implements CarState {
  private final Car car;

  public CarStateTurnOn(Car car) {
    this.car = car;
  }

  @Override
  public void turnOn() { 
    System.out.println("Car is already turned on");
  }

  @Override
  public void drive() {
    System.out.println("Driving . . .");
    this.car.changeState(this.car.getDriveCarState());
  }

  @Override
  public void stop() {
    System.out.println("Stopping . . .");
    this.car.changeState(this.car.getStopCarState());
  }

  @Override
  public void turnOff() {
    System.out.println("Turn off . . .");
    this.car.changeState(this.car.getTurnOffCarState());
  }
}
{% endhighlight %}

Our `Car` implementation itself will use this state interface to define its structure, but we'll also define a variable (of type `CarState`) for every state that the car can be in.

{% highlight java %}
public class Car implements CarState {
  private CarState carTurnOn;
  private CarState carDrive;
  private CarState carStop;
  private CarState carTurnOff;

  private CarState current;

  public Car() {
    this.carTurnOn  = new CarStateTurnOn(this);
    this.carDrive   = new CarStateDrive(this);
    this.carStop    = new CarStateStop(this);
    this.carTurnOff = new CarStateTurnOff(this);
  
    this.current = this.carTurnOff;
  }

  public CarState getTurnOnCarState() { return this.carTurnOn; }
  public CarState getDriveCarState() { return this.carDrive; }
  public CarState getStopCarState() { return this.carStop; }
  public CarState getTurnOffCarState() { return this.carTurnOff; }

  private void changeState(CarState newState) {
    this.current = newState;
  }

  @Override 
  public void turnOn() {
    // from state x to state y
    this.current.turnOn();
    // reflect state change internally
    changeState(this.carTurnOn);
  }

  @Override 
  public void drive() {
    // from state x to state y
    this.current.drive();
    // reflect state change internally
    changeState(this.carDrive);
  }

  @Override 
  public void stop() {
    // from state x to state y
    this.current.stop();
    // reflect state change internally
    changeState(this.carStop);
  }

  @Override 
  public void turnOff() {
    // from state x to state y
    this.current.turnOff();
    // reflect state change internally
    changeState(this.carTurnOff);
  }

}
{% endhighlight %}

### <a name="strategy"></a>Strategy

The **strategy** design pattern is more in the name than anything else. From a common base, you'll create derivatives that a *executioner* will use to perform an action.

{% highlight java %}
public abstract class Interpolator {
  private double index;
  
  public double getIndex() { return index; }
  
  public abstract double getValue();
}

public class LinearInterpolator extends Interpolator {
  public double getValue() {
    return getIndex();
  }
}

public class TrigonometricInterpolator extends Interpolator {
  public double getValue() {
    return Math.sin(this.getIndex() * (2 * Math.PI));
  }
}
{% endhighlight %}

Being able to interpolate values from 0 up to 1 by means of linear or trigonometric methods is an implementation detail for the **strategy** pattern.

### <a name="command"></a>Command

The **command** design pattern is all about dispatching messages (or instructions) from an invoker to a receiver. In sorts, you can think of it a message pump:

{% highlight java %}
public interface DataJob {
  public void execute();
}

public class CreatePerson implements DataJob {

  @Override
  public void execute() {
    // TODO: database code here to create a person
  }
  
}

public class DeletePerson implements DataJob {
  
  @Override
  public void execute() {
    // TODO: database code here to delete a person
  }
  
}

public class ActionManager {
  public List<DataJob> jobs = new ArrayList<DataJob>();
  
  public void run() {
    // TODO: process the job queue, calling "execute"
    // on items fetched
  }
}
{% endhighlight %}

### <a name="interpreter"></a>Interpreter

The **interpreter** pattern is about making an interpreted languages (and resulting execution environments). There are a lot of different tools already available to take language agnostic gammar definitions and output source code that will interpret this.

### <a name="decorator"></a>Decorator

The **decorator** patterns allows a developer to separate commonly described attributes into smaller class implementations. These smaller pieces are the composed together (or the original instance is *decorated*) so that we end up with an object that has more *extras*.

{% highlight java %}
public interface Car {
  public String getDescription();
  public double getPrice();
}

public class Toyota implements Car {
  @Override
  public String getDescription() { return "Toyota"; }

  @Override 
  public double getPrice() { return 19990; }
}

public class Honda implements Car {
  @Override
  public String getDescription() { return "Honda"; }

  @Override
  public double getPrice() { return 23500; }
}

public abstract class CarDecorator implements Car {
  @Override
  public String getDescription() { return "Extras"; }
}

public class Wheel extends CarDecorator {
  private final Car car;

  public Wheel(Car car) {
    this.car = car;
  }

  @Override
  public String getDescription() { return this.car.getDescription() + ", Wheel ($230)"; }

  @Override
  public double getPrice() { return this.car.getPrice() + 230; }
}

public class Paint extends CarDecorator {
  private final Car car;

  public Paint(Car car) {
    this.car = car;
  }

  @Override
  public String getDescription() { return this.car.getDescription() + ", Paint ($190)"; }

  @Override
  public double getPrice() { return this.car.getPrice() + 190; }
}
{% endhighlight %}

The `Car` interface defines what's important to us about the car, where as `CarDecorator` is how our system explains the pieces that we're going to decorate our `Car` with.

`Paint` and `Wheel` are both concrete decorations and can be applied like so:

{% highlight java %}
Car car = new Toyota();
car = new Paint(car);
car = new Wheel(car);
car = new Wheel(car);
car = new Wheel(car);
car = new Wheel(car);
{% endhighlight %}

### <a name="iterator"></a>Iterator

The **iterator** pattern allows us to treat a sequence of objects in a uniform way. The action of traversal is supplied in a common way so that sets can be enumerated in similar ways.

In java, this is achieved using the `Iterator` interface.

