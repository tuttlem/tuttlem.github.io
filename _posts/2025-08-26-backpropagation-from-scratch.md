---
layout: post
title: Backpropagation from Scratch
description: "A ground-up NumPy implementation of backpropagation solving XOR with neural networks"
date: 2025-08-26
comments: false
categories: [ ml, deep, learning, python, xor ]
---

# Introduction

One of the most powerful ideas behind deep learning is backpropagation—the algorithm that lets a neural network learn 
from its mistakes. But while modern tools like [PyTorch](https://pytorch.org/) and [TensorFlow](https://www.tensorflow.org/) 
make it easy to *use* backprop, they also hide the magic.

In this post, we’ll strip things down to the fundamentals and implement a neural network from scratch in [NumPy](https://numpy.org/) 
to solve the XOR problem. 

Along the way, we’ll dig into what backprop really is, how it works, and why it matters.

## What Is Backpropagation?

Backpropagation is a method for computing how to adjust the **weights**—the tunable parameters of a neural network—so 
that it improves its predictions. It does this by minimizing a **loss function**, which measures how far off the 
network's outputs are from the correct answers. To do that, it calculates **gradients**, which tell us how much each 
weight contributes to the overall error and how to adjust it to reduce that error.

Think of it like this:

- In calculus, we use **derivatives** to understand how one variable changes with respect to another.
- In neural networks, we want to know: *How much does this weight affect the final error?*
- Enter the **chain rule**—a calculus technique that lets us break down complex derivatives into manageable parts.

### The Chain Rule

Mathematically, if 

$$ 
z = f(g(x)) 
$$

then:

$$
\frac{dz}{dx} = \frac{df}{dg} \cdot \frac{dg}{dx}
$$

Backpropagation applies the chain rule across all the layers in a network, allowing us to efficiently compute the 
gradient of the loss function for *every* weight.

### Neural Network Flow

<div class="mermaid">
graph TD
    A[Input Layer] --> B[Hidden Layer]
    B --> C[Output Layer]
    C --> D[Loss Function]
    D -->|Backpropagate| C
    C -->|Backpropagate| B
    B -->|Backpropagate| A
</div>

We push inputs forward through the network to get predictions (**forward pass**), then pull error gradients backward to 
adjust the weights (**backward pass**).

# Solving XOR with a Neural Network

The XOR problem is a classic test for neural networks. It looks like this:

| Input      | Output |
|------------|--------|
| [0, 0]     | 0      |
| [0, 1]     | 1      |
| [1, 0]     | 1      |
| [1, 1]     | 0      |

A simple linear model can't solve XOR because it's not linearly separable. But with a small neural network—just one 
hidden layer—we can crack it.

We'll walk through our implementation step by step.

## Activation Functions

{% highlight python %}
import numpy as np

def sigmoid(x):
    return 1 / (1 + np.exp(-x))

def sigmoid_derivative(x):
    return x * (1 - x)

def mse_loss(y_true, y_pred):
    return np.mean((y_true - y_pred) ** 2)
{% endhighlight %}

We're using the sigmoid function for both hidden and output layers. 

The sigmoid activation function is defined as:

$$
\sigma(x) = \frac{1}{1 + e^{-x}}
$$

Its smooth curve is perfect for computing gradients.

Its derivative, used during backpropagation, is:

$$
\sigma'(x) = \sigma(x) \cdot (1 - \sigma(x))
$$

The `mse_loss` function computes the **mean squared error** between the network’s predictions and the known correct 
values (`y`).

Mathematically, the mean squared error is given by:

$$
\text{MSE}(y, \hat{y}) = \frac{1}{n} \sum_{i=1}^{n} (y_i - \hat{y}_i)^2
$$

Where:
- $$ y_i $$ is the actual target value (`y_true`),
- $$ \hat{y}_i $$ is the network’s predicted output (`y_pred`),
- $$ n $$ is the number of training samples.


## Data and Network Setup

{% highlight python %}
X = np.array([
    [0, 0],
    [0, 1],
    [1, 0],
    [1, 1]
])

y = np.array([
    [0],
    [1],
    [1],
    [0]
])
{% endhighlight %}

The `x` matrix defines all of our inputs. You can see these as the bit pairs that you'd normally pass through an `xor`
operation. The `y` matrix then defines the "well known" outputs.

{% highlight python %}
np.random.seed(42)
input_size = 2
hidden_size = 2
output_size = 1
learning_rate = 0.1
{% endhighlight %}

The `input_size` is the number of input features. We have two values going in as an input here.

The `hidden_size` is the number of "neurons" in the **hidden layer**. Hidden layers are where the network transforms 
input into internal features. XOR requires non-linear transformation, so at least one hidden layer is essential. Setting 
this to `2` keeps the network small, but expressive enough to learn XOR.

`output_size` is the number of output neurons. XOR is a binary classification problem so we only need a single output.

Finally, `learning_rate` controls how fast the network learns. This value **scales** the size of the weight updates 
during training. By increasing this value, we get the network to learn faster but we risk overshooting optimal values. 
Lower values are safer, but slower.

{% highlight python %}
W1 = np.random.randn(input_size, hidden_size)
b1 = np.zeros((1, hidden_size))
W2 = np.random.randn(hidden_size, output_size)
b2 = np.zeros((1, output_size))
{% endhighlight %}

We initialize weights randomly and biases to zero. The small network has two hidden units.

## Training Loop 

We run a "forward pass" and a "backward pass" many times (we refer to these as `epochs`).

### Forward pass

The forward pass takes the input `X`, feeds it through the network layer by layer, and computes the output `a2`. Then it 
calculates how far off the prediction is using a loss function.

{% highlight python %}
# Forward pass
z1 = np.dot(X, W1) + b1
a1 = sigmoid(z1)

z2 = np.dot(a1, W2) + b2
a2 = sigmoid(z2)

loss = mse_loss(y, a2)
{% endhighlight %}

In this step, we are calculating the **loss** for the current set of weights. 

This loss is a measure of how "wrong" the network is, and it’s what drives the learning process in the backward pass.

### Backward pass

The backward pass is how the network learns—by adjusting the weights based on how much they contributed to the final 
error. This is done by applying the **chain rule** in reverse across the network.

{% highlight python %}
# Step 1: Derivative of loss with respect to output (a2)
d_loss_a2 = 2 * (a2 - y) / y.size
{% endhighlight %}

This computes the gradient of the **mean squared error loss** with respect to the output. It answers: *How much does a 
small change in the output affect the loss?*

$$
\frac{\partial \text{Loss}}{\partial \hat{y}} = \frac{2}{n} (\hat{y} - y)
$$

{% highlight python %}
# Step 2: Derivative of sigmoid at output layer
d_a2_z2 = sigmoid_derivative(a2)
d_z2 = d_loss_a2 * d_a2_z2
{% endhighlight %}

Now we apply the chain rule. Since the output passed through a sigmoid function, we compute the derivative of the 
sigmoid to see how a change in the pre-activation $$ z_2 $$ affects the output.

{% highlight python %}
# Step 3: Gradients for W2 and b2
d_W2 = np.dot(a1.T, d_z2)
d_b2 = np.sum(d_z2, axis=0, keepdims=True)
{% endhighlight %}

- `a1.T` is the **transposed output from the hidden layer**.
- `d_z2` is the error signal coming back from the output.
- The dot product calculates how much each **weight in W2** contributed to the error.
- The bias gradient is simply the sum across all samples.

{% highlight python %}
# Step 4: Propagate error back to hidden layer
d_a1 = np.dot(d_z2, W2.T)
d_z1 = d_a1 * sigmoid_derivative(a1)
{% endhighlight %}

Now we move the error back to the hidden layer:

- `d_a1` is the effect of the output error on the hidden layer output.
- We multiply by the derivative of the hidden layer activation to get the **true gradient of the hidden pre-activations**.

{% highlight python %}
# Step 5: Gradients for W1 and b1
d_W1 = np.dot(X.T, d_z1)
d_b1 = np.sum(d_z1, axis=0, keepdims=True)
{% endhighlight %}

- `X.T` is the input data, transposed.
- We compute how each input feature contributed to the hidden layer error.

This entire sequence completes **one application of backpropagation**—moving from output to hidden to input layer, 
using the chain rule and computing gradients at each step.

The final gradients (`d_W1`, `d_W2`, `d_b1`, `d_b2`) are then used in the weight update step:

{% highlight python %}
# Apply the gradients to update the weights
W2 -= learning_rate * d_W2
b2 -= learning_rate * d_b2
W1 -= learning_rate * d_W1
b1 -= learning_rate * d_b1
{% endhighlight %}

This updates the model just a little bit—nudging the weights toward values that reduce the overall loss.

# Final Predictions

{% highlight python %}
print("\nFinal predictions:")
print(a2)
{% endhighlight %}

When we ran this code, we saw:

{% highlight text %}
Epoch 0, Loss: 0.2558
...
Epoch 9000, Loss: 0.1438

Final predictions:
[[0.1241]
[0.4808]
[0.8914]
[0.5080]]
{% endhighlight %}

## Interpreting the Results

The network is getting better, but not perfect. Let’s look at what these predictions mean:

| Input      | Expected | Predicted | Interpreted |
|------------|----------|-----------|-------------|
| [0, 0]     | 0        | 0.1241    | 0           |
| [0, 1]     | 1        | 0.4808    | ~0.5       |
| [1, 0]     | 1        | 0.8914    | 1          |
| [1, 1]     | 0        | 0.5080    | ~0.5       |

It’s nailed `[1, 0]` and is close on `[0, 0]`, but it’s *uncertain* about `[0, 1]` and `[1, 1]`. That’s okay—XOR is a 
tough problem when learning from scratch with minimal capacity.

This ambiguity is actually a great teaching point: neural networks don't just "flip a switch" to get things right. 
They learn gradually, and sometimes unevenly, especially when training conditions (like architecture or learning rate) 
are modest.

You can tweak the hidden layer size, activation functions, or even the optimizer to get better results—but the core 
algorithm stays the same: **forward pass**, **loss computation**, **backpropagation**, **weight update**.

# Conclusion

As it stands, this tiny XOR network is a full demonstration of what makes neural networks learn. 

You’ve now seen backpropagation from the inside.

A full version of this program can be [found as a gist](https://gist.github.com/tuttlem/dbb7a351b83ce76325e83360c0059595).