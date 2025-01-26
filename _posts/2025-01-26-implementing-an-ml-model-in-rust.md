---
layout: post
title: Implementing an ML Model in Rust
date: 2025-01-26
comments: false
categories: [ "ml", "rust" ]
---

# Introduction

Rust, known for its performance, memory safety, and low-level control, is gaining traction in domains traditionally 
dominated by Python, such as machine learning (ML). While Python is the go-to for prototyping ML models due to its 
mature ecosystem, Rust shines in scenarios demanding high performance, safety, and seamless system-level integration. 

In this post, we'll explore how to implement logistic regression in Rust and discuss the implications of the model's 
output.

## Why use Rust?

Before diving into code, it’s worth asking: why choose Rust for ML when Python's libraries like TensorFlow and PyTorch 
exist?

Benefits of Rust:

* **Performance**: Rust offers near-C speeds, making it ideal for performance-critical tasks.
* **Memory Safety**: Its ownership model ensures memory safety, preventing bugs like segmentation faults and data races.
* **Integration**: Rust can easily integrate with low-level systems, making it a great choice for embedding ML models into IoT, edge devices, or game engines.
* **Control**: Rust provides fine-grained control over execution, allowing developers to optimize their models at a deeper level.

While Rust's ML ecosystem is still evolving, libraries like `ndarray`, `linfa`, and `smartcore` provide foundational 
tools for implementing machine learning models.

# Logistic Regression

Logistic regression is a simple yet powerful algorithm for binary classification. It predicts whether a data point 
belongs to class `0` or `1` based on a weighted sum of features passed through a sigmoid function.

Below is a Rust implementation of logistic regression using the `ndarray` crate for numerical operations.

{% highlight rust %}
use ndarray::{Array2, Array1};
use ndarray_rand::RandomExt;
use ndarray_rand::rand_distr::Uniform;

fn sigmoid(x: f64) -> f64 {
    1.0 / (1.0 + (-x).exp())
}

fn logistic_regression(X: &Array2<f64>, y: &Array1<f64>, learning_rate: f64, epochs: usize) -> Array1<f64> {
    let (n_samples, n_features) = X.dim();
    let mut weights = Array1::<f64>::random(n_features, Uniform::new(-0.01, 0.01));
    let mut bias = 0.0;

    for _ in 0..epochs {
        let linear_model = X.dot(&weights) + bias;
        let predictions = linear_model.mapv(sigmoid);

        // Compute the error
        let error = &predictions - y;

        // Compute gradients
        let gradient_weights = X.t().dot(&error) / n_samples as f64;
        let gradient_bias = error.sum() / n_samples as f64;

        // Update weights and bias
        weights -= &(learning_rate * gradient_weights);
        bias -= learning_rate * gradient_bias;
    }

    weights
}

fn main() {
    let X = Array2::random((100, 2), Uniform::new(-1.0, 1.0)); // Random features
    let y = Array1::random(100, Uniform::new(0.0, 1.0)).mapv(|v| if v > 0.5 { 1.0 } else { 0.0 }); // Random labels

    let weights = logistic_regression(&X, &y, 0.01, 1000);
    println!("Trained Weights: {:?}", weights);
}
{% endhighlight %}

Key Concepts:

* **Sigmoid Function**: Converts the linear combination of inputs into a value between 0 and 1.
* **Gradient Descent**: Updates weights and bias iteratively to minimize the error between predictions and actual labels.
* **Random Initialization**: Weights start with small random values and are fine-tuned during training.

## Output

When you run the code, you’ll see output similar to this:

{% highlight text %}
Trained Weights: [0.034283492207871635, 0.3083430316223569], shape=[2], strides=[1], layout=CFcf (0xf), const ndim=1
{% endhighlight %}

### What Does This Mean?

1. **Weights**: Each weight corresponds to a feature in your dataset. For example, with 2 input features, the model learns two weights.
    * A positive weight means the feature increases the likelihood of predicting `1`.
    * A negative weight means the feature decreases the likelihood of predicting `1`.

2. **Bias (Optional)**: The bias adjusts the decision boundary and accounts for data not centered at zero. To view the bias, modify the print statement:

{% highlight rust %}
println!("Trained Weights: {:?}, Bias: {}", weights, bias);
{% endhighlight %}

3. **Predictions**: To test the model, use new data points and calculate their predictions:

{% highlight rust %}
let new_data = array![0.5, -0.2];
let linear_combination = new_data.dot(&weights) + bias;
let prediction = sigmoid(linear_combination);
println!("Prediction Probability: {}", prediction);
{% endhighlight %}

Predictions close to `1` indicate class `1`, while predictions close to `0` indicate class `0`.

### Why Does This Matter?

This simple implementation demonstrates the flexibility and control Rust provides for machine learning tasks. While 
Python excels in rapid prototyping, Rust's performance and safety make it ideal for deploying models in production, 
especially in resource-constrained or latency-critical environments.

### When Should You Use Rust for ML?

Rust is a great choice if:

* **Performance** is critical: For example, in real-time systems or embedded devices.
* **Memory safety** is a priority: Rust eliminates common bugs like memory leaks.
* **Integration** with system-level components is needed: Rust can seamlessly work in environments where Python may not be ideal.
* **Custom ML Implementations**: You want more control over how the algorithms are built and optimized.

For research or quick prototyping, Python remains the best choice due to its rich ecosystem and community. However, 
for production-grade systems, Rust’s strengths make it a compelling alternative.

# Conclusion

While Rust’s machine learning ecosystem is still maturing, it’s already capable of handling fundamental ML tasks like 
logistic regression. By combining performance, safety, and control, Rust offers a unique proposition for ML developers 
building high-performance or production-critical applications. 
