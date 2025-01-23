---
layout: post
title: Writing a Custom Loss Function for a Neural Network
date: 2025-01-24
comments: false
categories: [ "python", "loss", "neural" ]
---

# Introdution

[Loss functions](https://en.wikipedia.org/wiki/Loss_function) are the unsung heroes of machine learning. They guide the 
learning process by quantifying the difference between the predicted and actual outputs. While frameworks like 
[PyTorch](https://pytorch.org/) and [TensorFlow](https://www.tensorflow.org/) offer a plethora of standard loss 
functions such as [Cross-Entropy](https://en.wikipedia.org/wiki/Cross-entropy) and 
[Mean Squared Error](https://en.wikipedia.org/wiki/Mean_squared_error), there are times when a custom loss function 
is necessary.

In this post, we'll explore the why and how of custom loss functions by:

1. Setting up a simple neural network.
2. Using standard loss functions to train the model.
3. Introducing and implementing custom loss functions tailored to specific needs.

# Pre-reqs

Before we begin, you'll need to setup a python project and install some dependencies. We'll be using PyTorch and 
torchvision. To install these dependencies, use the following command:

{% highlight shell %}
pip install torch torchvision
{% endhighlight %}

Once installed, verify the installation by running:

{% highlight shell %}
python -c "import torch; print(torch.__version__)"
{% endhighlight %}

# Network Setup

Let’s start by creating a simple neural network to classify data. For simplicity, we'll use a toy dataset like the 
[MNIST digits](https://www.kaggle.com/datasets/hojjatk/mnist-dataset) dataset.

## Dataet preparation

* Use the MNIST dataset (handwritten digits) as an example.
* Normalize the dataset for faster convergence during training.

{% highlight python %}
import torch
import torch.optim as optim
from torchvision import datasets, transforms

# Data preparation
transform = transforms.Compose([transforms.ToTensor(), transforms.Normalize((0.5,), (0.5,))])
train_data = datasets.MNIST(root='./data', train=True, transform=transform, download=True)
train_loader = torch.utils.data.DataLoader(train_data, batch_size=64, shuffle=True)
{% endhighlight %}

## Model Architecture

* Input layer flattens the 28x28 pixel images into a single vector.
* Two hidden layers with 128 and 64 neurons, each followed by a ReLU `activation`.
* An output layer with 10 neurons (one for each digit) and no activation (handled by the loss function).

{% highlight python %}
# Simple Neural Network
import torch.nn as nn

class SimpleNN(nn.Module):
    def __init__(self):
        super(SimpleNN, self).__init__()
        self.fc1 = nn.Linear(28 * 28, 128)
        self.fc2 = nn.Linear(128, 64)
        self.fc3 = nn.Linear(64, 10)

    def forward(self, x):
        x = x.view(x.size(0), -1)  # Flatten the input
        x = torch.relu(self.fc1(x))
        x = torch.relu(self.fc2(x))
        x = self.fc3(x)
        return x
{% endhighlight %}

## Training Setup:

* Use an optimizer (e.g., Adam) and CrossEntropyLoss for training.
* Loop over the dataset for a fixed number of epochs, computing loss and updating weights.

{% highlight python %}
# Initialize model, optimizer, and device
model = SimpleNN()
optimizer = optim.Adam(model.parameters(), lr=0.001)
device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
model.to(device)
{% endhighlight %}

# Standard Loss

Let’s train the model using the standard Cross-Entropy Loss, which is suitable for classification tasks.

* Combines `log_softmax` and `negative log likelihood` into one step.
* Suitable for classification tasks as it penalizes incorrect predictions heavily.

{% highlight python %}
# Standard loss function
criterion = nn.CrossEntropyLoss()

# Training loop
def train_model(model, train_loader, criterion, optimizer, epochs=5):
    model.train()
    for epoch in range(epochs):
        total_loss = 0
        for images, labels in train_loader:
            images, labels = images.to(device), labels.to(device)

            # Forward pass
            outputs = model(images)
            loss = criterion(outputs, labels)

            # Backward pass and optimization
            optimizer.zero_grad()
            loss.backward()
            optimizer.step()

            total_loss += loss.item()

        print(f'Epoch {epoch+1}/{epochs}, Loss: {total_loss/len(train_loader):.4f}')

train_model(model, train_loader, criterion, optimizer)
{% endhighlight %}

The output of this training session should look something like this:

{% highlight text %}
Epoch 1/5, Loss: 0.3932
Epoch 2/5, Loss: 0.1834
Epoch 3/5, Loss: 0.1352
Epoch 4/5, Loss: 0.1054
Epoch 5/5, Loss: 0.0914
{% endhighlight %}

# Custom Loss

## Why Custom Loss Functions?

Standard loss functions may not work well in cases like:

* **Imbalanced Datasets**: Classes have significantly different frequencies.
* **Multi-Task Learning**: Different tasks require different weights.
* **Task-Specific Goals**: Optimizing for metrics like precision or recall rather than accuracy.

## Example: Weighted Loss

Suppose we want to penalize misclassifying certain classes more heavily. We can achieve this by implementing a 
weighted Cross-Entropy Loss.

{% highlight python %}
# Custom weighted loss function
class WeightedCrossEntropyLoss(nn.Module):
    def __init__(self, class_weights):
        super(WeightedCrossEntropyLoss, self).__init__()
        self.class_weights = torch.tensor(class_weights).to(device)

    def forward(self, outputs, targets):
        log_probs = torch.log_softmax(outputs, dim=1)
        loss = -torch.sum(self.class_weights[targets] * log_probs[range(len(targets)), targets]) / len(targets)
        return loss

# Example: Higher weight for class 0
class_weights = [2.0 if i == 0 else 1.0 for i in range(10)]
custom_criterion = WeightedCrossEntropyLoss(class_weights)

# Training with custom loss function
train_model(model, train_loader, custom_criterion, optimizer)
{% endhighlight %}

After running this, you should see output like the following:

{% highlight text %}
Epoch 1/5, Loss: 0.4222
Epoch 2/5, Loss: 0.1970
Epoch 3/5, Loss: 0.1390
Epoch 4/5, Loss: 0.1124
Epoch 5/5, Loss: 0.0976
{% endhighlight %}

## Example: Combining Losses

Sometimes, you might want to combine multiple objectives into a single loss function.

{% highlight python %}
# Custom loss combining Cross-Entropy and L1 regularization
class CombinedLoss(nn.Module):
    def __init__(self, alpha=0.1):
        super(CombinedLoss, self).__init__()
        self.ce_loss = nn.CrossEntropyLoss()
        self.alpha = alpha

    def forward(self, outputs, targets, model):
        ce_loss = self.ce_loss(outputs, targets)
        l1_loss = sum(torch.sum(torch.abs(param)) for param in model.parameters())
        return ce_loss + self.alpha * l1_loss

custom_criterion = CombinedLoss(alpha=0.01)

# Training with combined loss
train_model(model, train_loader, lambda outputs, targets: custom_criterion(outputs, targets, model), optimizer)
{% endhighlight %}

# Comparing Results

To compare the results of standard and custom loss functions, you need to evaluate the following:

1. Training Loss:
   * Plot the loss per epoch for both standard and custom loss functions.
   
2. Accuracy:
   * Measure training and validation accuracy after each epoch.
   * Compare how well the model performs in predicting each class.
   
3. Precision and Recall:
   * Useful for imbalanced datasets to measure performance on minority classes.
   
4. Visualization:
   * Confusion matrix: Visualize how often each class is misclassified.
   * Loss curve: Show convergence speed and stability for different loss functions.

We can use graphs to visualise how these metrics perform:

{% highlight python %}
from sklearn.metrics import classification_report, confusion_matrix
import matplotlib.pyplot as plt
import numpy as np

# After training
model.eval()
all_preds, all_labels = [], []
with torch.no_grad():
    for images, labels in train_loader:
        images, labels = images.to(device), labels.to(device)
        outputs = model(images)
        preds = torch.argmax(outputs, dim=1)
        all_preds.extend(preds.cpu().numpy())
        all_labels.extend(labels.cpu().numpy())

# Confusion Matrix
cm = confusion_matrix(all_labels, all_preds)
plt.imshow(cm, cmap='Blues')
plt.title('Confusion Matrix')
plt.colorbar()
plt.show()

# Classification Report
print(classification_report(all_labels, all_preds))
{% endhighlight %}

We can also produce visualisations of our loss curves:

{% highlight python %}
# Assuming loss values are stored during training
plt.plot(range(len(train_losses)), train_losses, label="Standard Loss")
plt.plot(range(len(custom_losses)), custom_losses, label="Custom Loss")
plt.xlabel('Epoch')
plt.ylabel('Loss')
plt.legend()
plt.title('Loss Curve')
plt.show()
{% endhighlight %}

# Conclusion

Custom loss functions empower you to fine-tune your neural networks for unique problems. By carefully designing and 
experimenting with loss functions, you can align your model’s learning process with the specific goals of your 
application.

Some closing tips for custom loss functions:

* Always start with a simple baseline (e.g., Cross-Entropy Loss) to understand your model's behavior.
* Visualize performance across metrics, especially when using weighted or multi-objective losses.
* Experiment with different weights and loss combinations to find the optimal setup for your task.

The key is to balance complexity and interpretability—sometimes, even simple tweaks can significantly impact 
performance.
