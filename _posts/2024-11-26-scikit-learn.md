---
layout: post
title: scikit-learn
date: 2024-11-26
comments: false
categories: [ "python", "machine learning" ]
---

# Introduction

[scikit-learn](https://scikit-learn.org) is one of the most popular Python libraries for machine learning, providing 
tools for supervised, unsupervised, and semi-supervised learning, as well as utilities for preprocessing, model 
selection, and more. 

This guide explores key features of the library with practical examples.

Let's get started by installing `scikit-learn` into our environment:

{% highlight shell %}
pip install scikit-learn
{% endhighlight %}

# Supervised Learning

Supervised learning is a type of machine learning where the model learns to map input data to labeled outputs 
(targets) based on a given dataset. During training, the algorithm uses these labeled examples to understand the 
relationship between features and outcomes, enabling it to make accurate predictions or classifications on new, 
unseen data. This approach is commonly used for tasks like regression (predicting continuous values) and classification 
(categorizing data into discrete labels).

## Regression

Regression models in supervised learning are used to predict continuous outcomes. These models establish relationships 
between input features and a target variable. Here’s a summary of the primary types of regression models available in 
scikit-learn:

- **Linear Regression**: A simple and interpretable model that predicts outcomes based on a linear relationship between input features and the target variable. It's ideal for tasks like predicting house prices based on square footage.
  
- **Ridge and Lasso Regression**: These are regularized versions of linear regression that handle multicollinearity and high-dimensional data by adding penalties to large coefficients. Common applications include gene expression analysis and other domains with many correlated features.
  
- **Support Vector Regression (SVR)**: A kernel-based approach that captures non-linear relationships between inputs and outputs, making it effective for problems like stock price prediction.
  
- **Random Forest Regressor**: An ensemble method that uses multiple decision trees to make robust predictions. It excels in tasks such as forecasting temperature or sales trends.
  
- **Gradient Boosting**: This method iteratively improves predictions by focusing on poorly predicted samples. It's commonly used for complex tasks like predicting customer lifetime value.
  
- **K-Neighbors Regressor**: This algorithm predicts based on the average target value of the nearest neighbors in feature space, often used in property value estimation.

Regression models are essential for problems where understanding or predicting a continuous trend is the goal. 
scikit-learn’s implementations provide a range of options from simple to complex to handle varying levels of data 
complexity and feature interactions.

### Linear Regression

Linear Regression predicts a target variable as a linear combination of input features. 

Example: predicting house prices.

{% highlight python %}
from sklearn.linear_model import LinearRegression

# Example data
X = [[1], [2], [3]]
y = [2, 4, 6]

model = LinearRegression()
model.fit(X, y)
print(model.predict([[4]]))  # Predict for new data
{% endhighlight %}

In this example, the `LinearRegression` predicts what the next value for `y` is when given an unstudied `X` value.

Predicting for `[4]` gives us:

{% highlight plain %}
[8.]
{% endhighlight %}

### Ridge and Lasso Regression
These methods add regularization to Linear Regression to handle high-dimensional data.

{% highlight python %}
from sklearn.linear_model import Ridge

# Example data
X = [[1, 2], [2, 4], [3, 6]]
y = [1, 2, 3]

model = Ridge(alpha=1.0)
model.fit(X, y)
print(model.coef_)  # Regularized coefficients
{% endhighlight %}

This produces the following output:

{% highlight plain %}
[0.18181818 0.36363636]
{% endhighlight %}

The `coef_` value here represents the coefficients (weights) of the features in the fitted model. These coefficients 
provide insight into the importance and contribution of each feature to the target variable.

These are important for:

- **Understanding feature importance** By examining the magnitude of the coefficients, you can determine which features have the greatest impact on the target variable. Larger absolute values typically indicate more influential features.
- **Interpret relationships** The sign of each coefficient indicates the direction of the relationship. Positive implies an increase in the feature value increases the target value. Negative coefficients imply the opposite.
- **Feature selection** As a feature's coefficient approaches zero, its importance is diminished and therefore can inform your decision on selecting it as a feature
- **Predict target changes** The coefficients are treated as multipliers here, allowing you to predict the change on any other coefficients

### Support Vector Regression (SVR)
SVR captures non-linear relationships by using kernels.

{% highlight python %}
from sklearn.svm import SVR

# Example data
X = [[1], [2], [3]]
y = [1.5, 3.7, 2.1]

model = SVR(kernel='rbf')
model.fit(X, y)
print(model.predict([[2.5]]))
{% endhighlight %}

The output here is:

{% highlight plain %}
[2.70105299]
{% endhighlight %}

The relationship between the features in `X` and the targets in `y` is now *not* linear. As a result, SVR predicts a 
value by finding a function that fits within a tolerance margin (called the epsilon-tube) around the data. The `SVR` 
constructor allows you to control the `epsilon`: passing a smaller value will aim for a more exact fit; a larger value 
will aim at better generalisation.

### Random Forest Regressor
An ensemble method that averages predictions from multiple decision trees.

{% highlight python %}
from sklearn.ensemble import RandomForestRegressor

# Example data
X = [[1], [2], [3]]
y = [10, 20, 30]

model = RandomForestRegressor(n_estimators=100)
model.fit(X, y)
print(model.predict([[2.5]]))
{% endhighlight %}

The output (when I run it) of this:

{% highlight plain %}
[22.5]
{% endhighlight %}

Run this a few more times though, and you'll see that you get different output values. As the name suggests, Random 
Forrest Regressor uses randomness in building its model which may lead to slightly different values each time the 
model is trained. This randomness helps improve the model's generalisation ability.

### Gradient Boosting
Boosting combines weak learners to achieve higher accuracy.

{% highlight python %}
from sklearn.ensemble import GradientBoostingRegressor

# Example data
X = [[1], [2], [3]]
y = [10, 15, 25]

model = GradientBoostingRegressor()
model.fit(X, y)
print(model.predict([[2.5]]))
{% endhighlight %}

The output of this is:

{% highlight plain %}
[15.00004427]
{% endhighlight %}

Gradient Boosting builds models sequentially to improve predictions by focusing on errors that it observes from 
previous models. It works by doing the following:

- **Sequential model building** Each subsequent model build attempts to correct residual errors from a previous model build
- **Gradient descent optimisation** Rather than fitting the target variable, gradient boosting aims at minimising the loss function
- **Weight contribution** Predictions from all models are combined, often using weighted sums to produce a final prediction

### K-Neighbors Regressor
Predicts the target value based on the mean of the nearest neighbors.

{% highlight python %}
from sklearn.neighbors import KNeighborsRegressor

# Example data
X = [[1], [2], [3]]
y = [1, 2, 3]

model = KNeighborsRegressor(n_neighbors=2)
model.fit(X, y)
print(model.predict([[2.5]]))
{% endhighlight %}

The output of this:

{% highlight plain %}
[2.5]
{% endhighlight %}

K-Neighbors Regressor is a non-parametric algorithm. It relies on the similarity between data points to predict the 
target value for a new input. It works in the following way:

- **Find neighbors** The algorithm identifies the \( K \) nearest data points (neighbors) in the feature space using a distance function (Euclidean/Manhattan/Minkowski)
- **Predict the value** The target value for the input is computed as the weighted average of the target value of the \( K \) neighbors

### Summary

| **Algorithm**             | **Where It Excels**                                                                                  | **Where to Avoid**                                                                               | **Additional Notes**                                                                                                      |
|----------------------------|------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------|
| **Linear Regression**      | - Simple, interpretable tasks.<br>- Problems with few features and a linear relationship.           | - Non-linear relationships.<br>- Datasets with outliers.<br>- Multicollinearity.                | Coefficients provide insights into feature importance.                                                                   |
| **Ridge Regression**       | - Handling multicollinearity.<br>- High-dimensional datasets.                                       | - Sparse datasets where some features are irrelevant.                                           | Adds \( L2 \) penalty (squared magnitude of coefficients).                                                               |
| **Lasso Regression**       | - Feature selection (shrinks irrelevant feature weights to zero).<br>- High-dimensional datasets.   | - Scenarios needing all features for predictions.<br>- Datasets with high noise levels.         | Adds \( L1 \) penalty (absolute value of coefficients).                                                                  |
| **ElasticNet Regression**  | - Combines Ridge and Lasso strengths for datasets with multiple feature types.                     | - Small datasets where simpler methods like Linear Regression suffice.                          | Balances \( L1 \) and \( L2 \) penalties via an `l1_ratio` parameter.                                                    |
| **Support Vector Regression (SVR)** | - Capturing non-linear relationships.<br>- Small to medium-sized datasets.                                          | - Large datasets (slow training).<br>- Poorly scaled features (sensitive to scaling).           | Uses kernels (e.g., RBF) to model non-linear relationships.                                                             |
| **Random Forest Regressor** | - Robust to outliers.<br>- Non-linear relationships.<br>- Feature importance estimation.           | - High-dimensional sparse data.<br>- Very large datasets (may require more memory).             | Ensemble method combining multiple decision trees.                                                                       |
| **Gradient Boosting Regressor** | - Complex datasets.<br>- Predictive tasks with high accuracy requirements.<br>- Tabular data.           | - Large datasets without sufficient computational resources.<br>- Overfitting if not regularized. | Iteratively improves predictions by focusing on poorly predicted samples.                                                |
| **K-Neighbors Regressor**  | - Small datasets with local patterns.<br>- Non-linear relationships without feature engineering.    | - Large datasets (computationally expensive).<br>- High-dimensional feature spaces.             | Predictions are based on the mean of \( k \) nearest neighbors in the feature space.                                     |

## Classification

Classification is a supervised learning technique used to predict discrete labels (classes) for given input data. In 
scikit-learn, various classification models are available, each suited for different types of problems. Here's a summary 
of some key classification models:

- **Logistic Regression** A simple yet powerful model that predicts probabilities of class membership using a logistic function. It works well for both binary (e.g., spam detection) and multi-class classification tasks. Logistic Regression is interpretable and often serves as a baseline model.
- **Decision Tree Classifier** A tree-based model that splits data based on feature values, creating interpretable decision rules. Decision trees excel in explaining predictions and handling non-linear relationships. They are prone to overfitting but can be controlled with pruning or parameter constraints.
- **Random Forest Classifier** An ensemble method that combines multiple decision trees to improve accuracy and reduce overfitting. Random forests are robust and handle high-dimensional data well. They're commonly used in applications like disease diagnosis and image classification.
- **Support Vector Machine (SVM)** SVMs create a hyperplane that separates classes while maximizing the margin between them. They are effective for both linear and non-linear classification tasks and work well for problems like handwriting recognition. SVMs are sensitive to feature scaling and require tuning parameters like the kernel type.
- **Naive Bayes** A probabilistic model based on Bayes' theorem, assuming independence between features. Naive Bayes is fast and efficient for high-dimensional data, making it ideal for text classification problems like spam filtering.
- **k-Nearest Neighbors (k-NN)** A simple and intuitive algorithm that classifies based on the majority label of \( k \) nearest neighbors in the feature space. It works well for recommendation systems and other tasks where the decision boundary is complex but local patterns are important.
- **Gradient Boosting Classifier** A powerful ensemble technique that iteratively improves predictions by correcting errors of previous models. Gradient Boosting achieves high accuracy on structured/tabular data and is often used in competitions and real-world applications like fraud detection.

### Logistic Regression
A simple classifier for binary or multi-class problems.

{% highlight python %}
from sklearn.linear_model import LogisticRegression

# Example data
X = [[1], [2], [3]]
y = [0, 1, 0]

model = LogisticRegression()
model.fit(X, y)
print(model.predict_proba([[2.5]]))
{% endhighlight %}

This will give you a list of values that are the probability of the `[2.5]` value is in the classification of `0` or `1`.
At these small data levels, the output probabilities make some boundary decisions that don't appear correct at first.

As the sample set grows and becomes more diverse, the classifications normalise.

### Decision Tree Classifier
A rule-based model for interpretable predictions.

{% highlight python %}
from sklearn.tree import DecisionTreeClassifier

# Example data
X = [[1], [2], [3]]
y = [0, 1, 0]

model = DecisionTreeClassifier()
model.fit(X, y)
print(model.predict([[2.5]]))
{% endhighlight %}

This output of this is:

{% highlight plain %}
[1]
{% endhighlight %}

As the input value moves further away from `2`, the output starts snapping to `0`.

This algorithm starts at the root node and selects the feature and threshold that best divide the dataset into subsets 
with the most homogeneous class labels.

The process is repeated for each subset until a stopping criterion is met, such as:

* Maximum tree depth.
* Minimum samples in a leaf node.
* All samples in a subset belong to the same class.

### Random Forest Classifier
An ensemble method that reduces overfitting.

{% highlight python %}
from sklearn.ensemble import RandomForestClassifier

# Example data
X = [[1], [2], [3]]
y = [0, 1, 0]

model = RandomForestClassifier()
model.fit(X, y)
print(model.predict([[2.5]]))
{% endhighlight %}

This output of this is:

{% highlight plain %}
[1]
{% endhighlight %}

Having a look at `predict_proba` from the `model` object, we can see the probabilities of the value being classified:

{% highlight plain %}
[[0.32 0.68]]
{% endhighlight %}

`2.5` gives us a `68%` chance according to the model that we should classify as a `1`.

This algorithm works by:

- Building multiple decision trees during training.
- Each tree is trained on a random subset of data (bagging) and considers a random subset of features at each split.
- Predictions are made by majority voting across all trees.

### Support Vector Machine (SVM)
Maximizes the margin between classes for classification.

{% highlight python %}
from sklearn.svm import SVC

# Example data
X = [[1], [2], [3], [2.5]]
y = [0, 1, 0, 1]

model = SVC(kernel='linear')
model.fit(X, y)
print(model.predict([[2.]]))

{% endhighlight %}

The output of this is:

{% highlight plain %}
[1]
{% endhighlight %}

SVM is a supervised learning algorithm used for classification and regression tasks. SVM finds the hyperplane that best 
separates classes while maximizing the margin between them.

### Naive Bayes
A probabilistic model based on Bayes’ theorem.

{% highlight python %}
from sklearn.naive_bayes import GaussianNB

# Example data
X = [[1], [2], [3]]
y = [0, 1, 0]

model = GaussianNB()
model.fit(X, y)
print(model.predict([[2.5]]))
{% endhighlight %}

The output of this is:

{% highlight plain %}
[0]
{% endhighlight %}

Naive Bayes (based on [Bayes' theorm](https://en.wikipedia.org/wiki/Bayes%27_theorem)) calculates the posterior 
probability of each class for a given input and assigns the class with the highest probability.

Types of Naive Bayes Models:

- **Gaussian Naive Bayes**: Assumes features follow a normal distribution (continuous data).
- **Multinomial Naive Bayes**: Suitable for discrete data, commonly used for text classification (e.g., word counts).
- **Bernoulli Naive Bayes**: Handles binary/boolean features (e.g., word presence/absence).

### k-Nearest Neighbors (k-NN)
Classifies based on the majority label of neighbors.

{% highlight python %}
from sklearn.neighbors import KNeighborsClassifier

# Example data
X = [[1], [2], [3]]
y = [0, 1, 0]

model = KNeighborsClassifier(n_neighbors=2)
model.fit(X, y)
print(model.predict([[2.5]]))
{% endhighlight %}

### Gradient Boosting Classifier

Gradient Boosting Classifier is a supervised learning algorithm that builds an ensemble of weak learners (typically 
decision trees) sequentially, with each new tree correcting the errors of the previous ones.

{% highlight python %}
from sklearn.ensemble import GradientBoostingClassifier

# Example data
X = [[1], [2], [3]]
y = [0, 1, 0]

model = GradientBoostingClassifier()
model.fit(X, y)
print(model.predict([[2.5]]))
{% endhighlight %}

How it works:

1. **Start with a Simple Model**:
The process begins with a weak learner (e.g., a small decision tree) that makes initial predictions.

2. **Compute Residuals**:
The errors (residuals) from the previous predictions are calculated.

3. **Fit the Next Model**:
A new weak learner is trained to predict the residuals.

4. **Combine Models**:
The predictions from all learners are combined (weighted sum) to form the final output.

5. **Gradient Descent**:
The algorithm minimizes the loss function (e.g., log loss for classification) by iteratively updating the predictions.

### Summary

| **Algorithm**             | **Where It Excels**                                                                                  | **Where to Avoid**                                                                               | **Additional Notes**                                                                                                      |
|----------------------------|------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------|
| **Logistic Regression**    | - Binary or multi-class classification.<br>- Interpretable and simple problems.<br>- Linearly separable data. | - Non-linear decision boundaries.<br>- Complex datasets with many features.                    | Outputs probabilities for class membership. Often used as a baseline model.                                              |
| **Decision Tree Classifier** | - Interpretable models.<br>- Handling non-linear relationships.<br>- Small to medium-sized datasets. | - Prone to overfitting on noisy data.<br>- Large datasets without pruning or constraints.        | Creates human-readable decision rules. Can be controlled using parameters like `max_depth`.                              |
| **Random Forest Classifier** | - Robust to overfitting.<br>- High-dimensional data.<br>- Tasks requiring feature importance ranking. | - Sparse datasets.<br>- Very large datasets (can require significant memory).                   | Ensemble method combining multiple decision trees. Uses bagging for improved performance.                                 |
| **Support Vector Machine (SVM)** | - Binary or multi-class problems with complex boundaries.<br>- High-dimensional feature spaces. | - Very large datasets (slow training).<br>- Datasets requiring soft predictions (probabilities). | Effective for small to medium-sized datasets. Requires scaling of features for optimal performance.                      |
| **Naive Bayes**            | - High-dimensional data.<br>- Text classification (e.g., spam detection).<br>- Multiclass problems. | - Strong feature dependencies.<br>- Continuous numerical features without preprocessing.         | Assumes feature independence. Fast and efficient for large-scale problems.                                               |
| **k-Nearest Neighbors (k-NN)** | - Small datasets with complex decision boundaries.<br>- Non-parametric problems.                 | - Large datasets (computationally expensive).<br>- High-dimensional feature spaces.             | Relies on distance metrics (e.g., Euclidean). Sensitive to feature scaling.                                              |
| **Gradient Boosting Classifier** | - Tabular data.<br>- High accuracy requirements for structured datasets.<br>- Imbalanced data with class weighting. | - Large datasets with limited resources.<br>- Risk of overfitting if not regularized.           | Ensemble of weak learners that iteratively improves predictions. Requires careful hyperparameter tuning.                 |
| **Multilayer Perceptron (MLP)** | - Non-linear decision boundaries.<br>- Complex datasets with many features.                    | - Large datasets without sufficient computational resources.<br>- Requires careful tuning.       | Neural network-based classifier. Requires scaling of features. Can model complex patterns.                               |

# Unsupervised Learning

Unsupervised learning is a type of machine learning where the model identifies patterns, structures, or relationships 
in data without labeled outputs. Instead of predicting specific outcomes, the algorithm organizes or simplifies the 
data based on inherent similarities or differences. Common applications include clustering (grouping similar data points) 
and dimensionality reduction (compressing high-dimensional data for visualization or analysis).

## Clustering

Clustering is a technique in unsupervised learning that involves grouping data points into clusters based on their 
similarities or proximity in feature space. The goal is to organize data into meaningful structures, where points 
within the same cluster are more similar to each other than to those in other clusters. It is commonly used for tasks 
like customer segmentation, anomaly detection, and exploratory data analysis.

### K-Means

K-Means is an unsupervised learning algorithm that partitions a dataset into $$ K $$ clusters based on feature 
similarity.

{% highlight python %}
from sklearn.cluster import KMeans

# Example data
X = [[1], [2], [10], [11]]

model = KMeans(n_clusters=2)
model.fit(X)
print(model.labels_)
{% endhighlight %}

The output of this is:

{% highlight plain %}
[0 0 1 1]
{% endhighlight %}

This tells us that `[1]` and `[2]` are assigned the label of `0`, with `[10]` and `[11]` being assigned `1`.

How it works:

1. **Choose Initial Centroids**:
Randomly select $$ K $$ points as the initial cluster centroids.

2. **Assign Data Points to Clusters**:
Assign each data point to the nearest centroid based on a distance metric (e.g., Euclidean distance).

3. **Update Centroids**:
Recalculate the centroids as the mean of all points assigned to each cluster.

4. **Iterate**:
Repeat steps 2 and 3 until centroids stabilize or a maximum number of iterations is reached.

### DBSCAN

DBSCAN is an unsupervised clustering algorithm that groups data points based on density, identifying clusters of 
arbitrary shapes and detecting outliers.

{% highlight python %}
from sklearn.cluster import DBSCAN

# Example data
X = [[1], [2], [10], [11], [50000], [50001]]

model = DBSCAN(eps=10, min_samples=2)
model.fit(X)
print(model.labels_)
{% endhighlight %}

Clearly the `50000` and `500001` are should clearly be clustered together. The output here:

{% highlight plain %}
[0 0 0 0 1 1]
{% endhighlight %}

How it works:

1. **Core Points**:
A point is a core point if it has at least min_samples neighbors within a specified radius (eps).

2. **Reachable Points**:
A point is reachable if it lies within the eps radius of a core point.

3. **Noise Points**:
Points that are neither core points nor reachable are classified as noise (outliers).

4. **Cluster Formation**:
Clusters are formed by connecting core points and their reachable points.

### Agglomerative Clustering

Agglomerative Clustering is a hierarchical, bottom-up clustering algorithm that begins with each data point as its own 
cluster and merges clusters iteratively based on a linkage criterion until a stopping condition is met.

{% highlight python %}
from sklearn.cluster import AgglomerativeClustering

# Example data
X = [[1], [2], [10], [11]]

model = AgglomerativeClustering(n_clusters=2)
model.fit(X)
print(model.labels_)
{% endhighlight %}

This outputs:

{% highlight plain %}
[1 1 0 0]
{% endhighlight %}

How it works:

1. **Start with Individual Clusters**:
Each data point is treated as its own cluster.

2. **Merge Clusters**:
Clusters are merged step-by-step based on a similarity metric and a linkage criterion.

3. **Stop Merging**:
The process continues until the desired number of clusters is reached, or all points are merged into a single cluster.

4. **Dendrogram**:
A tree-like diagram (dendrogram) shows the hierarchical relationship between clusters.

### Summary

| **Algorithm**             | **Where It Excels**                                                                                  | **Where to Avoid**                                                                               | **Additional Notes**                                                                                                      |
|----------------------------|------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------|
| **K-Means**               | - Partitioning data into well-defined, compact clusters.<br>- Large datasets with distinct clusters. | - Non-spherical clusters.<br>- Highly imbalanced cluster sizes.<br>- Datasets with noise or outliers. | Relies on centroids; sensitive to initializations. Requires the number of clusters (`k`) to be specified beforehand.      |
| **DBSCAN**                | - Finding clusters of arbitrary shapes.<br>- Detecting outliers.<br>- Spatial data analysis.         | - High-dimensional data.<br>- Datasets with varying densities.<br>- Requires careful tuning of `eps` and `min_samples`. | Density-based approach. Does not require the number of clusters to be predefined. Can identify noise points as outliers. |
| **Agglomerative Clustering** | - Hierarchical relationships between clusters.<br>- Small to medium-sized datasets.              | - Large datasets (computationally expensive).<br>- Very high-dimensional data.                   | Hierarchical clustering. Outputs a dendrogram for visualizing cluster merges.                                            |

## Dimensionality Reduction

### PCA

PCA is an unsupervised dimensionality reduction technique that transforms data into a lower-dimensional space while 
preserving as much variance as possible.

{% highlight python %}
from sklearn.decomposition import PCA

# Example data
X = [[1, 2], [3, 4], [5, 6]]

model = PCA(n_components=1)
transformed = model.fit_transform(X)
print(transformed)
{% endhighlight %}

This outputs the following:

{% highlight plain %}
[[-2.82842712]
 [ 0.        ]
 [ 2.82842712]]
{% endhighlight %}

### t-SNE

Visualizes high-dimensional data.

{% highlight python %}
import numpy as np
from sklearn.manifold import TSNE

# Example data
X = np.array([[1, 2], [3, 4], [5, 6]])

model = TSNE(n_components=2, perplexity=2, random_state=10)
transformed = model.fit_transform(X)
print(transformed)
{% endhighlight %}

The output from this is:

{% highlight plain %}
[[-200.7746     0.     ]
 [ 139.41475    0.     ]
 [ 479.60336    0.     ]]
{% endhighlight %}

### NMF
Non-negative matrix factorization for feature extraction.

{% highlight python %}
from sklearn.decomposition import NMF

# Example data
X = [[1, 2], [3, 4], [5, 6]]

model = NMF(n_components=2)
transformed = model.fit_transform(X)
print(transformed)
{% endhighlight %}

The output of this is:

{% highlight plain %}
[[1.19684855 0.        ]
 [0.75282266 0.72121572]
 [0.1905593  1.48561981]]
{% endhighlight %}


# 3. Semi-Supervised Learning

Semi-supervised learning bridges the gap between supervised and unsupervised learning by utilizing a small amount of 
labeled data alongside a large amount of unlabeled data.

## Label Propagation
Label Propagation spreads label information from labeled to unlabeled data.

{% highlight python %}
from sklearn.semi_supervised import LabelPropagation

# Example data
X = [[1], [2], [3], [4], [5]]
y = [0, 1, -1, -1, -1]  # -1 indicates unlabeled data

model = LabelPropagation()
model.fit(X, y)
print(model.transduction_)  # Predicted labels for unlabeled data
{% endhighlight %}

The output of this shows the remainder of the data getting labelled:

{% highlight plain %}
[0 1 1 1 1]
{% endhighlight %}

### Self-Training
Self-Training generates pseudo-labels for unlabeled data using a supervised model.

{% highlight python %}
from sklearn.semi_supervised import SelfTrainingClassifier
from sklearn.ensemble import RandomForestClassifier

# Example data
X = [[1], [2], [3], [4], [5]]
y = [0, 1, -1, -1, -1]  # -1 indicates unlabeled data

base_model = RandomForestClassifier()
model = SelfTrainingClassifier(base_model)
model.fit(X, y)
print(model.predict([[3.5]]))  # Predict for unlabeled data
{% endhighlight %}

The unlabelled `[3]` value now is returned with a value:

{% highlight plain %}
[1]
{% endhighlight %}

# 4. Model Selection

Model selection is the process of identifying the best machine learning model and its optimal configuration for a given 
dataset and problem. It involves comparing different models, evaluating their performance using metrics (e.g., accuracy, 
F1-score, or RMSE), and tuning their hyperparameters to maximize predictive accuracy or minimize error.

## Cross-Validation

Cross-validation is a model evaluation technique that assesses a model's performance by dividing the dataset into 
multiple subsets (folds) for training and testing.

{% highlight python %}
from sklearn.model_selection import cross_val_score
from sklearn.linear_model import LogisticRegression

# Example data
X = [[1], [2], [3], [4]]
y = [0, 1, 0, 1]

model = LogisticRegression()
scores = cross_val_score(model, X, y, cv=2)
print(scores)  # Accuracy scores for each fold
{% endhighlight %}

The output here is:

{% highlight plain %}
[0.5 0.5]
{% endhighlight %}

How it works:

- **Split the Data**: The dataset is split into $$ k $$ folds (subsets).
- **Train and Test**: The model is trained on $$ k - 1 $$ folds and tested on the remaining fold. This process repeats \( k \) times, with each fold used as the test set once.
- **Aggregate Results**: Performance metrics (e.g., accuracy, F1-score) from all folds are averaged to provide an overall evaluation.

### GridSearchCV

GridSearchCV is a tool in `scikit-learn` for hyperparameter tuning that systematically searches for the best combination 
of hyperparameters by evaluating all possible parameter combinations in a given grid.

{% highlight python %}
from sklearn.model_selection import GridSearchCV
from sklearn.svm import SVC

# Example data
X = [[1], [2], [3], [4]]
y = [0, 1, 0, 1]

param_grid = {'C': [0.1, 1, 10], 'kernel': ['linear', 'rbf']}
model = GridSearchCV(SVC(), param_grid, cv=2)
model.fit(X, y)
print(model.best_params_)  # Best parameters
{% endhighlight %}

The output of this is:

{% highlight plain %}
{'C': 0.1, 'kernel': 'linear'}
{% endhighlight %}

How it works:

- **Define a Parameter Grid**: Specify the hyperparameters and their possible values (e.g., `{'C': [0.1, 1, 10], 'kernel': ['linear', 'rbf']}` for an SVM).
- **Cross-Validation**: For each combination of parameters, the model is evaluated using cross-validation to estimate its performance.
- **Select the Best Model**: The combination of hyperparameters that produces the best cross-validation score is chosen.

### RandomizedSearchCV

RandomizedSearchCV is a hyperparameter tuning tool in scikit-learn that randomly samples a fixed number of parameter 
combinations from a specified grid and evaluates them using cross-validation.

{% highlight python %}
from sklearn.model_selection import RandomizedSearchCV
from sklearn.ensemble import RandomForestClassifier

# Example data
X = [[1], [2], [3], [4]]
y = [0, 1, 0, 1]

param_distributions = {'n_estimators': [10, 50, 100], 'max_depth': [None, 10, 20]}
model = RandomizedSearchCV(RandomForestClassifier(), param_distributions, n_iter=5, cv=2)
model.fit(X, y)
print(model.best_params_)  # Best parameters
{% endhighlight %}

The output of this is:

{% highlight plain %}
{'n_estimators': 100, 'max_depth': None}
{% endhighlight %}

How it works:

- **Define a Parameter Distribution**: Specify the hyperparameters and their possible ranges (distributions or lists) to sample from.
- **Random Sampling**: A fixed number of parameter combinations is randomly selected and evaluated.
- **Cross-Validation**: For each sampled combination, the model is evaluated using cross-validation.
- **Select the Best Model**: The parameter combination that yields the best performance is chosen.

# 5. Feature Selection

Feature selection is the process of identifying the most relevant features in a dataset for improving a machine learning 
model's performance. By reducing the number of features, it helps eliminate redundant, irrelevant, or noisy data, 
leading to simpler, faster, and more interpretable models

## SelectKBest

SelectKBest is a feature selection method in scikit-learn that selects the top $$ k $$ features from the dataset based 
on univariate statistical tests.

{% highlight python %}
from sklearn.feature_selection import SelectKBest, f_classif

# Example data
X = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
y = [0, 1, 0]

model = SelectKBest(f_classif, k=2)
X_new = model.fit_transform(X, y)
print(X_new)  # Selected features
{% endhighlight %}

The output of this is:

{% highlight plain %}
[[2 3]
 [5 6]
 [8 9]]
{% endhighlight %}

How it works:

- **Choose a Scoring Function**: Select a statistical test (e.g., ANOVA, chi-square, mutual information) to evaluate feature relevance.
- **Compute Scores**: Each feature is scored based on its relationship with the target variable.
- **Select Top $$ k $$ Features**: The $$ k $$ highest-scoring features are retained for the model.

## Recursive Feature Elimination (RFE)

RFE is a feature selection method that recursively removes the least important features based on a model's performance 
until the desired number of features is reached.

{% highlight python %}
from sklearn.feature_selection import RFE
from sklearn.svm import SVC

# Example data
X = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
y = [0, 1, 0]

model = RFE(SVC(kernel='linear'), n_features_to_select=2)
X_new = model.fit_transform(X, y)
print(X_new)  # Selected features
{% endhighlight %}

The output of this is:

{% highlight plain %}
[[1 3]
 [4 6]
 [7 9]]
{% endhighlight %}

How it works:

- **Fit a Model**: Train a machine learning model (e.g., Logistic Regression, SVM) on the dataset.
- **Rank Features**: The model assigns importance scores to the features (e.g., weights or coefficients).
- **Remove Features**: Eliminate the least important features (based on the scores) and refit the model.
- **Repeat**: Continue the process until the specified number of features is retained.

## VarianceThreshold

VarianceThreshold is a simple feature selection method in scikit-learn that removes features with low variance, 
assuming that low-variance features do not carry much information.

{% highlight python %}
from sklearn.feature_selection import VarianceThreshold

# Example data
X = [[0, 2, 0], [0, 3, 0], [0, 4, 0]]

model = VarianceThreshold(threshold=0.5)
X_new = model.fit_transform(X)
print(X_new)  # Features with variance above threshold
{% endhighlight %}

The output of this is:

{% highlight plain %}
[[2]
 [3]
 [4]]
{% endhighlight %}

The zeros don't change, so they're stripped from the result.

How it works:

- **Compute Variance**: For each feature, calculate the variance across all samples.
- **Apply Threshold**: Remove features whose variance falls below a specified threshold.


# 6. Preprocessing

Preprocessing transforms raw data to make it suitable for machine learning algorithms.

## StandardScaler

StandardScaler is a preprocessing technique in scikit-learn that standardizes features by removing the mean and scaling 
them to unit variance (z-score normalization).

{% highlight python %}
from sklearn.preprocessing import StandardScaler

# Example data
X = [[1], [2], [3]]

scaler = StandardScaler()
X_scaled = scaler.fit_transform(X)
print(X_scaled)
{% endhighlight %}

This outputs the following:

{% highlight plain %}
[[-1.22474487]
 [ 0.        ]
 [ 1.22474487]]
{% endhighlight %}

How it works:

- **Compute Mean and Standard Deviation**:  
  For each feature, calculate its mean $$ (\mu) $$ and standard deviation $$ (\sigma) $$.

- **Transform Features**:  
  Scale each feature $$ ( x ) $$ using the formula:  
  $$ z = \frac{x - \mu}{\sigma} $$

This results in features with a mean of 0 and a standard deviation of 1.

## MinMaxScaler

MinMaxScaler is a preprocessing technique in scikit-learn that scales features to a fixed range, typically `[0, 1]`. 
It preserves the relationships between data points while ensuring all features are within the specified range.

{% highlight python %}
from sklearn.preprocessing import MinMaxScaler

# Example data
X = [[1], [2], [3]]

scaler = MinMaxScaler()
X_scaled = scaler.fit_transform(X)
print(X_scaled)
{% endhighlight %}

This outputs the following:

{% highlight plain %}
[[0. ]
 [0.5]
 [1. ]]
{% endhighlight %}

How it works:

### How It Works:

- **Compute Minimum and Maximum Values**: For each feature, calculate its minimum $$( \text{min} )$$ and maximum $$( \text{max} )$$ values.
- **Transform Features**: Scale each feature $$ x $$ using the formula:  
   $$ x' = \frac{x - \text{min}}{\text{max} - \text{min}} \times (\text{max}_{\text{scale}} - \text{min}_{\text{scale}}) + \text{min}_{\text{scale}} $$  

   By default, $$ \text{min}_{\text{scale}} = 0 \) and \( \text{max}_{\text{scale}} = 1 $$.


## PolynomialFeatures

PolynomialFeatures is a preprocessing technique in scikit-learn that generates new features by adding polynomial 
combinations of existing features up to a specified degree.

{% highlight python %}
from sklearn.preprocessing import PolynomialFeatures

# Example data
X = [[1], [2], [3]]

poly = PolynomialFeatures(degree=2)
X_poly = poly.fit_transform(X)
print(X_poly)
{% endhighlight %}

This outputs the following:

{% highlight plain %}
[[1. 1. 1.]
 [1. 2. 4.]
 [1. 3. 9.]]
{% endhighlight %}

How it works:

- **Generate Polynomial Features**:  
   Creates polynomial terms (e.g., $$ ( x_1^2, x_1 \cdot x_2, x_2^3 ) $$ for the input features up to the specified degree.

- **Include Interaction Terms**:  
   Optionally includes interaction terms (e.g., $$ ( x_1 \cdot x_2 ) $$ to capture feature interactions.

- **Expand the Feature Space**:  
   Transforms the input dataset into a higher-dimensional space to model non-linear relationships.

## LabelEncoder

LabelEncoder is a preprocessing technique in scikit-learn that encodes categorical labels as integers, making them 
suitable for machine learning algorithms that require numerical input.

{% highlight python %}
from sklearn.preprocessing import LabelEncoder

# Example data
y = ['cat', 'dog', 'cat']

encoder = LabelEncoder()
y_encoded = encoder.fit_transform(y)
print(y_encoded)
{% endhighlight %}

The output of is this:

{% highlight plain %}
[0 1 0]
{% endhighlight %}

- **Fit to Labels**:  
   Maps each unique label in the dataset to an integer.  
   Example: `['cat', 'dog', 'mouse']` → `[0, 1, 2]`.

- **Transform Labels**:  
   Converts the original labels into their corresponding integer representation.

- **Inverse Transform**:  
   Converts encoded integers back into their original labels.

## OneHotEncoder

OneHotEncoder is a preprocessing technique in scikit-learn that converts categorical data into a binary matrix 
(one-hot encoding), where each category is represented by a unique binary vector.

{% highlight python %}
from sklearn.preprocessing import OneHotEncoder

# Example data
X = [['cat'], ['dog'], ['cat']]

encoder = OneHotEncoder()
X_encoded = encoder.fit_transform(X).toarray()
print(X_encoded)
{% endhighlight %}

The output of this:

{% highlight plain %}
[[1. 0.]
 [0. 1.]
 [1. 0.]]
{% endhighlight %}

How it works:

- **Fit to Categories**:  
   Identifies the unique categories in each feature.

- **Transform Features**:  
   Converts each category into a binary vector, with a `1` indicating the presence of the category and `0` elsewhere.

- **Sparse Representation**:  
   By default, the output is a sparse matrix to save memory for large datasets with many categories.


## Imputer
Imputer fills in missing values in datasets.

{% highlight python %}
from sklearn.impute import SimpleImputer

# Example data
X = [[1, 2], [None, 3], [7, 6]]

imputer = SimpleImputer(strategy='mean')
X_imputed = imputer.fit_transform(X)
print(X_imputed)
{% endhighlight %}

The output of this, you can see the `None` has been filled in:

{% highlight plain %}
[[1. 2.]
 [4. 3.]
 [7. 6.]]
{% endhighlight %}

How it works:

- **Identify Missing Values**:  
   Detects missing values in the dataset (default: `np.nan`).

- **Compute Replacement Values**:  
   Based on the chosen strategy (e.g., mean or median), calculates replacement values for each feature.

- **Fill Missing Values**:  
   Replaces missing values in the dataset with the computed replacements.

# 7. Pipelines and Utilities

Pipelines streamline workflows by chaining preprocessing steps with modeling.

## Pipeline

A Pipeline in scikit-learn is a sequential workflow that chains multiple preprocessing steps and a final estimator 
into a single object, simplifying and automating machine learning workflows.

{% highlight python %}
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.svm import SVC

# Example data
X = [[1], [2], [3]]
y = [0, 1, 0]

pipeline = Pipeline([
    ('scaler', StandardScaler()),
    ('svc', SVC(kernel='linear'))
])
pipeline.fit(X, y)
print(pipeline.predict([[2.5]]))
{% endhighlight %}

## ColumnTransformer

ColumnTransformer in scikit-learn is a tool that applies different preprocessing steps to specific columns of a 
dataset, enabling flexible and efficient handling of mixed data types.

{% highlight python %}
from sklearn.compose import ColumnTransformer
from sklearn.preprocessing import StandardScaler, OneHotEncoder

# Example data
X = [[1, 'cat'], [2, 'dog'], [3, 'cat']]

transformer = ColumnTransformer([
    ('num', StandardScaler(), [0]),
    ('cat', OneHotEncoder(), [1])
])
X_transformed = transformer.fit_transform(X)
print(X_transformed)
{% endhighlight %}

## FunctionTransformer

FunctionTransformer in scikit-learn allows you to apply custom or predefined functions to transform data as part of 
a machine learning pipeline.

{% highlight python %}
from sklearn.preprocessing import FunctionTransformer
import numpy as np

# Example data
X = [[1], [2], [3]]

log_transformer = FunctionTransformer(np.log1p)
X_transformed = log_transformer.fit_transform(X)
print(X_transformed)
{% endhighlight %}

# 8. Neural Network Integration

Though scikit-learn is not primarily designed for deep learning, it includes simple neural network models.

### MLPClassifier/MLPRegressor

MLPClassifier (for classification) and MLPRegressor (for regression) are multi-layer perceptron models in 
scikit-learn that implement neural networks with backpropagation. They are part of the feedforward neural network 
family.

{% highlight python %}
from sklearn.neural_network import MLPClassifier

# Example data
X = [[1], [2], [3]]
y = [0, 1, 0]

model = MLPClassifier(hidden_layer_sizes=(10,), max_iter=500)
model.fit(X, y)
print(model.predict([[2.5]]))
{% endhighlight %}

How it works:

- **Layers**:  
   Composed of an input layer, one or more hidden layers, and an output layer. Hidden layers process weighted inputs using activation functions.

- **Activation Functions**:  
   - **Hidden Layers**: Use non-linear activation functions like ReLU (`'relu'`) or tanh (`'tanh'`).
   - **Output Layer**: 
     - MLPClassifier: Uses softmax for multi-class classification or logistic sigmoid for binary classification.
     - MLPRegressor: Uses linear activation.

- **Optimization**:  
   Parameters are learned through backpropagation using stochastic gradient descent (SGD) or adaptive optimizers like Adam.

#### Key Parameters:

- **`hidden_layer_sizes`**:  
  Tuple specifying the number of neurons in each hidden layer.  
  Example: `hidden_layer_sizes=(100, 50)` creates two hidden layers with 100 and 50 neurons respectively.

- **`activation`**:  
  Activation function for hidden layers:  
  - `'relu'` (default): Rectified Linear Unit.  
  - `'tanh'`: Hyperbolic tangent.  
  - `'logistic'`: Sigmoid function.

- **`solver`**:  
  Optimization algorithm:  
  - `'adam'` (default): Adaptive Moment Estimation (fast and robust).  
  - `'sgd'`: Stochastic Gradient Descent.  
  - `'lbfgs'`: Quasi-Newton optimization (good for small datasets).

- **`alpha`**:  
  Regularization term to prevent overfitting (default: `0.0001`).

- **`learning_rate`**:  
  Determines how weights are updated:  
  - `'constant'`: Fixed learning rate.  
  - `'adaptive'`: Adjusts learning rate based on performance.

- **`max_iter`**:  
  Maximum number of iterations for training (default: `200`).

## Conclusion

scikit-learn is a versatile library that offers robust tools for a wide range of machine learning tasks, from regression 
and classification to clustering, dimensionality reduction, and preprocessing. Its simplicity and efficiency make it a 
great choice for both beginners and advanced practitioners. Explore its extensive documentation to dive deeper into its 
capabilities!
