---
layout: post
title: Turning Words into Vectors
date: 2024-10-19
comments: false
categories: [ "nlp", "text" ]
---

# Introduction

In our [previous post]({% post_url 2024-10-19-computers-understanding-text %}), we covered the preprocessing steps 
necessary to convert text into a machine-readable format, like tokenization and stop word removal. But once the text is 
preprocessed, how do we represent it for use in machine learning models?

Before the rise of **word embeddings**, simpler techniques were commonly used to represent text as vectors. Today, we’ll 
explore two foundational techniques: **One-Hot Encoding** and **Bag-of-Words (BoW)**. These methods don’t capture the 
semantic meaning of words as well as modern embeddings do, but they’re essential for understanding the evolution of 
Natural Language Processing (NLP).

# One-Hot Encoding

One of the simplest ways to represent text is through **One-Hot Encoding**. In this approach, each word in a vocabulary 
is represented as a vector where all the elements are zero, except for a single element that corresponds to the word's 
index.

Let’s take a small vocabulary:

{% highlight plain %}
["I", "love", "NLP"]
{% endhighlight %}

The vocabulary size is 3, and each word will be represented by a 3-dimensional vector:

{% highlight plain %}
I -> [1, 0, 0] 
love -> [0, 1, 0] 
NLP -> [0, 0, 1]
{% endhighlight %}

Each word is "hot" (1) in one specific position, while "cold" (0) everywhere else.

## Example

Let’s generate one-hot encoded vectors using Python:

{% highlight python %}
from sklearn.preprocessing import OneHotEncoder
import numpy as np

# Define a small vocabulary
vocab = ["I", "love", "NLP"]
# Reshape the data for OneHotEncoder
vocab_reshaped = np.array(vocab).reshape(-1, 1)

# Initialize the OneHotEncoder
encoder = OneHotEncoder(sparse_output=False)
# Fit and transform the vocabulary
onehot_encoded = encoder.fit_transform(vocab_reshaped)
print(onehot_encoded)
{% endhighlight %}

The output shows the three indexed words:

{% highlight plain %}
[[1. 0. 0.]
 [0. 0. 1.]
 [0. 1. 0.]]
{% endhighlight %}

Each word has a unique binary vector representing its position in the vocabulary.

## Drawbacks of One-Hot Encoding

One-Hot Encoding is simple but comes with some limitations:

* High Dimensionality: For large vocabularies, the vectors become huge, leading to a "curse of dimensionality".
* Lack of Semantic Information: One-Hot vectors don’t capture any relationships between words. "love" and "like" would have completely different vectors, even though they are semantically similar.

# Bag-of-Words (BoW)

One-Hot Encoding represents individual words, but what about whole documents or sentences? That’s where the Bag-of-Words 
(BoW) model comes in. In BoW, the text is represented as a vector of word frequencies.

BoW counts how often each word from a given vocabulary appears in the document, without considering the order of words 
(hence, a "bag" of words).

Let’s take two example sentences:

1. "I love NLP"
2. "NLP is amazing"

The combined vocabulary for these two sentences is:

{% highlight plain %}
["I", "love", "NLP", "is", "amazing"]
{% endhighlight %}

Now, using BoW, we represent each sentence as a vector of word counts:

1. "I love NLP" -> `[1, 1, 1, 0, 0]` (since "I", "love", and "NLP" appear once, and "is" and "amazing" don't appear)
2. "NLP is amazing" -> `[0, 0, 1, 1, 1]` (since "NLP", "is", and "amazing" appear once, and "I" and "love" don't appear)

## Example

We can use `CountVectorizer` from the `sklearn` library to easily apply Bag-of-Words to a corpus of text:

{% highlight python %}
from sklearn.feature_extraction.text import CountVectorizer

# Define a set of documents
corpus = [
    "I love NLP",
    "NLP is amazing"
]

# Initialize the CountVectorizer
vectorizer = CountVectorizer()
# Transform the corpus into BoW vectors
X = vectorizer.fit_transform(corpus)

# Display the feature names (the vocabulary)
print(vectorizer.get_feature_names_out())

# Display the BoW representation
print(X.toarray())
{% endhighlight %}

The output of which looks like this:

{% highlight plain %}
['amazing' 'is' 'love' 'nlp']
[[0 0 1 1]
 [1 1 0 1]]
{% endhighlight %}

## Limitations of Bag-of-Words

While BoW is a simple and powerful method, it too has its drawbacks:

1. Sparsity: Like One-Hot Encoding, BoW produces high-dimensional and sparse vectors, especially for large vocabularies.
2. No Word Order: BoW ignores word order. The sentence "I love NLP" is treated the same as "NLP love I", which may not always make sense.
3. No Semantic Relationships: Just like One-Hot Encoding, BoW doesn't capture the meaning or relationships between words. All words are treated as independent features.

# Conclusion

Both One-Hot Encoding and Bag-of-Words are simple and effective ways to represent text as numbers, but they have significant 
limitations, particularly in capturing semantic relationships and dealing with large vocabularies.

These methods laid the groundwork for more sophisticated representations like **TF-IDF** (which we'll cover next) and 
eventually led to word embeddings, which capture the meaning and context of words more effectively.