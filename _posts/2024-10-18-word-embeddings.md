---
layout: post
title: Word Embeddings
date: 2024-10-18
comments: false
categories: [ "python", "word", "embedding" ]
---

# Introduction 

Word embeddings are one of the most significant advancements in natural language processing (NLP). They allow us to 
transform words or sentences into vectors, where each word is represented by a point in a high-dimensional space. 
The core idea is that words with similar meanings are close to each other in this space, making it possible to use 
mathematical operations on these vectors to uncover relationships between words.

In this post, we'll explore how to create word embeddings using a pre-trained model, and we'll perform various vector 
operations to see how these embeddings capture semantic relationships. We’ll cover examples like analogy generation, 
word similarity, and how these embeddings can be leveraged for search tasks.

# What Are Word Embeddings?

Word embeddings are dense vector representations of words, where each word is mapped to a point in a continuous vector 
space. Unlike older techniques (such as one-hot encoding) that give each word a unique identifier, embeddings represent 
words in a way that captures semantic relationships, such as similarity and analogy.

For example, embeddings can represent the relationship:

{% highlight python %}
king - man + woman = queen
{% endhighlight %}

This is made possible because words that are semantically similar (e.g., "king" and "queen") have vector representations 
that are close together in space, while words that are opposites (e.g., "good" and "bad") may have vectors pointing in 
opposite directions.

# Gensim

Let’s begin by loading a pre-trained word embedding model. We'll use the `glove-wiki-gigaword-50` model, which provides 
50-dimensional vectors for many common words.

{% highlight python %}
import gensim.downloader as api

# Load the pre-trained GloVe Word2Vec model
model = api.load("glove-wiki-gigaword-50")
{% endhighlight %}

This might take a moment to download. It's not too big.

Now that we have the model, let’s try converting some words into vectors.

# Converting Words to Vectors

We can take individual words and get their vector representations. Let’s look at the vectors for "king," "queen," "man," 
and "woman."

{% highlight python %}
# Example words
word1 = "king"
word2 = "queen"
word3 = "man"
word4 = "woman"

# Get the vectors for each word
vector_king = model[word1]
vector_queen = model[word2]
vector_man = model[word3]
vector_woman = model[word4]

# Print the vector for 'king'
print(f"Vector for '{word1}':\n{vector_king}")
{% endhighlight %}

You’ll see that each word is represented as a 50-dimensional vector. These vectors capture the meanings of the words in 
such a way that we can manipulate them mathematically.

{% highlight plain %}
Vector for 'king':
[ 0.50451   0.68607  -0.59517  -0.022801  0.60046  -0.13498  -0.08813
0.47377  -0.61798  -0.31012  -0.076666  1.493    -0.034189 -0.98173
0.68229   0.81722  -0.51874  -0.31503  -0.55809   0.66421   0.1961
-0.13495  -0.11476  -0.30344   0.41177  -2.223    -1.0756   -1.0783
-0.34354   0.33505   1.9927   -0.04234  -0.64319   0.71125   0.49159
0.16754   0.34344  -0.25663  -0.8523    0.1661    0.40102   1.1685
-1.0137   -0.21585  -0.15155   0.78321  -0.91241  -1.6106   -0.64426
-0.51042 ]
{% endhighlight %}

# Performing Vector Arithmetic

One of the most famous examples of vector arithmetic in word embeddings is the analogy:

{% highlight python %}
king - man + woman = queen
{% endhighlight %}

We can perform this operation by subtracting the vector for "man" from "king" and then adding the vector for "woman." 
Let’s try this and see what word is closest to the resulting vector.

{% highlight python %}
# Perform vector arithmetic
result_vector = vector_king - vector_man + vector_woman

# Find the closest word to the resulting vector
similar_words = model.similar_by_vector(result_vector, topn=3)

# Print the result
print("Result of 'king - man + woman':", similar_words)
{% endhighlight %}

You should find that the word closest to the resulting vector is "queen," demonstrating that the model captures the 
gender relationship between "king" and "queen."

# Measuring Word Similarity with Cosine Similarity

Another key operation you can perform on word embeddings is measuring the similarity between two words. The most common 
way to do this is by calculating the **cosine similarity** between the two vectors. The cosine similarity between two 
vectors is defined as:

$$ \text{cosine similarity} = \frac{A \cdot B}{\|A\| \|B\|} $$

This returns a value between -1 and 1:
- 1 means the vectors are identical (the words are very similar),
- 0 means the vectors are orthogonal (unrelated words),
- -1 means the vectors are pointing in opposite directions (possibly antonyms).

Let’s measure the similarity between related words like "apple" and "fruit," and compare it to unrelated words like 
"apple" and "car."

{% highlight python %}
import numpy as np
from numpy.linalg import norm

# Function to calculate cosine similarity
def cosine_similarity(vec1, vec2):
return np.dot(vec1, vec2) / (norm(vec1) * norm(vec2))

# Get vectors for 'apple', 'fruit', and 'car'
vector_apple = model['apple']
vector_fruit = model['fruit']
vector_car = model['car']

# Calculate cosine similarity
similarity_apple_fruit = cosine_similarity(vector_apple, vector_fruit)
similarity_apple_car = cosine_similarity(vector_apple, vector_car)

print(f"Cosine Similarity between 'apple' and 'fruit': {similarity_apple_fruit:.4f}")
print(f"Cosine Similarity between 'apple' and 'car': {similarity_apple_car:.4f}")
{% endhighlight %}

You will see that the cosine similarity between "apple" and "fruit" is much higher than that between "apple" and "car," 
illustrating the semantic relationship between "apple" and "fruit."

{% highlight plain %}
Cosine Similarity between 'apple' and 'fruit': 0.5918
Cosine Similarity between 'apple' and 'car': 0.3952
{% endhighlight %}

# Search Using Word Embeddings

Another powerful use of word embeddings is in search tasks. If you want to find words that are most similar to a given 
word, you can use the model’s `similar_by_word` function to retrieve the top N most similar words. Here’s how you can 
search for words most similar to "apple":

{% highlight python %}
# Find words most similar to 'apple'
similar_words_to_apple = model.similar_by_word('apple', topn=5)
print("Words most similar to 'apple':", similar_words_to_apple)
{% endhighlight %}

You can see here that "apple" is treated in the proper noun sense as in the company [Apple](https://www.apple.com/).

{% highlight plain %}
Words most similar to 'apple': [
('blackberry', 0.7543067336082458), 
('chips', 0.7438644170761108), 
('iphone', 0.7429664134979248), 
('microsoft', 0.7334205508232117), 
('ipad', 0.7331036329269409)
]
{% endhighlight %}

Each of these words has strong relevance to the company.

# Averaging Word Vectors

Another interesting operation is averaging word vectors. This allows us to combine the meaning of two words into a 
single vector. For instance, we could average the vectors for "apple" and "orange" to get a vector that represents 
something like "fruit."

{% highlight python %}
# Average of 'apple' and 'orange'
vector_fruit_avg = (model['apple'] + model['orange']) / 2

# Find the words closest to the average vector
similar_to_fruit_avg = model.similar_by_vector(vector_fruit_avg, topn=5)
print("Words similar to the average of 'apple' and 'orange':", similar_to_fruit_avg)
{% endhighlight %}

There are a number of related words to both "apple" and "orange". The average provides us with this intersection.

{% highlight plain %}
Words similar to the average of 'apple' and 'orange': [
('apple', 0.8868993520736694), 
('orange', 0.8670367002487183), 
('juice', 0.7459520101547241), 
('cherry', 0.7071465849876404), 
('cream', 0.7013142704963684)
]
{% endhighlight %}

# Conclusion

Word embeddings are a powerful way to represent the meaning of words as vectors in a high-dimensional space. By using 
simple mathematical operations, such as vector arithmetic and cosine similarity, we can uncover a variety of semantic 
relationships between words. These operations allow embeddings to be used in tasks such as analogy generation, search, 
and clustering.

In this post, we explored how to use pre-trained word embeddings, perform vector operations, and leverage them for 
real-world tasks. These foundational concepts are what power much of the magic behind modern NLP techniques, from search 
engines to chatbots and more.
