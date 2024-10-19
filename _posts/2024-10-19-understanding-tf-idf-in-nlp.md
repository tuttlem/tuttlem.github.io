---
layout: post
title: Understanding TF-IDF in NLP
date: 2024-10-19
comments: false
categories: [ "td-idf", "nlp", "text" ]
---

# Introduction

In our [previous post]({% post_url 2024-10-19-turning-words-into-vectors %}), we introduced **One-Hot Encoding** and 
the **Bag-of-Words (BoW)** model, which are simple methods of representing text as numerical vectors. While these 
techniques are foundational, they come with certain limitations. One major drawback of Bag-of-Words is that it treats 
all words equally—common words like "the" or "is" are given the same importance as more meaningful words like 
"science" or "NLP."

**TF-IDF (Term Frequency-Inverse Document Frequency)** is an extension of BoW that aims to address this problem. By 
weighting words based on their frequency in individual documents versus the entire corpus, TF-IDF highlights more 
important words and reduces the impact of common, less meaningful ones.

# TF-IDF

TF-IDF stands for **Term Frequency-Inverse Document Frequency**. It’s a numerical statistic used to reflect the 
importance of a word in a document relative to a collection of documents (a corpus). The formula is:

$$ \text{TF-IDF}(t, d) = \text{TF}(t, d) \times \text{IDF}(t) $$

Where:
- $$ \text{TF}(t, d) $$: Term Frequency of term $$ t $$ in document $$ d $$, which is the number of times $$ t $$ appears in $$ d $$.
- $$ \text{IDF}(t) $$: Inverse Document Frequency, which measures how important $$ t $$ is across the entire corpus.

## Term Frequency (TF)

**Term Frequency (TF)** is simply a count of how frequently a term appears in a document. The higher the frequency, the 
more relevant the word is assumed to be for that specific document.

$$ \text{TF}(t, d) = \frac{\text{Number of occurrences of } t \text{ in } d}{\text{Total number of terms in } d} $$

For example, if the word "NLP" appears 3 times in a document of 100 words, the term frequency for "NLP" is:

$$ \text{TF}(NLP, d) = \frac{3}{100} = 0.03 $$

## Inverse Document Frequency (IDF)

**Inverse Document Frequency (IDF)** downweights common words that appear in many documents and upweights rare words 
that are more meaningful in specific contexts. The formula is:

$$ \text{IDF}(t) = \log\left(\frac{N}{1 + \text{DF}(t)}\right) $$

Where:
- $$ N $$ is the total number of documents in the corpus.
- $$ \text{DF}(t) $$ is the number of documents that contain the term $$ t $$.

The "+1" in the denominator is there to avoid division by zero. Words that appear in many documents (e.g., "is", "the") 
will have a lower IDF score, while rare terms will have higher IDF scores.

## Example 

Let’s take an example with two documents:

1. **Document 1**: "I love NLP and NLP loves me"
2. **Document 2**: "NLP is great and I enjoy learning NLP"

The combined vocabulary is:

{% highlight plain %}
["I", "love", "NLP", "and", "loves", "me", "is", "great", "enjoy", "learning"]
{% endhighlight %}

For simplicity, let’s calculate the TF and IDF for the term **"NLP"**.

- **TF for "NLP" in Document 1**: The term "NLP" appears twice in Document 1, which has 7 words total, so:


$$ \text{TF}(NLP, d_1) = \frac{2}{7} \approx 0.286 $$

- **TF for "NLP" in Document 2**: The term "NLP" appears twice in Document 2, which has 8 words total, so:

$$ \text{TF}(NLP, d_2) = \frac{2}{8} = 0.25 $$

Now, let’s calculate the IDF for "NLP". Since "NLP" appears in both documents (2 out of 2 documents), the IDF is:

$$ \text{IDF}(NLP) = \log\left(\frac{2}{1 + 2}\right) = \log\left(\frac{2}{3}\right) \approx -0.176 $$

The negative value here shows that "NLP" is a very common term in this corpus, and its weight will be downscaled.

## Code Example: TF-IDF with `TfidfVectorizer`

Now let’s use `TfidfVectorizer` from `sklearn` to automatically calculate TF-IDF scores for our documents.

{% highlight python %}
from sklearn.feature_extraction.text import TfidfVectorizer

# Define a corpus of documents
corpus = [
    "I love NLP and NLP loves me",
    "NLP is great and I enjoy learning NLP"
]

# Initialize the TF-IDF vectorizer
vectorizer = TfidfVectorizer()
# Fit and transform the corpus into TF-IDF vectors
X = vectorizer.fit_transform(corpus)

# Display the feature names (vocabulary)
print(vectorizer.get_feature_names_out())

# Display the TF-IDF matrix
print(X.toarray())
{% endhighlight %}

The output of this is:

{% highlight plain %}
['and' 'enjoy' 'great' 'is' 'learning' 'love' 'loves' 'me' 'nlp']
[[0.30253071 0.         0.         0.         0.         0.42519636 0.42519636 0.42519636 0.60506143]
 [0.27840869 0.39129369 0.39129369 0.39129369 0.39129369 0.         0.         0.         0.55681737]]
{% endhighlight %}

Each row in the output corresponds to a document, and each column corresponds to a term in the vocabulary. The values 
represent the TF-IDF score of each term for each document.

# Advantages of TF-IDF

1. **Balances Frequency**: TF-IDF considers both how frequently a word appears in a document (term frequency) and how unique or common it is across all documents (inverse document frequency). This helps prioritize meaningful words.
2. **Reduces Impact of Stop Words**: By downweighting terms that appear in many documents, TF-IDF naturally handles common stop words without needing to remove them manually.
3. **Efficient for Large Corpora**: TF-IDF is computationally efficient and scales well to large datasets.

# Limitations of TF-IDF

While TF-IDF is a significant improvement over simple Bag-of-Words, it still has some limitations:

1. **No Semantic Meaning**: Like Bag-of-Words, TF-IDF treats words as independent features and doesn't capture the relationships or meaning between them.
2. **Sparse Representations**: Even with the IDF weighting, TF-IDF still generates high-dimensional and sparse vectors, especially for large vocabularies.
3. **Ignores Word Order**: TF-IDF doesn’t account for word order, so sentences with the same words in different arrangements will have the same representation.

# Conclusion

TF-IDF is a powerful and widely-used method for text representation, especially in tasks like document retrieval and 
search engines, where distinguishing between important and common words is crucial. However, as we’ve seen, TF-IDF 
doesn’t capture the meaning or relationships between words, which is where word embeddings come into play.
