---
layout: post
title: Computers Understanding Text
date: 2024-10-19
comments: false
categories: [ "" ]
---

# Introduction

When working with Natural Language Processing (NLP), one of the first challenges you encounter is how to convert 
human-readable text into a format that machines can understand. Computers don't natively understand words or sentences; 
they operate in numbers. 

So, how do we get from words to something a machine can process?

This is where **text preprocessing** comes in. 

Text preprocessing involves several steps to prepare raw text for analysis. In this post, we'll walk through the 
foundational techniques in preprocessing: **tokenization**, **lowercasing**, **removing stop words**, and 
**stemming/lemmatization**. These steps ensure that our text is in a clean, structured format for further processing 
like word embeddings or more complex NLP models.

# Tokenization: Breaking Down the Text

## What is Tokenization?

Tokenization is the process of breaking a string of text into smaller pieces, usually words or subwords. In essence, 
it's the process of splitting sentences into tokens, which are the basic units for further NLP tasks.

For example, consider the sentence:

{% highlight plain %}
"I love NLP!"
{% endhighlight %}

Tokenization would break this into:

{% highlight plain %}
["I", "love", "NLP", "!"]
{% endhighlight %}

This is a simple example where each token corresponds to a word or punctuation. However, tokenization can get more 
complex depending on the language and the task. For instance, some tokenizers split contractions like "can't" into 
`["can", "'t"]`, while others might treat it as one token. Tokenization also becomes more challenging in languages that 
don't have spaces between words, like Chinese or Japanese.

## Code Example: Tokenization in Python

Let’s look at a basic example of tokenization using Python’s `nltk` library:

{% highlight python %}
import nltk
nltk.download('punkt_tab')
from nltk.tokenize import word_tokenize

sentence = "I love NLP!"
tokens = word_tokenize(sentence)
print(tokens)
{% endhighlight %}

The output you can see is simply:

{% highlight plain %}
['I', 'love', 'NLP', '!']
{% endhighlight %}

## Code Example: Sentence Tokenization

Tokenization can also occur at the sentence level, which means breaking down a paragraph or a larger body of text into 
individual sentences. This is helpful for tasks like summarization or sentiment analysis, where sentence boundaries 
matter.

{% highlight python %}
from nltk.tokenize import sent_tokenize

text = "NLP is fun. It's amazing how machines can understand text!"
sentences = sent_tokenize(text)
print(sentences)
{% endhighlight %}

The output is now on the sentence boundary:

{% highlight plain %}
['NLP is fun.', "It's amazing how machines can understand text!"]
{% endhighlight %}

# Lowercasing: Making Text Uniform

In English, the words "Dog" and "dog" mean the same thing, but to a computer, they are two different tokens. 
Lowercasing is a simple yet powerful step in text preprocessing. By converting everything to lowercase, we reduce the 
complexity of the text and ensure that words like "NLP" and "nlp" are treated identically.

We can achieve this simple with the `.lower()` method off of a string.

This step becomes crucial when dealing with large text corpora, as it avoids treating different capitalizations of the 
same word as distinct entities.

# Removing Stop Words: Filtering Out Common Words

Stop words are commonly used words that don’t carry significant meaning in many tasks, such as "and", "the", and "is". 
Removing stop words helps reduce noise in the data and improves the efficiency of downstream models by focusing only on 
the meaningful parts of the text.

Many libraries provide lists of stop words, but the ideal list can vary depending on the task.

{% highlight python %}
from nltk.corpus import stopwords

stop_words = set(stopwords.words('english'))
tokens = ["I", "love", "NLP", "!"]
filtered_tokens = [word for word in tokens if word.lower() not in stop_words]
print(filtered_tokens)
{% endhighlight %}

Only the value words remain, removing the "I":

{% highlight plain %}
["love", "NLP", "!"]
{% endhighlight %}

# Stemming and Lemmatization: Reducing Words to Their Root Forms

Another key step in preprocessing is reducing words to their base or root form. There are two common approaches:

Stemming: This cuts off word endings to get to the base form, which can sometimes be rough. For example, "running", 
"runner", and "ran" might all be reduced to "run".

Lemmatization: This is a more refined process that looks at the word's context and reduces it to its dictionary form. 
For instance, "better" would be lemmatized to "good".

Here’s an example using nltk for both stemming and lemmatization:

{% highlight python %}
from nltk.stem import PorterStemmer

ps = PorterStemmer()
words = ["running", "runner", "ran"]
stemmed_words = [ps.stem(word) for word in words]
print(stemmed_words)
{% endhighlight %}

The output here:

{% highlight plain %}
['run', 'runner', 'ran']
{% endhighlight %}

An example of Lemmatization looks like this:

{% highlight python %}
from nltk.stem import WordNetLemmatizer
nltk.download('wordnet')

lemmatizer = WordNetLemmatizer()
words = ["running", "better", "ran"]
lemmatized_words = [lemmatizer.lemmatize(word, pos="v") for word in words]
print(lemmatized_words)
{% endhighlight %}

{% highlight plain %}
['run', 'good', 'run']
{% endhighlight %}

# Conclusion

Text preprocessing is a crucial first step in any NLP project. By breaking down text through tokenization, making it 
uniform with lowercasing, and reducing unnecessary noise with stop word removal and stemming/lemmatization, we can 
create a clean and structured dataset for further analysis or model training. These steps form the foundation upon which 
more advanced techniques, such as word embeddings and machine learning models, are built.
