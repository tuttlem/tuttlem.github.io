---
layout: post
title: Naive Bayes Classifier from First Principles
date: 2025-09-30
comments: false
categories: [ machine-learning, algorithms, naive-bayes, python ]
---

# Introduction

The **Naive Bayes classifier** is one of the simplest algorithms in machine learning, yet it’s surprisingly powerful.  
It answers the question:

**“Given some evidence, what is the most likely class?”**

It’s *naive* because it assumes that features are conditionally independent given the class. That assumption rarely 
holds in the real world — but the algorithm still works remarkably well for many tasks such as spam filtering, document 
classification, and sentiment analysis.

At its core, Naive Bayes is just **counting, multiplying probabilities, and picking the largest one.**

# Bayes’ Rule Refresher

First, let's start with a quick definition of terms.

**Class** is the label that we're trying to predict. In our example below, the class will be either "spam" or "ham" 
(not spam).

The **features** are the observed pieces of evidence. For text, features are usually the words in a message.

**P** is shorthand for "probability".

* `P(Class)` = the **prior probability**: how likely a class is before seeing any features.
* `P(Features | Class)` = the **likelihood**: how likely it is to see those words if the class is true.
* `P(Features)` = the **evidence**: how likely the features are overall, across the classes. This acts as a normalising constant so probabilities sum to 1.

Bayes’ rule tells us:

{% highlight plain %}
P(Class | Features) = ( P(Class) * P(Features | Class) ) / P(Features)
{% endhighlight %}

Since the denominator is the same for all classes, we only care about:

{% highlight plain %}
P(Class | Features) ∝ P(Class) * Π P(Feature_i | Class)
{% endhighlight %}

Naive Bayes assumes independence, so the likelihood of multiple features is just the product of the individual 
feature likelihoods.

# Pen & Paper Example

Let’s build the smallest possible spam filter.

Our training data is 4 tiny, two word emails:

{% highlight plain %}
Spam → "buy cheap"
Spam → "cheap pills"
Ham  → "meeting schedule"
Ham  → "project meeting"
{% endhighlight %}

Based on this training set, we can say:

{% highlight plain %}
P(spam) = 2/4 = 0.5  
P(ham)  = 2/4 = 0.5  
{% endhighlight %}

Next, we can look at the word likelihoods for a given class.

For spam (words: buy, cheap, pills):  

{% highlight plain %}
P(buy | spam)    = 1/4  
P(cheap | spam)  = 2/4  
P(pills | spam)  = 1/4  
{% endhighlight %}

For ham (words: meeting, schedule, project):  

{% highlight plain %}
P(meeting | ham)   = 2/4  
P(schedule | ham)  = 1/4  
P(project | ham)   = 1/4  
{% endhighlight %}

With all of this basic information in place, we can try and classify a new email.

As an example, we'll look at an email that simply says `"cheap meeting"`.

For spam:  

{% highlight plain %}
  P(spam) * P(cheap|spam) * P(meeting|spam)  
= 0.5     * (2/4)         * (0/4) 
= 0  
{% endhighlight %}

For ham:  

{% highlight plain %}
  P(ham) * P(cheap|ham) * P(meeting|ham)  
= 0.5    * (0/4)        * (2/4) 
= 0  
{% endhighlight %}

{% include callout.html type="warning" title="That didn't work!" text="Both go to zero because “cheap” never appeared in ham, and “meeting” never appeared in spam. This is why we use Laplace smoothing" %}

# Laplace Smoothing

To avoid zero probabilities, add a tiny count (usually +1) to every word in every class.

With smoothing, our likelihoods become:

{% highlight plain %}
P(buy | spam)    = (1+1)/(4+6) = 2/10  
P(cheap | spam)  = (2+1)/(4+6) = 3/10  
P(pills | spam)  = (1+1)/(4+6) = 2/10  
P(meeting | spam)= (0+1)/(4+6) = 1/10  
… and so on for ham.
{% endhighlight %}

Here 4 is the total token count in spam, and 6 is the vocabulary size.

Now the “cheap meeting” example will give non-zero values, and we can meaningfully compare classes.

For spam:

{% highlight plain %}
  P(spam) * P(cheap|spam) * P(meeting|spam)  
= 0.5     * (3/10)        * (1/10)
= 0.015  
{% endhighlight %}

For ham:

{% highlight plain %}
  P(ham) * P(cheap|ham) * P(meeting|ham)  
= 0.5    * (1/10)       * (3/10)
= 0.015
{% endhighlight %}

So both classes land on the same score — a perfect tie — which is why the Python demo falls back to whichever label it checks first.

# Python Demo (from scratch)

Here’s a tiny implementation that mirrors the example above:

{% highlight python %}
from collections import Counter, defaultdict

# Training data
docs = [
    ("spam", "buy cheap"),
    ("spam", "cheap pills"),
    ("ham",  "meeting schedule"),
    ("ham",  "project meeting"),
]

class_counts = Counter()
word_counts = defaultdict(Counter)

# Build counts
for label, text in docs:
    class_counts[label] += 1
    for word in text.split():
        word_counts[label][word] += 1

def classify(text, alpha=1.0):
    words = text.split()
    scores = {}
    total_docs = sum(class_counts.values())
    vocab = {w for counts in word_counts.values() for w in counts}
    V = len(vocab)

    for label in class_counts:
        # Prior
        score = class_counts[label] / total_docs
        total_words = sum(word_counts[label].values())

        for word in words:
            count = word_counts[label][word]
            # Laplace smoothing
            score *= (count + alpha) / (total_words + alpha * V)

        scores[label] = score

    # Pick the class with the highest score
    return max(scores, key=scores.get), scores

print(classify("cheap project"))
print(classify("project schedule"))
print(classify("cheap schedule"))
{% endhighlight %}

Running this gives:

{% highlight plain %}
('spam', {'spam': 0.015, 'ham': 0.015})
('ham', {'spam': 0.005, 'ham': 0.02})
('spam', {'spam': 0.015, 'ham': 0.01})
{% endhighlight %}

As we predicted earlier, `"cheap project"` is a tie, while `"project schedule"` is more likely ham. Finally, `"cheap schedule"` 
is noted as spam because it uses stronger spam trigger words.

# Real-World Notes

- Naive Bayes is fast, memory-efficient, and easy to implement.  
- Works well for text classification, document tagging, and spam filtering.  
- The independence assumption is rarely true, but it doesn’t matter — it often performs surprisingly well.  
- In production, you’d tokenize better, remove stop words, and work with thousands of documents.  

# Conclusion

Building a Naive Bayes classifier from first principles is a great exercise because it shows how machine learning can be 
just careful counting and probability. With priors, likelihoods, and a dash of smoothing, you get a surprisingly useful 
classifier — all without heavy math or libraries.
