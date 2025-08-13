---
layout: post
title: Understanding the Transformer Architecture
date: 2025-08-13
comments: false
categories: [ ml, llm, ai, gpt, python ]
---


# Introduction

Natural language processing (NLP) has gone through several paradigm shifts:

- **Bag-of-Words** — treated text as unordered word counts; no sequence information. We've spoken about this [previously]({% post_url 2024-10-19-turning-words-into-vectors %}).
- **Word Embeddings** (word2vec, GloVe) — learned fixed-vector representations that captured meaning. We've looked at these [previously]({% post_url 2024-10-18-word-embeddings %}).
- **RNNs, LSTMs, GRUs** — processed sequences token-by-token, retaining a hidden state; struggled with long-range dependencies due to vanishing gradients.
- **Seq2Seq with Attention** — attention helped the model “focus” on relevant input tokens; a leap in translation and summarization.
- **Transformers** (*Vaswani et al., 2017 — "Attention Is All You Need"*) — replaced recurrence entirely with *self-attention*, allowing parallelization and longer context handling.

Transformers didn’t just improve accuracy; they unlocked the ability to scale models massively.

In this post, we’ll walk though an understanding of the transformer architecture by **implementing a GPT-style 
Transformer from scratch in PyTorch**, from tokenization to text generation.  

The goal: make the architecture concrete and understandable, not magical.

# Overview

At a high level, our model will:

1. Tokenize text into integers.
2. Map tokens to dense embeddings + positional encodings.
3. Apply *self-attention* to mix contextual information.
4. Use feed-forward networks for per-token transformations.
5. Wrap attention + FFN in *Transformer Blocks* with residual connections and layer normalization.
6. Project back to vocabulary logits.
7. Generate text autoregressively.

<div class="mermaid">
graph TD
  A[Text Input] --> B[Tokenizer]
  B --> C[Token Embeddings + Positional Encoding]
  C --> D[Transformer Block × N]
  D --> E[Linear Projection to Vocabulary Size]
  E --> F[Softmax Probabilities]
  F --> G[Sample / Argmax Next Token]
  G -->|Loop| C
</div>

## Tokenization

Before our model can process text, we need to turn characters into numbers it can work with — a process called 
tokenization. In this example, we use a simple byte-level tokenizer, which treats every UTF-8 byte as its own token. 
This keeps the implementation minimal while still being able to represent any possible text without building a custom 
vocabulary.

{% highlight python %}
class ByteTokenizer:
    """
    UTF-8 bytes <-> ints in [0..255].
    NOTE: For production models you'd use a subword tokenizer (BPE, SentencePiece).
    """
    def __init__(self) -> None:
        self.vocab_size = 256

    def encode(self, text: str) -> list[int]:
        return list(text.encode("utf-8"))

    def decode(self, ids: list[int]) -> str:
        return bytes(ids).decode("utf-8", errors="ignore")
{% endhighlight %}

Example:

{% highlight python %}
tok = ByteTokenizer()
ids = tok.encode("Hello")
print(ids)        # [72, 101, 108, 108, 111]
print(tok.decode(ids))  # "Hello"
{% endhighlight %}

## Embeddings & Positional Encoding

Once we have token IDs, we map them into **embedding vectors** — learned dense representations that capture meaning in 
a continuous space. Each token ID indexes a row in an embedding matrix, turning a discrete integer into a trainable 
vector of size $$ d_{\text{model}} $$. Because self-attention alone has no sense of order, we also add 
**positional embeddings**, giving the model information about each token’s position within the sequence.

{% highlight python %}
self.tok_emb = nn.Embedding(vocab_size, d_model)   # token embeddings
self.pos_emb = nn.Embedding(block_size, d_model)   # positional embeddings
{% endhighlight %}

## Self-Attention

Self-attention lets each token *attend* to all previous tokens (causally masked to prevent peeking ahead).

Mathematically:

$$
\text{Attention}(Q, K, V) = \text{softmax}\left(\frac{QK^\top}{\sqrt{d_k}}\right) V
$$

That equation means each token computes a similarity score with all other tokens (via $$ QK^\top $$), scales it 
by $$ \sqrt{d_k} $$ to stabilize gradients, turns the scores into probabilities with softmax, and then uses those 
probabilities to take a weighted sum of the value vectors $$ V $$  to produce its new representation.

Multi-head attention runs this in parallel on different projections.

{% highlight python %}
class MultiHeadSelfAttention(nn.Module):
    def __init__(self, d_model, n_heads, block_size, dropout):
        super().__init__()
        assert d_model % n_heads == 0
        self.n_heads = n_heads
        self.head_dim = d_model // n_heads
        self.qkv = nn.Linear(d_model, 3 * d_model, bias=False)
        self.out_proj = nn.Linear(d_model, d_model, bias=False)
        self.attn_drop = nn.Dropout(dropout)
        self.resid_drop = nn.Dropout(dropout)
        mask = torch.tril(torch.ones(block_size, block_size, dtype=torch.bool))
        self.register_buffer("causal_mask", mask)

    def forward(self, x):
        B, T, C = x.shape
        qkv = self.qkv(x)
        q, k, v = qkv.chunk(3, dim=-1)
        def split_heads(t): return t.view(B, T, self.n_heads, self.head_dim).transpose(1, 2)
        q, k, v = split_heads(q), split_heads(k), split_heads(v)
        scores = (q @ k.transpose(-2, -1)) / math.sqrt(self.head_dim)
        scores = scores.masked_fill(~self.causal_mask[:T, :T], float("-inf"))
        att = F.softmax(scores, dim=-1)
        att = self.attn_drop(att)
        y = att @ v
        y = y.transpose(1, 2).contiguous().view(B, T, C)
        y = self.out_proj(y)
        y = self.resid_drop(y)
        return y
{% endhighlight %}

## Feed-Forward Network

A per-token MLP, applied identically at each position.

{% highlight python %}
class FeedForward(nn.Module):
    def __init__(self, d_model, mult=4, dropout=0.0):
        super().__init__()
        self.net = nn.Sequential(
            nn.Linear(d_model, mult * d_model),
            nn.GELU(),
            nn.Linear(mult * d_model, d_model),
            nn.Dropout(dropout),
        )

    def forward(self, x):
        return self.net(x)
{% endhighlight %}

This tiny two-layer neural network can be broken down as follows:

- **Input:** token embedding vector (size $$ d_{\text{model}} $$).
- **Linear layer:** expands to $$ \text{mult} \times d_{\text{model}} $$.
- **GELU activation:** introduces non-linearity.
- **Linear layer:** projects back to $$ d_{\text{model}} $$.
- **Dropout:** randomly zeroes some activations during training for regularization.

## Transformer Block

A Transformer block applies pre-layer normalization, then runs the data through either a multi-head self-attention 
layer or a feed-forward network (FFN), and adds a residual connection after each. This structure is stacked multiple 
times to deepen the model.

<div class="mermaid">
graph TD
  A[Input] --> B[LayerNorm]
  B --> C[Multi-Head Self-Attention]
  C --> D[Residual Add]
  D --> E[LayerNorm]
  E --> F[Feed-Forward Network]
  F --> G[Residual Add]
  G --> H[Output to Next Block]
</div>

{% highlight python %}
class TransformerBlock(nn.Module):
    def __init__(self, d_model, n_heads, block_size, dropout):
        super().__init__()
        self.ln1 = nn.LayerNorm(d_model)
        self.ln2 = nn.LayerNorm(d_model)
        self.attn = MultiHeadSelfAttention(d_model, n_heads, block_size, dropout)
        self.ffn  = FeedForward(d_model, mult=4, dropout=dropout)

    def forward(self, x):
        x = x + self.attn(self.ln1(x))
        x = x + self.ffn(self.ln2(x))
        return x
{% endhighlight %}

## GPT-Style Model Head & Loss

After token and position embeddings are summed, the data flows through a **stack of Transformer blocks**, each applying 
self-attention and a feed-forward transformation with residual connections.  
Once all blocks have run, we apply a **final LayerNorm** to normalize the hidden state vectors and keep training stable.  

From there, each token’s hidden vector is **projected back into vocabulary space** — producing a vector of raw 
scores (*logits*) for each possible token in the vocabulary.  

We also use **weight tying** here: the projection matrix for mapping hidden vectors to logits is **the same matrix as 
the token embedding layer’s weights**.  
This reduces the number of parameters, ensures a consistent mapping between tokens and embeddings, and has been shown 
to improve generalization.  

Mathematically, weight tying can be expressed as:

$$\text{logits} = H \cdot E^\top$$

where $$ H $$ is the matrix of hidden states from the final Transformer layer, and $$ E $$ is the embedding matrix 
from the input token embedding layer. This means the output projection reuses (shares) the same weights as the input 
embedding, just transposed.

{% highlight python %}
class TinyGPT(nn.Module):
    def __init__(self, vocab_size, d_model=128, n_layers=2, n_heads=4, block_size=64, dropout=0.1):
        super().__init__()
        self.block_size = block_size
        self.tok_emb = nn.Embedding(vocab_size, d_model)
        self.pos_emb = nn.Embedding(block_size, d_model)
        self.drop = nn.Dropout(dropout)
        self.blocks = nn.ModuleList([
            TransformerBlock(d_model, n_heads, block_size, dropout)
            for _ in range(n_layers)
        ])
        self.ln_f = nn.LayerNorm(d_model)
        self.head = nn.Linear(d_model, vocab_size, bias=False)
        self.head.weight = self.tok_emb.weight
        self.apply(self._init_weights)

    def _init_weights(self, m):
        if isinstance(m, nn.Linear):
            nn.init.normal_(m.weight, mean=0.0, std=0.02)
            if m.bias is not None: nn.init.zeros_(m.bias)
        elif isinstance(m, nn.Embedding):
            nn.init.normal_(m.weight, mean=0.0, std=0.02)

    def forward(self, idx, targets=None):
        B, T = idx.shape
        assert T <= self.block_size
        tok = self.tok_emb(idx)
        pos = self.pos_emb(torch.arange(T, device=idx.device))
        x = self.drop(tok + pos)
        for blk in self.blocks:
            x = blk(x)
        x = self.ln_f(x)
        logits = self.head(x)
        loss = None
        if targets is not None:
            loss = F.cross_entropy(
                logits.view(B * T, -1),
                targets.view(B * T)
            )
        return logits, loss
{% endhighlight %}

## Generation Loop

This method performs **autoregressive text generation**: we start with some initial tokens, repeatedly predict the 
next token, append it, and feed the result back into the model.

Key concepts:
- **Autoregressive**: generation proceeds one token at a time, conditioning on all tokens so far.
- **Temperature**: scales the logits before softmax; values < 1.0 make predictions sharper/more confident, > 1.0 make them more random.
- **Top-k filtering**: keeps only the k highest-probability tokens and sets all others to negative infinity before sampling, which limits randomness to plausible options.

Step-by-step in `generate()`:
1. **Crop context**: keep only the last `block_size` tokens to match the model’s maximum context window.
2. **Forward pass**: get logits for each position in the sequence.
3. **Select last step’s logits**: we only want the prediction for the next token.
4. **Adjust for temperature** (optional).
5. **Apply top-k filtering** (optional).
6. **Softmax**: convert logits into a probability distribution.
7. **Sample**: randomly choose the next token according to the probabilities.
8. **Append**: add the new token to the sequence and repeat.

This loop continues until `max_new_tokens` tokens have been generated.

{% highlight python %}
@torch.no_grad()
def generate(self, idx, max_new_tokens, temperature=1.0, top_k=None):
    for _ in range(max_new_tokens):
        idx_cond = idx[:, -self.block_size:]
        logits, _ = self(idx_cond)
        logits = logits[:, -1, :]
        if temperature != 1.0:
            logits = logits / temperature
        if top_k is not None:
            v, _ = torch.topk(logits, top_k)
            thresh = v[:, [-1]]
            logits = torch.where(logits < thresh, torch.full_like(logits, float("-inf")), logits)
        probs = F.softmax(logits, dim=-1)
        next_id = torch.multinomial(probs, num_samples=1)
        idx = torch.cat([idx, next_id], dim=1)
    return idx
{% endhighlight %}

# In Practice

That concludes the entire stack that we need. We can start to ask questions of this very basic model. Just remember, 
this is a tiny model so results are not going to be amazing, but it will give you a sense of how these tokens are 
generated.

After training briefly on a small excerpt of *Moby Dick* plus a few Q/A lines, we can get:

{% highlight plain %}
Q: Why does he go to sea?
A: To drive off the spleen and regulate the circulation.
{% endhighlight %}

Even a tiny model learns local structure.

# Conclusion

Even though this isn't the perfect model that will challenge all of the big guys, I hope this has been a bit of a step 
by step walkthough on how the transformer architecture is put together. 

A full version of the code referenced in this article can be found [here](https://gist.github.com/tuttlem/1c848af07c79992e0517cbd24bd3873e). 
The code here includes the training loop so you can run it end-to-end.
