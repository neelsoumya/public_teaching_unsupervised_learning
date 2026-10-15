# From Unsupervised Learning to LLM Embeddings

## The central idea

An LLM does not normally store a word as a simple, human-readable label such as *animal*, *Australian*, or *unusual*. It converts text into tokens, maps token IDs to learned numerical vectors, and processes those vectors through transformer layers. The resulting representations encode statistical regularities that help the model predict and interpret text.

> **Teaching takeaway:** hidden dimensions are coordinates in a learned representation space—not a list of neatly labelled concepts.

## 1. Start with a sentence

Consider:

> “An aardvark and an echidna are at the zoo, and they are unusual animals.”

A tokenizer splits text into tokens. A token may be a whole word, part of a word, punctuation, or another text unit. Each token is assigned an integer ID.

The IDs are identifiers, not meaningful measurements: token 500 is not inherently “more animal-like” than token 200.

## 2. One-hot vectors versus embeddings

A one-hot representation has one position for each item in the vocabulary. Almost every entry is zero, and the entry for the chosen item is one. It identifies a token but does not, by itself, express similarity between tokens.

An embedding lookup maps the token ID to a dense vector, for example:

`aardvark → [0.12, -0.07, 0.31, ..., 0.04]`

The numbers are learned during training. Two tokens used in similar contexts may acquire related representations, although similarity is not guaranteed to correspond to a single intuitive human category.

## 3. What do the hidden dimensions encode?

A vector might have hundreds or thousands of coordinates. It is tempting to say that one coordinate means “animal”, another means “Australian”, and another means “strange”. Usually, that is too simple.

- **Distributed representation:** information is typically spread across many coordinates.
- **Mixed features:** one coordinate can contribute to several patterns, and one concept can depend on many coordinates.
- **Learned geometry:** directions and distances can capture useful statistical relationships.
- **Context matters:** the representation used by later transformer layers changes with surrounding words. “Jaguar” in a wildlife article and “Jaguar” in a car review can be interpreted differently.

A dimension can sometimes be associated with a feature through careful analysis, but a dimension should not automatically be treated as a single, stable, human-readable concept.

## 4. A linear view

A simple linear unit combines inputs using weights:

\[
z = \phi_1 x_1 + \phi_2 x_2 + \cdots + \phi_n x_n + b
\]

Here, the \(x_i\) values are input features, the \(\phi_i\) values are learned weights, and \(b\) is a bias. This illustrates how multiple numerical signals can contribute to a result. Neural networks build on this idea using many learned transformations and nonlinearities.

The equation is a useful intuition, but it is not a complete description of an LLM: transformer blocks also use attention, feed-forward networks, residual connections, normalization, and other operations.

## 5. Where unsupervised learning fits

Modern LLMs are commonly pretrained with **self-supervised learning**. The training text itself supplies the learning signal—for example, the model learns to predict the next token from preceding tokens. No person needs to assign a category label to every word.

The model adjusts its parameters to reduce prediction error. In doing so, it can learn representations that reflect syntax, semantic associations, topic, style, and other regularities in the training data. This is often introduced alongside unsupervised learning, although *self-supervised learning* is the more precise term for next-token prediction.

## 6. From token embeddings to contextual representations

The initial embedding gives each token a learned vector. Transformer layers then update representations using surrounding context. As a result, the representation of a token is not just a fixed dictionary entry: later-layer representations depend on the sentence and the other tokens.

For example, the words *aardvark* and *echidna* may share contextual patterns related to animals, but the model need not dedicate one coordinate exclusively to “animal”. Their useful information can be distributed across the vector and transformed by subsequent layers.

## 7. Important correction about dimensions

Avoid presenting “GPT-3 has 16,000 dimensions” as a general fact. GPT-3 model sizes have different configurations; the original 175-billion-parameter GPT-3 model is commonly reported with a 12,288-wide hidden representation. Other models have different widths, and the embedding size is not a universal property of LLMs.

## Discussion questions

1. If a vector has 1,000 dimensions, does that mean the model has learned exactly 1,000 concepts?
2. Why might two words used in similar contexts have related embeddings?
3. Why can the representation of a word change when its surrounding sentence changes?
4. What evidence would you need before claiming that one hidden dimension represents “animalness”?
5. How is predicting the next token a learning signal even when the text has no manually supplied labels?

## Summary

**Text → tokens → token IDs → learned embeddings → contextual transformer representations → predictions.**

Unsupervised and self-supervised learning allow a model to discover useful structure from data. The hidden vectors encode that structure in distributed numerical form; they are not generally a transparent dictionary of individually labelled concepts.
