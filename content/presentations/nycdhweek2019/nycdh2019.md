---
title: Does "late style" exist? 
author: Jonathan Reeve
---

# Does "late style" exist?

---

Jonathan Reeve  
Literary Modeling and Visualization Lab  
Group for Experimental Methods in the Humanities  
Columbia University  

# The Problems / Hypotheses

## Edward Said: _On Late Style_

![Cover of _On Late Style_](images/on-late-style.jpg){width=35%}

## Characteristics

> "The maturity of the late works of significant artists does not resemble the kind one finds in fruit. They are, for the most part, not round, but furrowed, even ravaged. Devoid of sweetness, bitter and spiny, they do not surrender themselves to mere delectation." (Adorno, quoted in Said)
 - Beethoven's Ninth Symphony

# Operationalizing

## Stylometry

 - Quantifies textual stylistic differences
 - Well-studied in authorship attribution

## Document Embeddings

 - Averaged word embeddings
 - Encode semantic information about documents
 - Test "style as content" hypothesis
 
# Corpus Creation

## Corpus-DB

 - Machine-generated using [Corpus-DB.org](http://corpus-db.org)
 - 51 writers
   - ~900 total works

## Query

```sql
SELECT text FROM gutenberg 
WHERE count(text) > 8 
AND lang = 'en' AND genre = 'fiction'
```

# Method

## Stylometry++

 - Random sampling of documents
 - Document-term matrices of 800 MFW
 - Reduced to 5D using PCA
 - Modeled using Bayesian Gaussian Mixture model
   - 1-3 possible "periods" or clusters
 - 20 trials, averaged
 - ~12 hour laptop computation

## Metrics

 - "Lateness" 
   - Of a work: L2 norm of the 5D document vector
   - Of a writer: 
     - Difference of Euclidean L2 norms of centroids of doc. vectors 
 - "Periodicity"
   - Adjusted Rand Index comparing BGM clusters with initial date-based 1D-clustering
     - Category-agnostic mutual information score
     
# Results

## Marcel Proust

![Marcel Proust](images/proust.png)

## Mary Augusta Ward

![Mary Augusta Ward (Mrs. Humpry Ward)](images/ward.png)

## Henry James

![Henry James](images/james.png)

## Charles Dickens

![Charles Dickens](images/dickens.png)

## Willa Cather

![Willa Cather](images/cather.png)

## Periodicities

| Author    | Periodicity Score |
|-----------|-------------------|
| James     |             0.472 | 
| Dickens   |             0.469 |
| Genet     |             0.457 |
| Mann      |             0.367 |
| Conrad    |             0.177 |
| Cather    |             0.177 |
| Ward      |             0.166 |
| Proust    |             0.023 |

---

![Mean Latenesses, Sorted](images/latenesses.png)

## Mean Lateness, All

-0.0171  
(Early style, not late style.)  

## Mean Periodicities

![Mean Periodicies, Sorted](images/periodicities.png)

## Mean Latenesses (TTD)

![Mean Latenesses by Time to Death](images/tdd.png)

## Doc Embeddings: Mean Latenesses

![Mean Latenesses using Document Embeddings](images/vectors.png)

## Doc Embeddings: Mean Periodicities

![Mean Periodicities using Document Embeddings](images/vectorsp.png)

## Doc Embeddings: Overall Mean

Mean: -0.0288  
(Even stronger early style.)

# Conclusions

---

 - ~~Late Style~~ Early Style
   - Even stronger when style=content

---

## Links

 - [jonreeve.com](jonreeve.com): my website
 - [xpmethod.plaintext.in](http://xpmethod.plaintext.in/): our lab
 - [jonreeve.com/presentations/nycdhweek2019](http://jonreeve.com/presentations/nycdhweek2019): this presentation
 - [github.com/JonathanReeve/late-style-PCA](https://github.com/JonathanReeve/late-style-PCA): the code, bibliography
 - [twitter.com/j0_0n](http://twitter.com/j0_0n): my twitter account
