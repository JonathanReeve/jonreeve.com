---
title: Corpus-DB
---

Jonathan Reeve

Literary Modeling and Visualization Lab

Department of English and Comparative Literature

Columbia University

# Problem

---

How can I download all the Charles Dickens novels? 

![](images/dickens-portrait.jpg)

---

![There are lots of them.](images/dickens.jpg)

---

![Charles Dickens Books on Project Gutenberg](images/gutenberg-dickens.png)

---

## Poor Solution 

---

![](images/click.jpg)

---

## Project Gutenberg Problems

 - Server bans robots
 - Many duplicate texts
 - Texts contain paratext, licenses

---

## Metadata Problems 

 - No publication dates
 - Little other metadata.
 
---

## Wikipedia Has Metadata

 - Novel setting (London, Paris)
 - Genre, category (Bildungsroman)

---

![](images/bleak-wiki.png)
 
---

![](images/bleak2.png)
 
---

## Book Databases Have Metadata

 - Google Books
 - Amazon
 - Goodreads
 
# Solution

---

![](images/aggregate.png)

## Solution

Programmatically combine metadata from: 

   - Project Gutenberg
   - Wikipedia
   - Google Books
   - Amazon
   - Goodreads


## Solution: Clean Texts

  - Strip legal licenses
  - Strip paratexts (tables of contents, indices)

## Solution: Vector-Based Deduplication

 - Use word embeddings to identify possible duplicates
 - (Also finds texts that belong to the same series)

## Solution: Combine Archives

 - Project Gutenburg
 - Oxford Text Archive (Public Domain)
 - British Library (Public Domain)
 - UVA Etext Archive
 - Others
 
# Corpus-DB 

---

## Corpus-DB Is Not

 - Not a service
 - Not a website
 - Not user-friendly

---

## Corpus-DB

 - A textual corpus database
 - With awesome metadata
 - And an API wrapper for it

## DB Example: a Single-Author Corpus

```sql
SELECT fullText from corpusDB
WHERE author = "Charles Dickens"
```

## DB Example: Novels Set in London

```sql
SELECT * from corpusDB
WHERE genre = "Novel" 
AND setting = "London"
```

## DB Example: British Literature

```sql
SELECT * from corpusDB
WHERE LCC = "PR"
```

## DB Example: Novels Published in Britain Between 1900-1930

```sql
SELECT * from corpusDB
WHERE genre = "Novel" 
AND LCC = "PR"
AND pubYear BETWEEN 1900 AND 1930
```


## DB Example: Bildungsromane

```sql
SELECT * from corpusDB
WHERE genre = "Novel" 
AND wikipediaCategory LIKE "%Bildungsromans%"
```

# API

---

 - RESTful
 - Query over HTTP using mnemonic URLs
 - Returns JSON
 
## API Example: All Metadata for Single-Author Corpus

corpus-db.org/api/author/Dickens

## API Example: Get the Full Text

corpus-db.org/api/id/9.0/fulltext

## API Example: All Books with LCSH

corpus-db.org/api/subject/Detective and mystery stories

## API Example: Wikipedia Category

- corpus-db.org/api/adaptedTo/comics
- corpus-db.org/api/adaptedTo/films
- (coming soon!)

---

![WikiPedia Categories](images/subcorpora.png)

# Analysis

## Vector Deduplication

![](images/pg-vecs.png)

## Vector Similarities

![](images/pg-similar.png)

## LCSH vs. Goodreads Ratings

![](images/subject-ratings.png)

## Paris vs. London Novels (GoodReads Ratings)

<li class="fragment">London: 3.35</li>
<li class="fragment">Paris: 3.8</li>

## Austen v. Eliot: Comparative Stylometry

![](images/austen-eliot.png)

## Most Common LCSH for Dickens? 

```python
[('England -- Fiction', 20),
 ('Bildungsromans', 19),
 ('London (England) -- Fiction', 18),
 ('Orphans -- Fiction', 17),
 ('England -- Social life and customs -- 19th century -- Fiction', 16),
 ('Boys -- Fiction', 14),
 ('Christmas stories', 13),
 ('Ghost stories', 11),
 ('Young men -- Fiction', 11),
 ('Poor families -- Fiction', 10),
 ('Christmas stories, English', 9),
 ('Misers -- Fiction', 8),
 ('Scrooge, Ebenezer (Fictitious character) -- Fiction', 8),
 ('Sick children -- Fiction', 8),
 ('Holidays -- Fiction', 7),
 ('Domestic fiction', 7),
 ('Kidnapping victims -- Fiction', 7),
 ('Criminals -- Fiction', 7),
 ('Autobiographical fiction', 7),
 ('Child labor -- Fiction', 7)]
```

## Wikipedia Categories for Austen Novels

```python
[('Novels_by_Jane_Austen', 10),
 ('Novels_about_nobility', 9),
 ('British_novels_adapted_into_films', 6),
 ('British_novels', 5),
 ('Novels_adapted_into_plays', 5),
 ('Love_stories', 4),
 ('19th-century_novels', 4),
 ('John_Murray_(publisher)_books', 3),
 ('Novels_published_posthumously', 3),
 ('Novels_set_in_Somerset', 3),
 ('Novels_set_in_England', 3),
 ('1818_novels', 2),
 ('Books_about_persuasion', 2),
 ('British_bildungsromans', 2),
 ('Parodies', 2),
 ('Debut_novels', 2),
 ('Works_published_under_a_pseudonym', 2),
 ('1811_novels', 2),
 ('Novels_set_in_Hertfordshire', 2),
 ('1813_novels', 2)]
```

# About

## Current Status

 - DB complete, needs cleaning
 - API functional, new endpoints all the time
 - Please help!

## Contribution

 - Add example analysis notebooks (any language)
 - Help to clean and expand the DB (any language)
 - Contribute code to the project (Haskell)
 - Make the website better (CSS)

## Future Work 

 - Dynamic ingestion of new texts from original sources
 - More API endpoints 
 - More Analyses

## Thanks!

Funding provided by a NYC-DH Graduate Student Project Award (2017) and a Columbia University Libraries Digital Centers Internship (2017-2018). 

## Links

 - [corpus-db.org](corpus-db.org): the API
 - [jonreeve.com](jonreeve.com): my website
 - [twitter.com/j0_0n](http://twitter.com/j0_0n): my twitter feed
 - [xpmethod.plaintext.in](http://xpmethod.plaintext.in/): our lab
 - [github.com/JonathanReeve/corpus-db](https://github.com/JonathanReeve/corpus-db): project home
 - [jonreeve.com/presentations/corpus-db](jonreeve.com/presentations/corpus-db):  this presentation
