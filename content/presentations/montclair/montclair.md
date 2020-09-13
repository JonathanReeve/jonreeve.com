---
title: Computational Approaches to the Study of Literary Style
author: Jonathan Reeve
---

# Computational Approaches to the Study of Literary Style 

----------------------------

Jonathan Reeve  
Literary Modeling and Visualization Lab  
Group for Experimental Methods in the Humanities  
Columbia University  

----------------------------

## Open source philosophy

 - always-already online
 - open-access
 - collaborative

----------------------------

# Macro-Etymological Text Analysis

-----------------

Consider these near-synonyms: 

 - ask / question / interrogate
 - kingly / royal / regal

-----------------

![Latinate words, Brown Corpus, by genre](images/brown-latinate-with-sorted.jpg)

-----------------

![Latinate words, Caterbury Tales](../../images/chaucer/latinate-by-tale.png)

-----------------

![Latinate words, Caterbury Tales, narrative time](../../images/chaucer/latinate-by-chunks8.png)

----------------------------

![Latinate words, _Paradise Lost_, by speaker](../../images/milton-macroetym/speakers-latinate.png)

----------------------------

![Latinate words, _A Portrait of the Artist as a Young Man_, by chapter](images/portrait-2g-w-lat.png)

----------------------------

![Latinate words, CLMET Fiction, by publication date](images/clmet.png)

----------------------------

# Character Voice

## Stylistic Analysis

----------------------------

![_Clarissa_, letters, labeled](images/clarissa-style-l.png)

----------------------------

![_Clarissa_, letters, predicted](images/clarissa-style-p.png)

----------------------------

![_Ulysses_ characters](images/ulysses-style.png)

----------------------------

## Semantic Analysis

----------------------------

![_Ulysses_ characters' topics](images/ulysses-topics.png)

----------------------------

![_Clarissa_, letters, labeled](images/clarissa-vecs-l.png)

----------------------------

![_Clarissa_, letters, predicted](images/clarissa-vecs-p.png)

----------------------------

# Histories of Literary Criticism

----------------------------

![_Middlemarch_ literary critical quotations, by chapter, diachronic](images/middlemarch-diachronic.png)

----------------------------

## Words Likely to Appear in Critical Citations, 20th C

life, light, world, woman, new, great, consciousness, live, eye, knowledge, long

## Words Likely to Appear in Critical Citations, 19th C

life, Paris, old, live, time, sit, key, soul, prayer, consciousness, belief, marriage

----------------------------

![_Middlemarch_ literary critical quotations, by chapter and journal](images/middlemarch-journals.png)

----------------------------

# Experiments in Procedural Corpus Creation

## Problems

 - Difficult to download and manipulate large collections of text files
 - Book metadata is scattered

## Solution

 - Aggregate texts from public-domain sources
 - Clean licenses, paratext
 - Aggregate metadata from Wikipedia, Goodreads, Etc. 

----------------------------

![corpus-db.org](images/corpus-db.png)

----------------------------

![Vector similarity of Project Gutenberg texts](images/pg-vecs.png)

----------------------------

## Which are higher-rated on Goodreads? 
Novels set in Paris, or London?

<li class="fragment">London: 3.35</li>
<li class="fragment">Paris: 3.8</li>

## Which Austen and Eliot novels are stylistically similar? 


----------------------------

![PCA of Austen and Eliot novels](images/austen-eliot.png)

----------------------------

# Chapters

----------------------------

 - Uses Chapterize
 - Uses corpus-db.org

----------------------------

![Average number of chapters by LCSH](images/chap-subjs.png)

----------------------------

![Chapter statistics](images/chap-stats.png)

----------------------------

## Words distinctive of first paragraphs of chapters

morning, early, breakfast, afternoon, summer, autumn, winter, sunday, weather, october, arrival, june, september, saturday, awoke, situated, november, july, season, december

## Words distinctive of middle paragraphs of chapters

replied, retorted, inquired, doesn't, haven't, mustn't, shouldn't, wid, fer, wi, em, yer, protested, nothin

## Words distinctive of last paragraphs of chapters

kissed, farewell, bye, muttered, parted, disappeared, sank, page, asleep, strode, chapter, kiss, withdrew, homeward, sobbing, thanked, wept, murmured, prayed

----------------------------

# Allusion Detection

## Allusion Detection

 - Uses custom fuzzy text-matcher (plagiarism detector) over 500 novels
 - "Biblical": quote/structural resonance from the KJV
 - "Classical": mention of Greek/Roman diety
 
----------------------------

![Biblical and classical allusions, by decade](images/allusion-bibl-clas.png)

----------------------------

![Classical allusions, modernist/non-modernist, by decade](images/allusion-mod.png)

----------------------------

## Most-Quoted Book of the Bible

----------------------------

 - Matthew: 129
 - Luke: 83
 - Psalms: 70
 - Job: 50
 - Isaiah: 48
 - Mark: 46
 - Proverbs: 39
 - Genesis: 34

----------------------------

## Most-Mentioned Greco-Roman Mythological Figure

----------------------------

 - Venus: 229
 - Muses: 156
 - Apollo: 136
 - Jupiter: 126
 - Juno: 100
 - Pan: 94

----------------------------

# Resources

## Do it yourself

## Books

 - [nltk.org/book](http://nltk.org/book)
 
## Notebooks

 - [github.com/JonathanReeve/dataviz-workshop-2017]( https://github.com/JonathanReeve/dataviz-workshop-2017/blob/master/dataviz-workshop.ipynb) 
 - [github.com/JonathanReeve/advanced-text-analysis-workshop-2017](https://github.com/JonathanReeve/advanced-text-analysis-workshop-2017) 
 
## DHBox

![](images/dhbox.png)

## DHBox Notebooks

 - [github.com/DH-Box/docs/](https://github.com/DH-Box/docs/blob/master/text-analysis-tutorial.md)

## DH-USB

![](images/dh-usb.png)

----------------------------

![](images/dh-usb2.png)

----------------------------

# Links

## Links

 - [xpmethod.plaintext.in](http://xpmethod.plaintext.in/): our lab
 - [jonreeve.com](jonreeve.com): my website
 - [jonreeve.com/presentations/montclair](http://jonreeve.com/presentations/montclair): this presentation
 - [github.com/JonathanReeve](https://github.com/JonathanReeve): the code
 - [twitter.com/j0_0n](http://twitter.com/j0_0n): my twitter account

----------------------------
