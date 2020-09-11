---
date: 2017-12-05
title: 'Computationally Identifying Similar Books in Project Gutenberg'
category: digital humanities
tags: 
 - corpora
 - python
---

As one of the first digital libraries, Project Gutenberg has lived through a few generations of computers, digitization techniques, and textual infrastructures. It's not surprising, then, that the corpus is fairly messy. Early transcriptions of some electronic texts, hand-keyed using only uppercase letters, were succeeded by better transcriptions, but without replacing the early versions. As such, working with the corpus as a whole means working with a soup of duplicates. To make matters worse, some early versions of text were broken into many parts, presumably as a means to mitigate historical bandwidth limitations. Complete versions were then later created, but without removing the original parts. I needed a way to deduplicate Project Gutenberg books. 

To do this, I used a suggestion from Ben Schmidt and vectorized each text, using the new Python-based natural language processing suite SpaCy. [SpaCy creates document vectors by averaging word vectors from its model containing about 1.1M 300-dimensional vectors](https://spacy.io/usage/vectors-similarity). These document vectors can then be compared using cosine similarity to determine the semantic similarities of the documents. It turns out that this is a fairly good way to identify duplicates, but has some interesting side-effects. 

Here, for instance, are high-ranking similarities (99.99% vector similarity or above) for the first 100 works in Project Gutenberg. The numbers are the Project Gutenberg book IDs (see, for instance, [this index](http://www.gutenberg.org/dirs/GUTINDEX.1996) of the first 768 works). 

```
1.  The King James Version of (10) -similar to- The Bible, King James Ver (30)
2.  Alice's Adventures in Won (11) -similar to- Through the Looking-Glass (12)
3.  Through the Looking-Glass (12) -similar to- Alice's Adventures in Won (11)
4.  The 1990 CIA World Factbo (14) -similar to- The 1992 CIA World Factbo (48)
5.  Paradise Lost             (20) -similar to- Paradise Lost             (26)
6.  O Pioneers!               (24) -similar to- The Song of the Lark      (44)
7.  O Pioneers!               (24) -similar to- Alexander's Bridge        (91)
8.  Paradise Lost             (26) -similar to- Paradise Lost             (20)
9.  The 1990 United States Ce (29) -similar to- The 1990 United States Ce (37)
10. The Bible, King James Ver (30) -similar to- The King James Version of (10)
11. The 1990 United States Ce (37) -similar to- The 1990 United States Ce (29)
12. The Strange Case of Dr. J (42) -similar to- The Strange Case of Dr. J (43)
13. The Strange Case of Dr. J (43) -similar to- The Strange Case of Dr. J (42)
14. The Song of the Lark      (44) -similar to- O Pioneers!               (24)
15. The Song of the Lark      (44) -similar to- Alexander's Bridge        (91)
16. Anne of Green Gables      (45) -similar to- Anne of Avonlea           (47)
17. Anne of Green Gables      (45) -similar to- Anne of the Island        (50)
18. Anne of Avonlea           (47) -similar to- Anne of Green Gables      (45)
19. Anne of Avonlea           (47) -similar to- Anne of the Island        (50)
20. The 1992 CIA World Factbo (48) -similar to- The 1990 CIA World Factbo (14)
21. The 1992 CIA World Factbo (48) -similar to- The 1993 CIA World Factbo (84)
22. Anne of the Island        (50) -similar to- Anne of Green Gables      (45)
23. Anne of the Island        (50) -similar to- Anne of Avonlea           (47)
24. A Princess of Mars        (60) -similar to- The Gods of Mars          (62)
25. A Princess of Mars        (60) -similar to- Warlord of Mars           (65)
26. The Gods of Mars          (62) -similar to- A Princess of Mars        (60)
27. The Gods of Mars          (62) -similar to- Warlord of Mars           (65)
28. Warlord of Mars           (65) -similar to- A Princess of Mars        (60)
29. Warlord of Mars           (65) -similar to- The Gods of Mars          (62)
30. Adventures of Huckleberry (73) -similar to- Tom Sawyer Abroad         (88)
31. Tarzan of the Apes        (75) -similar to- The Return of Tarzan      (78)
32. The Return of Tarzan      (78) -similar to- Tarzan of the Apes        (75)
33. The Beasts of Tarzan      (82) -similar to- Tarzan and the Jewels of  (89)
34. The 1993 CIA World Factbo (84) -similar to- The 1992 CIA World Factbo (48)
35. Tom Sawyer Abroad         (88) -similar to- Adventures of Huckleberry (73)
36. Tarzan and the Jewels of  (89) -similar to- The Beasts of Tarzan      (82)
37. Alexander's Bridge        (91) -similar to- O Pioneers!               (24)
38. Alexander's Bridge        (91) -similar to- The Song of the Lark      (44)
```

The first pair here is of duplicates: both are King James Versions of the Bible. 
The same is true of lines 5 and 8, and lines 12-13: they're just duplicates. 
All the other works are members of a novelistic series. Lines 2 and 3 are _Alice in Wonderland_ and its sequel. Lines 6 and 7 are Willa Cather novels of the Great Plains trilogy. Lines 16-19, and 22-23 identify the _Anne of Green Gables_ novels. Lines 24-29 are a cluster of Edgar Rice Burroughs of the _Mars_ series, and there is also another cluster of Burroughs novels, the _Tarzan_ series, at 31-33. Line 35 shows Mark Twain novels of the Tom Sawyer and Huck Finn world. The algorithm even identifies the two 90s CIA World Factbooks as part of a series. 

When I lower the cutoff similarity score, I can get even more interesting pairs. Less-recognizable series, like _Paradise Lost_ and _Paradise Regained_, have similarity scores of around 97%. At that level, completely unrelated novels with the same settings, or written in around the same time period (Victorian novels, for instance), begin to cluster together. 

The chart below shows a PCA-reduced 2D vector space approximating the similarity between books 1-20. There are interesting clusters here: the American Constitution and Bill of Rights cluster together, along with the Declaration of Independence, the Federalist Papers, and Abraham Lincoln's first inaugural address. Lincoln's second address, however, clusters rather with his Gettysburg Address, and John F. Kennedy's inaugural address. 

![PCA of PG Books 1-20](/images/gutenberg/pg-vecs.png)

Non-fiction seems to be clustered at the bottom of the chart, whereas fiction is at the top. Quasi-fictional works, like the _Book of Mormon_ and the Bible, are in between. Similarly, _Moby Dick_, a work of fiction that nonetheless contains long encyclopedic passages of non-fiction, lies in the same area. The most fantastical works, which are also the three children's books, _Peter Pan_ and the two Carroll novels, cluster together in the upper left. 

As always, [the code used to create all of this is on GitHub](https://github.com/JonathanReeve/gitenberg-experiments/blob/master/pg-vectorize2.ipynb). I welcome your comments and suggestions below!
