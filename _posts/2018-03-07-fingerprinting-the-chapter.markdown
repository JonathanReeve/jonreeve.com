---
layout: post
title: "Fingerprinting the Chapter"
category: digital humanities
tags:
 - corpus-db
 - text analysis
 - python
---

What is a chapter, really? Inspired by Nicholas Dames's work on the history of the chapter ([published in the _New Yorker_ a few years ago as "The Chapter: A History"](https://www.newyorker.com/books/page-turner/chapter-history)]), I've started investigating this question. We all have an intuition about chapters—how long they should be, their temporal scope, what they generally contain. Most of us, if given a paragraph chosen at random from a novel, could probably tell whether it began a chapter, ended a chapter, or fell somewhere in between. Yet could we say why? Could we explain our intuition to a computer? Sure, if we saw a line like "and they rode off into the setting sun" we might think it belonged to the end of a chapter. Do chapters represent a single day in narrative time? I decided to compute the linguistic fingerprint of the chapter, do get closer to understanding what it is. 

[Corpus-DB](http://corpus-db.org), the [textual corpus database I've been developing](http://jonreeve.com/2017/06/project-gutenberg-the-database/), makes it easy to get large numbers of plain text files that match certain categories. At the moment it contains all 50,000 or so texts from Project Gutenberg, and metadata about these books gathered from Wikipedia and other sources. Using this database, I was easily able to assemble corpora of several thousand chaptered novels. I then broke these novels into chapters using two methods. The first, using my tool [chapterize](https://github.com/JonathanReeve/chapterize), reads the plain text files of the novels, looks for markers like "Chapter I" using regular expressions, and divides the novels accordingly. The second leverages hidden metadata in Project Gutenberg HTML files. If you take [a peek at the source code of a Project Gutenberg HTML file](<p><a name="c2" id="c2">), you'll see named anchors that look like `<a name="c2" id="c2">`. These IDs correspond to the chapter numbers. By counting the number of words of text that occur between these markers, we can measure the chapter lengths of thousands of novels at a time. 

# Chapter Statistics


