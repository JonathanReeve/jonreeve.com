---
date: 2017-06-23
title: 'A Project Gutenberg Database for Text Mining'
category: digital humanities
tags: 
 - corpora
---

Project Gutenberg is a large store of public domain electronic texts, one which has been around since the 70s. Nearly everyone that has experimented with computational literary analysis has at some point used their electronic texts. Many digital humanists undoubtedly share my frustrations with it: its interface is clunky, its metadata is incomplete, and it's not very friendly to computational text extraction. Inspired by David McClure's textual databases, used at the Stanford Literary Lab, I decided to fix this by creating a structured database for Project Gutenberg's corpus, and augmenting it as much as possible with publicly-available book data. This database contains the complete cleaned text of each work, all its associated metadata, and additional metadata derived from GITenberg and Wikipedia. Here's an example entry, Dickens's novel _A Tale of Two Cities_: 

```
LCC                                                                  {PR}
author                                                   Dickens, Charles
authoryearofbirth                                                    1812
authoryearofdeath                                                    1870
downloads                                                           11809
formats                 {'text/plain; charset=utf-8': 'http://www.gute...
id                                                                     98
languages                                                            [en]
lcsh                    {London (England) -- History -- 18th century -...
title                                                A Tale of Two Cities
type                                                                 Text
_repo                                             A-Tale-of-Two-Cities_98
_version                                                            0.2.2
alternative_title                                                     NaN
contributor                                                           NaN
covers                  [{'attribution': 'Alexis Lampley, 2015', 'cove...
creator                 {'author': {'agent_name': 'Dickens, Charles', ...
description                                                           NaN
edition_identifiers     {'edition_id': 'http://www.gutenberg.org/ebook...
edition_note                                                          NaN
gutenberg_bookshelf                                    Historical Fiction
gutenberg_issued                                               1994-01-01
gutenberg_type                                                       Text
identifiers                                           {'gutenberg': '98'}
jmdate                                                         2011-01-23
subjects                ['Historical fiction', 'French -- England -- L...
language_note                                                         NaN
production_note                                                       NaN
publication_date                                               2015-08-01
publication_note                                                      NaN
rights                                                           CC BY-NC
rights_url                 http://creativecommons.org/licenses/by-nc/4.0/
series_note                                                           NaN
summary                                                               NaN
tableOfContents                                                       NaN
titlepage_image                                                       NaN
url                                    http://www.gutenberg.org/ebooks/98
wikipedia                                                             NaN
filename                /run/media/jon/SAMSUNG/gitenberg/A-Tale-of-Two...
releaseDate                                                           NaN
wp_publication_date                                                  1859
wp_subjects             ['Novels_by_Charles_Dickens', 'Victorian_novel...
wp_info                 {'http://www.w3.org/1999/02/22-rdf-syntax-ns#t...
wp_literary_genres                                 ['Historical_fiction']
```

The first few fields contain Project Gutenberg's own metadata about the book: the Dickens's dates of birth and death, for instance. Project Gutenberg also provides Library of Congress classification and subject data: PR (British Literature) as well as these subjects: 

```
 'British -- France -- Paris -- Fiction',
 'Executions and executioners -- Fiction',
 'France -- History -- Revolution, 1789-1799 -- Fiction',
 'French -- England -- London -- Fiction',
 'Historical fiction',
 'London (England) -- History -- 18th century -- Fiction',
 'Lookalikes -- Fiction',
 'Paris (France) -- History -- 1789-1799 -- Fiction',
 'War stories'
```

Already you can see that this might be useful for corpus generation. If you're interested in, say, the properties of novels that deal with lookalikes, you can easily build a corpus using these subject headings, and use those texts for your analyses. 

To Project Gutenberg's metadata, I've added metadata from GITenberg. [GITenberg](https://github.com/GITenberg), like my project [Git-Lit](https://github.com/git-lit), is an initiative to make editable GitHub repositories for each Project Gutenberg book. They've also enhanced the metadata for these works. [Their edition of A Tale of Two Cities](https://github.com/GITenberg/A-Tale-of-Two-Cities_98), for instance, contains [a nice ASCIIDOC version of the book](https://github.com/GITenberg/A-Tale-of-Two-Cities_98/blob/master/book.asciidoc) which anyone can edit in the browser, and [a Creative Commons-licensed cover designed as part of the Recovering the Classics project](https://github.com/GITenberg/A-Tale-of-Two-Cities_98/blob/master/cover.jpg). All of this data, as well as a link to the GITenberg repository itself, is included in the database, along with GITenberg's version of the text. 

The next few fields come from Wikipedia. I queried DBPedia for the books with titles and authors similar to those in Project Gutenberg, and it returned about 1,800 matches. The wp_info field here contains the complete structured set of data about the book, which I then parsed into the more useful individual fields prefixed with `wp_`. This includes, for instance, `wp_subjects`, the categories to which the book's Wikipedia page belongs. For _A Tale of Two Cities_, this is: 

```
'Novels_by_Charles_Dickens', 
'Victorian_novels', 
'19th-century_novels', 
'British_novels', 
'1775_in_fiction', 
'1859_novels', 
'Chapman_&_Hall_books', 
'Novels_adapted_into_radio_programs', 
'Novels_adapted_into_television_programs', 
'Novels_adapted_into_operas', 
'A_Tale_of_Two_Cities', 
'Novels_adapted_into_plays', 
'Novels_adapted_into_comics', 
'Novels_adapted_into_films', 
'Novels_first_published_in_serial_form', 'Works_originally_published_in_All_the_Year_Round', 
'Novels_set_in_Paris', 
'Novels_set_in_London', 
'Novels_set_in_the_French_Revolution'
```

The potential for corpus creation here is immense. For instance, to get a corpus of all novels that Wikipedia lists as set in London, I can just run the Pandas query: ` df[df.wp_subjects.str.contains('Novels_set_in_London')]`, which returns a table of 46 novels. To get the average year of birth of the authors in this corpus, I can append `.authordateofbirth.mean()` (it's 1844). By using this metadata to query other APIs, I can then do fun things like compare the average Goodreads ratings for novels set in London and in Paris. (Paris wins, with an average rating of 3.8, compared with an average rating of 3.5 for London novels.)

# Corpus Statistics

## Subjects

Here are the top ten Project Gutenberg-provided Library of Congress subject headings, along with the numbers of associated texts: 

```
('Fiction', 1921),
('Short stories', 1604),
('Science fiction', 1286),
('Adventure stories', 789),
('Historical fiction', 654),
('Conduct of life -- Juvenile fiction', 639),
('Poetry', 634),
('Love stories', 620),
('English wit and humor -- Periodicals', 555),
('Detective and mystery stories', 546)
```

For those books that could be found on Wikipedia, here are the top ten Wikipedia categories for those books, along with their counts: 

```
('Novels_first_published_in_serial_form', 247),
('British_novels_adapted_into_films', 109),
('19th-century_American_novels', 99),
('Victorian_novels', 96),
('British_novels', 93),
('Novels_adapted_into_plays', 92),
('English_novels', 88),
('20th-century_American_novels', 85),
('American_science_fiction_novels', 63),
('American_novels_adapted_into_films', 59),
('19th-century_novels', 58),
```
Other interesting categories include Debut Novels (51), 1915 Novels (39), Gothic Novels (37) and Novels Adapted into Comics (36). These categories could easily be used to create sub-corpora. If you're interested in, say, differences between American and British science fiction novels, whether there's anything unusually characteristic about 1915 novels, or the properties of novels that have been adapted into comics, it's easy to construct those experiments using this corpus. Finally, here are the most common Project Gutenberg "bookshelves", with associated text counts: 

```
[('Bestsellers, American, 1895-1923', 225),
 ("Children's Literature", 178),
 ('The Mirror of Literature, Amusement, and Instruction', 174),
 ("Children's Book Series", 168),
 ('Historical Fiction', 164),
 ('US Civil War', 115),
 ('Best Books Ever Listings', 110),
 ("Children's Fiction", 99),
 ('Movie Books', 86),
 ('FR Littérature', 86)]
 ```

## It was a dark and stormy night...

I wasn't very surprised to find 306 works by Williams Shakespeare in the Project Gutenberg database, but I found it very surprising that the next most-represented author is Edward Bulwer Lytton, with 219 texts. Here are a few more: 

```
Various                                            3253
Anonymous                                           719
Shakespeare, William                                306
Lytton, Edward Bulwer Lytton, Baron                 219
Ebers, Georg                                        172
Twain, Mark                                         164
Balzac, Honoré de                                   139
Verne, Jules                                        137
Kingston, William Henry Giles                       133
```

## Languages

And here are the numbers of texts in Project Gutenberg, by language: 

```
English:             43410
French:               2766
Finnish:              1622
German:               1516
Dutch:                 749
Italian:               690
Portuguese:            544
Spanish:               538
Chinese:               425
Modern Greek (1453-):  219
```

# Future Developments

In the coming months, I'm going to try to generate sub-corpora from this database, starting with large corpora like American Literature and British Literature, and moving into more specialized corpora, like single-author corpora. I'll make these all available through [the corpus downloader `corpus`](https://github.com/DH-Box/corpus-downloader) that I'm developing with [DHBox](http://dhbox.org). 

# Code

To see the (very messy) code that I used to generate this database, check out [this project on GitHub](https://github.com/JonathanReeve/gitenberg-experiments/blob/master/pr-metadata.ipynb). I'll release the database itself, as well, as soon as I can figure out a way to get it inexpensively online. If you have any ideas for how best to accomplish that, please leave a note in the comments below!
