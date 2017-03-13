---
layout: post.pug
title: "Macroetym: a Command-Line Tool for Macro-Etymological Textual Analysis"
category: digital humanities
tags:
 - etymology
 - text analysis
 - python
---

I'm proud to introduce macroetym, a command-line tool for macro-etymological textual analysis, which is now available for download with the Python package manager, pip. It's a complete rewrite of [The Macro-Etymological Analyzer](http://jonreeve.com/etym), the web tool for macro-etymological analysis I wrote a few years ago, first described in [this post](/2013/11/introducing-the-macro-etymological-analyzer), and presented at [DH2014](http://jonreeve.com/dh2014/). It can now analyze any number of texts, and texts in 250 languages. Here are a few examples of the program in action:

### A simple comparative macro-etymological analysis of two texts

```
$ macroetym wells-time-machine.txt woolf-mrs-dalloway.txt

              wells-time-machine.txt  woolf-mrs-dalloway.txt
Austronesian                0.045788                0.021177
Celtic                      0.068681                0.047649
Germanic                   40.885226               40.334075
Hellenic                    0.840965                1.080051
Indo-Iranian                0.015263                0.153537
Latinate                   57.724359               57.722893
Other                       0.137363                0.333545
Semitic                     0.282357                0.243541
Turkic                      0.000000                0.031766
Uralic                      0.000000                0.031766
```

### A more verbose analysis of a single text

```
$ macroetym woolf-mrs-dalloway.txt --allstats

                                woolf-mrs-dalloway.txt
Anglo-Norman                                      7.23
Angloromani                                       0.03
Arabic                                            0.06
Aragonese                                         0.06
Dutch                                             0.29
Dutch, Middle (ca. 1050-1350)                     0.27
English, Old (ca. 450-1100)                      36.56
French                                            7.81
French, Middle (ca. 1400-1600)                    3.96
French, Old (842-ca. 1400)                       21.58
German                                            0.06
German, Middle Low                                0.12
... etc.
```

### An analysis of all the books of _Paradise Lost_

```
$ macroetym paradise-lost-books/* --showfamilies Latinate        

          bk/book01.txt  bk/book02.txt  bk/book03.txt  bk/book04.txt
Latinate      52.622816      56.005313      52.644493      50.522588   

          bk/book05.txt  bk/book06.txt  bk/book07.txt  bk/book08.txt
Latinate      55.929858      56.608863       51.46886      54.492665   

          bk/book09.txt  bk/book10.txt  bk/book11.txt  bk/book12.txt  
Latinate      53.625632      54.745275      50.982633      52.195609  
```

### Machine-readable output

    macroetym paradise-lost-books/* --csv > pl-books.csv

### Analysis of texts in languages other than English

    macroetym madame-bovary.txt --lang=fra

# Installation

Install macroetym using pip3:

    pip3 install macroetym

Alternatively, grab the source code on GitHub and install locally:

    git clone https://github.com/JonathanReeve/macro-etym
    cd macro-etym
    pip3 install .

# Contributing

Macroetym is free and open-source software! There are a number of [open issues](https://github.com/JonathanReeve/macro-etym/issues) with the program that need addressing. If you know a bit of python, feel free to hack around on the code as you see fit. Pull requests are very welcome. Non-code contributions are also welcome in the form of bug reports, documentation, or experiments in text analysis that use the program.
