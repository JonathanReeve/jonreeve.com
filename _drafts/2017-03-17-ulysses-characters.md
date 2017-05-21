---
layout: post
title: An Exploratory Analysis of Character Speech in Ulysses
category: digital humanities
tags:
 - natural language processing
 - pca
 - statistics
 - text analysis
 - joyce
---

For the past few months, I’ve been working on [a semantically marked-up edition of James Joyce’s _Ulysses_ in TEI XML](https://github.com/JonathanReeve/corpus-joyce-ulysses-tei), based on Hans Walter Gabler’s edition. About two weeks ago, thanks to great work by [Ronan Crowley](http://uni-passau.academia.edu/RonanCrowley), the edition just reached complete dialogue attribution. That markup turns the opening line of dialogue from “Introibo ad altare Dei” to `<said who="Buck Mulligan">―<quote xml:lang="la">Introibo ad altare Dei.</quote></said>`. This markup is easily machine-readable, allowing for the extraction of dialogue according to character, and comparative analyses to be done between them. Using this markup, I’ve been able to find a few interesting things about character utterances in _Ulysses_. Here are a few preliminary findings from an exploratory analysis of these utterances. 

# The Utterances of Ulysses, Quantified

![Utterances of Ulysses](/images/ulysses-characters/utterances.png)
