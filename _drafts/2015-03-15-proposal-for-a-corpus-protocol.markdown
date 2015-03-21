---
layout: post
title: A Proposal for a Corpus Sharing Protocol
category: digital humanities
tags: 
 - text analysis
 - corpus linguistics
 - corpora 
 - open source
---

Digital humanists working in digital text analysis need a way to share corpora. Following is a rough sketch of a way to share corpora in way that faciliatates collaboration, provides for easy error correction, and adheres as much as possible to decentralized, open-source, and open-access models. 

#Problems

##The Problem of Corpus Availability
Franco Moretti gave a talk at NYU last week, detailing digital text analyses he had conducted on large corpora. The handout he had circulated beforehand, whose authors also included Mark Algee-Hewitt, Sarah Allison, Marissa Gemma, Ryan Heuser, and Hannah Walser, included a section about the difficulty of obtaining many of these corpora: 

>In August, requests were sent to Hathi and Gale – with both of which Stanford [has] a solid, long- standing agreement – for their 300 volumes; two months later, we haven't yet received anything. Of the 100 existing only in print, about half were held by the British Library, in London, which just a few months earlier had kindly offered the Literary Lab a collection of 65,000 digitized volumes from its collections; unfortunately, none of the books we were looking for was in this corpus. The special collections at UCLA and Harvard (which held about 50 of the 100 books) sent us a series of estimates that ranged (depending, quite reasonably, on the conditions of the original, and on the photographic requirements, that could be possibly very labor-intensive) from $1,000 to $20,000 per novel; finally, six novels were part of larger collections held by Proquest, which offered us a very generous 50% discount; even so, those six books would have cost us $147,000, or $25,000 per title. 

It is beyond absurd that a text that has long been out of copyright would cost researchers $25,000, and that this represents a 50% discount on the usual rate. The very premise that public-domain works are held captive by for-profit institutions is one that seems at odds with the ideals of science and academia--it greatly obstructs the repeatability of experiments in text analysis, and sets the barrier for entry into the field excessively high. I asked Prof. Moretti if he had any ideas about how to minimize this problem, and he basically said that his hands were tied--there were legal reasons why he couldn't provide texts from his corpus to other researchers. Stanford had particular agreements with these data providers, and if I wanted to use that data, I would first need to be affiliated with an institution that could strike corpus sharing agreements with the data providers, and then make agreements with the data providers through that institution. Those are major hurdles for an independent scholar who wishes to do large-scale text analysis.  

I've encountered this difficulty of obtaining texts a few times, myself. When I saw the fantastic presentation on the detection of poetic meter at last year's Digital Humanities conference, I asked them how they were able to obtain texts from the Literature Online (LION) corpus which were the subject of their experiments. They replied that they'd simply asked ProQuest for this. With this in mind, I contacted John Pegum at ProQuest, in the hopes of obtaining texts I might use for large-scale analyses. His reply was thorough, but basically concluded that I couldn't afford to pay for this privilege. When I responded with a few other options--one of which was working directly on a server of their choosing, without making a copy of the text--I never received a reply. 

Even some of the best text repositories, like the Oxford Text Archive, are designed in such a way as to make them prohibitively difficult to use for macroanalysis. Many of the texts in the Oxford archive are behind a permissions wall by default--you have to email Oxford and specifically request a certain text in order to gain access to it. If a researcher, then, is interested in analyzing hundreds of texts, that would require hundreds of emails.  

This problem of corpus availability was one of the major topics of discussion at the short workshop Computer-Based Analysis of Drama I attended this week in Munich. Many of the presenters used texts from the TextGrid repository, and it was suggested that this could be a platform for the sharing of corpora among researchers. Yet TextGrid is apparently losing funding soon, and might go down. So how can we find a common platform for sharing texts, so that others might benefit from them?   

##The Problem of Corpus Immutability

Corpora of scanned texts, such as those of Project Gutenburg or EEBO-TCP, are full of OCR errors. Project Gutenberg handles this with a fleet of volunteer proofreaders, but the fundamentally static nature of their web pages makes it difficult easy for others outside of their infrastructure to contribute textual corrections. At Computer-Based Analysis of Drama, Martin Muller of Northwestern University demonstrated a fantastic system for crowdsourced correction of these errors, that is the best such system I've seen. Yet, at the moment, it only works on a handful of texts that ... 

##Summary 

What is needed is a platform for: 
 * easily and instantly uploading corpora for sharing
 * tracking the versions of those corpora
 * collaboratively correcting texts in those corpora that contain OCR errors 
 * tracking problems with the texts, and openly discussing those problems 
 * allowing for the machine-readability of these corpora with a REST API
 * accomplishing all of the above with minimal programming
 * accomplishing all of the above with adherence to an open-source and open-access philosophy

#A Proposed Solution

Such a platform as described above has already long been in use among coders, and would take only minor usage modifications to be used as a plaform for sharing corpora. For those that aren't already familiar with it, GitHub is a repository of repositories, tightly integrated into the version control system git, where coders can upload their projects and collaborate on others' projects. Each repository features an issue tracker, 

The generator platform Yeoman uses an interesting decentralized repository schema. Rather than manually maintain a list of user-created generators, they dynamically search GitHub for any repository with the prefix `generator-`. Then they sort those repositories by search relevance and the number of times they've been starred. This decentralized approach ensures that their list of generators is always up-to-date, and sorted fairly democratically. This model would work well for corpora.  

I propose that we upload our corpora to GitHub, with repositories prefixed `corpus-`. For example, if I am uploading a corpus of Shakespeare TEI files, I might name my repository `corpus-shakespeare-TEI`. That repository could contain a set of TEI XML files, or a TEI corpus file. This would ensure that the repository is easily machine-readable.  

#Possible Concerns
 * Although this is a decentralized solution when compared with other, more static, repositories, GitHub is still a centralized hub. 
   - This might be solved by dynamically pulling repositories from a number of similar git sources, like BitBucket. 
 * As Dario ... reminded me, git has peculiarities with diffing whitespace that might make it less than ideal for tracking changes in XML.  
   - This might be solved by turning on the `ignore whitespace` flag in git, by running ...
