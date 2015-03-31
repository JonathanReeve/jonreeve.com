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

Digital humanists working in computational text analysis need a better way to share corpora. Following is a rough sketch of a way to share texts in way that facilitates collaboration, provides for easy error correction, and adheres as much as possible to decentralized, open-source, and open-access models. 

#Problems

##The Problem of Corpus Availability
Franco Moretti gave a talk at NYU two weeks ago, detailing digital analyses he had conducted on large numbers of novels. The handout he circulated beforehand, whose authors also included Mark Algee-Hewitt, Sarah Allison, Marissa Gemma, Ryan Heuser, and Hannah Walser, included a section about the difficulty of obtaining many of these corpora: 

>In August, requests were sent to Hathi and Gale – with both of which Stanford [has] a solid, long- standing agreement – for their 300 volumes; two months later, we haven't yet received anything. Of the 100 existing only in print, about half were held by the British Library, in London, which just a few months earlier had kindly offered the Literary Lab a collection of 65,000 digitized volumes from its collections; unfortunately, none of the books we were looking for was in this corpus. The special collections at UCLA and Harvard (which held about 50 of the 100 books) sent us a series of estimates that ranged (depending, quite reasonably, on the conditions of the original, and on the photographic requirements, that could be possibly very labor-intensive) from $1,000 to $20,000 per novel; finally, six novels were part of larger collections held by Proquest, which offered us a very generous 50% discount; even so, those six books would have cost us $147,000, or $25,000 per title. 

It is unreasonable that a text that has long been out of copyright would cost researchers $25,000, and that this represents a 50% discount on the usual rate. The very premise that public-domain works are held captive by for-profit institutions is one that seems at odds with the ideals of science and academia—it greatly obstructs the repeatability of experiments in text analysis, and sets the barrier for entry into the field unnecessarily high. I asked Prof. Moretti if he had any ideas about how to minimize this problem, and he answered that there were legal reasons why he couldn't provide texts from his corpus to other researchers. Stanford had particular agreements with these data providers, and if I wanted to use that data, I would first need to be affiliated with an institution that could strike corpus sharing agreements with the data providers, and then make agreements with the data providers through that institution. Those are major hurdles for an independent scholar who wishes to do large-scale text analysis.  

The problem of corpus availability is deep and pervasive. At last year's Digital Humanities conference, I saw a fantastic presentation by the Stanford Literary Lab on the detection of poetic meter. Since they had analyzed a set of texts from the Literature Online (LION) database, I asked them how they were able to get permissions for that data set. They replied that they'd simply asked ProQuest for it. With this in mind, I contacted John Pegum at ProQuest, in the hopes of obtaining texts I might use for large-scale analyses. His reply was polite and thorough, but concluded that I very likely couldn't afford to pay for this privilege. When I responded with a few other options—one of which was working directly on a server of their choosing, without making a copy of the text—I never received a reply. 

Even some of the best text repositories, like the [Oxford Text Archive](http://ota.ahds.ac.uk/), are designed in such a way as to make them prohibitively difficult to use for macroanalysis. Many of the texts in the Oxford archive are behind a permissions wall, and seemingly by default. To gain access to a text, a researcher must apply to Oxford and specifically request a particular text. If that scholar is then interested in analyzing hundreds of texts, that would require hundreds of requests.

This problem was one of the major topics of discussion at the workshop Computer-Based Analysis of Drama I attended last week in Munich. Many of the presenters used texts from the TextGrid repository, and it was suggested that this could be a platform for the sharing of corpora among researchers. Yet TextGrid is apparently losing funding soon, and might go down. So how can we find a common platform for sharing texts, so that others might benefit from them?   

##The Problem of Corpus Immutability

Another issue with the current paradigms for the sharing of corpora is the problem of corpus immutability. If texts are only available from a centralized source, then new versions of that text depend on the maintainers of that centralized source for all changes. Corpora of scanned texts, such as those of Project Gutenburg or EEBO-TCP, are full of OCR errors, and there are few opportunities for user corrections of those errors. Project Gutenberg handles this problem with volunteer proofreaders, but the fundamentally static nature of their web pages makes it difficult for those outside of their infrastructure to contribute textual corrections. At Computer-Based Analysis of Drama, Martin Muller of Northwestern University demonstrated a fantastic system for crowdsourced correction of these errors, [Annolex](http://annolex.at.northwestern.edu/), that is the one of the best such systems I've seen. Yet, at the moment, it only works on a select number of early modern English texts. How can we make this sort of crowdsourced error correction even more democratic, and even more distributed? 

##Summary 

What is needed is a platform for: 

 * easily and instantly uploading corpora for sharing among scholars and researchers
 * tracking the versions of those corpora
 * collaboratively correcting texts in those corpora that contain OCR or other errors 
 * tracking problems with the texts, and openly discussing those problems 
 * allowing for the machine-readability of these corpora with a REST API
 * adherence to an open-source and open-access philosophy
 * accomplishing all of the above with minimal programming

#A Proposed Solution

Such a platform as described above has already long been in use among coders, and would need only minor usage modifications to be used as a plaform for sharing corpora. For those that aren't already familiar with it, [GitHub](https://github.com) is a repository of repositories, tightly integrated into the version control system [git](http://git-scm.com/book/en/v2/Getting-Started-Git-Basics), where coders can [upload their projects](https://guides.github.com/introduction/getting-your-project-on-github/) and [collaborate on others' projects](https://guides.github.com/activities/forking/). Each repository features [an issue tracker](https://guides.github.com/features/issues/), wiki, and a statistics suite that can help you to see at a glace the history of a project's versions. It works very well already for handling computer code, and would work just as well for handling text corpora.  

One of the best things about GitHub is its push-button forking ability. To create a copy of any existing repository, just click the "fork" button on that repository's home page. This creates a copy of the entire project that is now in your user account. You can make any changes you like to your copy (even without leaving the browser), and when you're finished with your contributions, you can submit a pull request to the original repo to request that your changes be merged into the original project. This allows for distributed error-correction, maintaining all the while the history of the contributions to the text.   

The web app generator platform [Yeoman](http://yeoman.io/) uses GitHub for [their decentralized generator collection](http://yeoman.io/generators/). Rather than manually maintain a list of user-created generators, they dynamically search GitHub for any repository with certain metadata, like the prefix `generator-` or the keyword `yeoman-generator`. Then they sort those repositories by the number of times they've been starred. This approach ensures that their list of generators is always up-to-date, and sorted fairly democratically. This model could be applied to corpora.  

I propose that we upload our corpora to GitHub, with repositories named with the prefix `corpus-`. For example, if I am uploading a corpus of Shakespeare TEI files, I might name my repository `corpus-shakespeare-TEI`. That repository could contain a set of TEI XML files, or a TEI corpus file. This would ensure that the repository is as easily machine-readable as it is human-readable. 

Eventually, a scraping engine might be built which could, like Yeoman's generator list, dynamically search GitHub for any repo beginning with `corpus-`. Then, performing large-scale text analyses on any set of these corpora would be orders of magnitude easier than manually assembling a database of texts. 

#Possible Concerns

 * Although this is a decentralized solution when compared with other, more static, repositories, GitHub is still a centralized hub. 
   - This might be solved by dynamically pulling repositories from a number of similar git sources, like BitBucket. 
 * As Dario Kampkaspar reminded me, git has peculiarities with diffing whitespace that might make it less than ideal for tracking changes in XML.  
   - This might be solved by [turning on the `ignore whitespace` flag in git](http://stackoverflow.com/questions/9776527/merging-without-whitespace-conflicts). 
 * GitHub has [a repository size limit of about 1GB, with a soft limit of about 100M per file](https://help.github.com/articles/what-is-my-disk-quota/). 
   - This might be solved by breaking up repositories into submodule repositories. One parent corpus repo can hold several submodule repos. 

#Discussion
What do you think? Is this a protocol you might be interested in adopting? Why or why not? Please leave comments below. 

#Update 3/31

[Carrie Schroeder](https://twitter.com/ctschroeder), [Allen Riddel](https://twitter.com/ariddell) and others on Twitter have pointed out that, especially in non-English DH fields, many corpora are already on GitHub: 

 * [Gitenberg](https://github.com/GITenberg) is a GitHub user with repositories of Project Gutenberg texts. 

Non-English Corpora on GitHub Include: 
 
 * Texts from the [Chinese Buddhist Electronic Text Association](https://github.com/cltk/chinese_text_cbeta_indices)
 * The [Open Greek and Latin Project](http://github.com/OpenGreekAndLatin) at Leipzig
 * Papyri from the [Integrating Digital Papyrology Project](https://github.com/papyri/idp.data)

If I'm missing any, please let me know in the comments below. 
