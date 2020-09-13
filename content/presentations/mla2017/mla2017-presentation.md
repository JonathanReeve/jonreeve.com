
# {#overview .step data-scale=10}

# {.step data-x=-4000 data-y=2000 data-scale=8}

![](branching-illustration.png) 

# Git-Lit: an Application of Distributed Version Control Technology toward the Creation of 50,000 Digital Scholarly Editions {.step data-x=-2000 data-y=-1000 data-scale=7}

#{.step data-x=-2000 data-y=1500 data-scale=5} 

##Jonathan Reeve 

##Group for Experimental Methods in the Humanities

##Columbia University

# The Problems {.step data-x=2000} 

* Electronic texts are full of errors
* These errors are difficult to correct
* Etexts are often static, stuck in the hands of the few
* Their editorial histories are often obscure
* They’re difficult to assemble into plain-text corpora for text analysis
* They’re ugly (looking at you, Project Gutenberg!)  

# Version Control Systems {.step data-x=2000 data-y=1000} 

* Git, mercurial, svn, bazaar
* Open-source, community-owned 
* Some centralized, some distributed
* Used very widely by coders (over 10 million GitHub repositories) 
* Starting to be used by humanists

# Features of Version Control Systems {.step data-x=2000 data-y=2000} 

* Store every version of a document or project *efficiently* 
* Store metadata about every edit: who made the change, when the change happened, what changed
* Facilitate collaboration, analysis, version management

# {.step data-x=2000 data-y=3000} 

![](phd101212s.gif){.figure}

# Distributed Version Control {.step data-x=3000} 

![](distributed.png){.figure}

# Distributed Version Control {.step data-x=3000 data-y=1000}  

* Doesn’t privilege a central text, repository, user, or computer
* Anyone can pull changes from anyone 
* Democratization of scholarly editing? 

# Example Features:  {.step data-x=3000 data-y=2000} 

* Temporarily reset your project to an earlier version/state: 

`git checkout <change number>`

* Revert a single change from an arbitrary point in the past: 

`git revert <change number>`

(Try doing that with Microsoft Word!)

* Automatically combine changesets from two versions: 

`git merge <branch name>`

# Version Control Cloud Services {.step data-x=4000} 

* GitHub, BitBucket, GitLab
* Allow easy forking, pull requests
* Facilitate collaboration
* Offer project management features such as issues, wiki, milestones

# Git-Lit {.step data-x=5000}

* Open-source initiative 
* Inspired by GITenberg’s work with Project Gutenberg
* Comprised of ~2-3 volunteer developers and many other contributors 
* Codebase written in Python

# Project Aims {.step data-x=5000 data-y=1000}

* parse and post to GitHub ~50,000 texts
* create submodules for categories of texts 
* facilitate the crowdsourced proofreading of these texts
* make plain text editions for text mining 
* make beautiful, readable web editions of each text 

# The Corpora {.step data-x=5000 data-y=2000}

* ~45,000+ scanned works from the British Library in ALTO XML
* ~1,000 works from the University of Virginia Electronic Text Center in TEI XML
* merged with the ~40,000 from Project Gutenberg, already available via GITenberg

# How Can I Use It? {.step data-x=5000 data-y=3000}

* Download the git repositories using the `corpus-downloader` python module
* Use it with DHBox
* Proofread a text and submit changes by forking a text repository and submitting a pull request

# Call for Contributions {.step data-x=5000 data-y=4000}

* Anyone can be involved! Coders and non-coders alike. 
* Pull requests appreciated (Python)
* Bug reports, feature requests, and issue comments appreciated

# Links {.step data-x=6000} 

* About: <http://git-lit.github.io>
* Sources: <https://github.com/Git-Lit>
* Corpus downloader: <https://github.com/DH-Box/corpus-downloader>
* DHBox: <http://dhbox.org/>
* This presentation: <http://jonreeve.com/presentations/mla2017/>
* My Homepage: <http://jonreeve.com>
* Email: <jon.reeve@gmail.com> 
