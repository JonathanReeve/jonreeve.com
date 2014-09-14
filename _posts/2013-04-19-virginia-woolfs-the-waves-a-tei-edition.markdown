---
layout: post
status: publish
published: true
title: ! 'Virginia Woolf''s The Waves: A TEI Edition'
category: digital humanities
tags: TEI Python XML
---

Stephen Ramsay’s chapter “Algorithmic Criticism” (from [_Reading Machines_](http://books.google.com/books?id=14KPI0ORQigC), anthologized in [A Companion to Digital Literary Studies](http://nora.lis.uiuc.edu:3030/companion/view?docId=blackwell/9781405148641/9781405148641.xml&doc.view=print&chunk.id=ss1-6-7&toc.depth=1&toc.id=0)) describes conducting a tf-idf analysis on the speech from the characters in Virginia Woolf’s novel _The Waves_. _The Waves_ is probably the ideal novel for computationaly studying fictional speech, since it is all in the form 

>“A short clause,” said Speaker, “a continuation of the sentence.” 

For example, 

>“I see a ring,” said Bernard, “hanging above me. It quivers and hangs in a loop of light.” 

Chris Forster [describes a process for extracting the characters’ speech using some manual edits and some command-line-fu](http://cforster.com/2013/02/reading-the-waves-with-stephen-ramsay/), but bemoans the lack of a TEI edition which would simplify this process. I took that as a challange and threw together [a quick and dirty TEI version of _The Waves_](http://jonreeve.com/waves-tei/waves-tei.xml) as an exercise, and used a XSL transformation to color-code the characters’ dialog. 

![Color Coding](/images/waves-tei/waves-tei.jpg) 

From here, there are a number of ways of extracting the dialog of a particular character or group of characters. One easy way is to edit the CSS for the character (i.e. `p.Bernard`) in the Web Inspector of Firefox or Chrome, adding `display: none;` under `color: red;`:  

![Web Inspector](/images/waves-tei/inspector.jpg)

If you do this for all the male characters, only female dialog will display. You can also edit the XSLT to selectively display dialog, or extract it with an XML parser like the [BeautifulSoup](http://www.crummy.com/software/BeautifulSoup/bs4/doc/#id11) python module like this: 
 
```python
from bs4 import BeautifulSoup
tei=open('waves-tei.xml').read() #read the file
soup.find_all(who=u'#Bernard')
```

Future versions of the webpage will hopefully provide user-interface buttons that will allow the visitor to turn on or off a character’s dialog, or to turn off the color-coding, as I attempted to do with the commentary and metadata in my TEI edition of [_The Art of Good Behaviour_](http://jonreeve.com/behaviour/).

Disclaimer: this text is an experiment in textual analysis.  
