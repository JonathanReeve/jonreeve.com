#Git-Lit 
an Application of Distributed Version Control Technology toward the Creation of 50,000 Digital Scholarly Editions

Jonathan Reeve

Columbia University

#The British Library Corpus
 * ~49,500 scanned and OCRed books from the British Library
 * ALTO XML
 * Not available online—shipped through the mail on a hard drive
 * Largest ever (soon-to-be) downloadable digital literary corpus? 
 * Enormous potential for text mining, digital literary studies

---

![Histogram of Publication Years](years.png)

---

#Problem: 
##Unshared corpora
 * Corpora are not often shared in DH
 * This makes DH experiments unrepeatable
 * Bad science!

#Problem: 
##Etexts lack editorial history
 * Especially early Project Gutenberg texts
 * Who edited a text? When? Where?
 * What edits were made? 

#Problem: 
##Corpora are difficult to assemble
 * Compiling a corpus of, say, 19th century _Bildungsromane_, traditionally requires manually assembling and cleaning texts
 * This process should be automated

#Problem
##Centralized data stores ("data silos")
 * What happens when the site goes down? 
 * When the project loses funding? 
 * e.g. TextGrid

---

![](silos-gonna-silo.png)


#Problem 
##ALTO XML

```xml
<String ID="P21_ST00004" HPOS="138" VPOS="151" 
WIDTH="67" HEIGHT="34" CONTENT="Thy" WC="1.00" CC="000"/>
<SP ID="P21_SP00002" HPOS="205" VPOS="185" WIDTH="21"/>
<String ID="P21_ST00005" HPOS="226" VPOS="152" 
WIDTH="210" HEIGHT="28" CONTENT="self-oblation" WC="0.87" CC="0030010105102"/>
<SP ID="P21_SP00003" HPOS="436" VPOS="185" WIDTH="19"/>
<String ID="P21_ST00006" HPOS="455" VPOS="155" WIDTH="114" 
HEIGHT="26" CONTENT="earned" WC="0.95" CC="000200"/>
<SP ID="P21_SP00004" HPOS="569" VPOS="185" WIDTH="18"/>
<String ID="P21_ST00007" HPOS="587" VPOS="155" WIDTH="49" 
HEIGHT="27" CONTENT="the" WC="1.00" CC="000"/>
<SP ID="P21_SP00005" HPOS="636" VPOS="185" WIDTH="21"/>
<String ID="P21_ST00008" HPOS="657" VPOS="155" WIDTH="63" 
HEIGHT="27" CONTENT="love" WC="0.83" CC="0600"/>
```

#Solution
##Distributed Version Control (Git)
 * Logs the history of every edit, editor, revision
 * Stores the complete state of the text at every revision
 * Decentralized: repository-agnostic 
 * Allows for democratic scholarly editing

#Git is MacGyver

![Git is MacGyver](macgyver.jpg)

#Solution 
##Git Submodules
 * Allow arbitrary curation of texts into collections
 * E.g. 18th Century British poetry
 * Collections aren't necessarily mutually exclusive

#Solution 
##GitHub
 * Allows for easy discoverability of corpora
 * Enables issue tracking, charting of contributor networks
 * Allows for viewing and editing files directly in the browser

---

![](silos-gonna-macgyver.png)

#Solution
##ALTO -> Markdown -> Jekyll -> GitHub.io 
 * Allows casual editing of the text in markdown
 * Creates beautiful web editions for each text via github.io 

#Call for Contributions
 * Git-Lit is an open project: anyone can get involved
 * Even non-coders! Also a good way to learn coding
 * Community-based; benefits everyone
 * Visit <https://github.com/git-lit> to get started
 * Or email me: jonathan.reeve@columbia.edu
 * These slides: <http://jonreeve.com/presentations/media-res2/>
