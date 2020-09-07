#Collaborative, Open-Source Creation of Digital Editions 

#Principles

 * Archive-ready and future-proof texts. 
 * Open-source development. 
 * Simple software stack. 
 * Repeatable software stack. 
 * Open-access annotation. 

#Problem: Markup

 * Most digital editions integrate form and content in a way that prohibits future derivative editions. 
 * Consider this HTML 3.0 markup: 

```html
There are very few <i>risqué</i> passages in <i>Paradise Lost</i>
```

 * doesn't differentiate between the two types of italicized text
 * this is a major future-proofing problem

#Markup Solution 
 * TEI XML is a _semantic_ markup language. 
 * It doesn't describe how text should look, but what the text _is_. 
 * "Teaches the computer to read the text." --- crucial step for computational literary criticism 
 * This makes it easier to handle in the future. 

#TEI XML Example
```xml
There are very few <foreign>risqué</foreign> 
passages in <title>Paradise Lost</title> 
```

#Problem: Closed-Source

 * Many digital editions are centralized, closed-source, and locked down, unable to be edited by third parties.
 * If the project dies (loses funding, gets hacked, loses its hosting provider, becomes obsolete), the edition also dies. 

#Open-Source Solution 

 * Make digital editions that are open-source and open-access (e.g. available on GitHub from the beginning). 
 * Allows anyone to fork (copy) the edition, make a change, and submit the change back to the original project, if desired. 
 * Allows for community-based improvement of the quality of the edition, crowdsourcing its creation. 
 * If the project dies, anyone may make a copy of the edition and continue it for themselves. 

#Problem: Data Storage

 * Most digital editions have their annotations and other features locked away in a database. 
 * Most WordPress, CommentPress, and Drupal editions have this problem. 
 * Difficult to download, difficult for text analysis, difficult to extract for future derivative editions. 

#Data Storage Solution 

 * Encode annotations in TEI XML, and programmatically export them to an annotation interface like Hypothesis. 
 * Allows for parallel static (TEI) and dynamic (database) data storage. 
 * Unlike WordPress, XML is future-proof, easily exportable to other formats (e.g. EPUB, PDF, etc). 

#Problem: Repeatability
 
 * Most digital edition projects are limited to one text or author. 
 * Whitman Archive, Rosetti Archive, Etc. 
 * Technology created for these editions must be heavily adapted or re-created to be used with other editions.

#Repeatability Solution 

 * Create a standard-compliant, abstracted technological framework and workflow that can easily be reused in the creation of other digital editions. 

#Problem: Interface
 
 * Many critical editions are so rich with notes and features that it becomes difficult to read the text. 

![](ship-of-theseus-3.jpg)

#Interface Solution 

 * TEI (with XSLT, jQuery) allows for programmatic selection of these extra features, allowing the user to control what he or she sees. 
 * Everything can be turned on or off. 

#Annotation Platforms

 * OpenBook (obsolete)
 * CommentPress (based on WordPress) 
 * Genius (proprietary)
 * Drupal Annotate (requires Drupal)
 * Annotator.js (not bad)  
 * Hypothesis h (not bad) 

#A (Proposed) Digital Edition Stack

 * **The text**: TEI XML
 * **The interface**: repeatable XSLT, jQuery
 * **Version control**: git
 * **Project management platform, open-source platform for crowdsourcing community contributions**: GitHub or analogous git-based platform 
 * **Open-access annotation platform**: locally installed Hypothesis `h` container

#What to do about PDFs / Facsimile Editions? 

 * Two methods. Not mutually exclusive, but may be chained. 

#Best Method

 * Extract text from PDF, maintaining page location information.
 * Encode into TEI XML. 
 * Write XSL to display words according to their locations on the original page. 
 * Extract images from PDF, and overlay on TEI (XSL -> HTML). 
 * Import existing footnotes into a local instance of Hypothesis H using the H HTTP API. 

#Fastest Method

 * Skip TEI encoding for now. 
 * Import existing footnotes into a local instance of Hypothesis H, but associate the annotations with the PDF rather than the TEI text. 

#Links

 * <http://git.io/v4kIw> (the edition homepage) 
 * <http://github.com/JonathanReeve/corpus-joyce-portrait-tei> (long form for the edition homepage)
 * <http://jonreeve.com/portrait/portrait.xml> (an alpha display build)
 * <http://jonreeve.com/presentations/digital-editions> (this presentation)
