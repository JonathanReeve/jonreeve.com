---
layout: post.pug
title: Introducing Git-Lit
category: digital humanities
tags:
 - text analysis
 - corpus linguistics
 - corpora
 - open source
 - git-lit
---

A vibrant discussion followed [my March 15th post, "A Proposal for a Corpus Sharing Protocol."](http://jonreeve.com/2015/03/proposal-for-a-corpus-protocol/). [Carrie Schroeder](https://twitter.com/ctschroeder), [Allen Riddel](https://twitter.com/ariddell) and others on Twitter pointed out that, especially in non-English DH fields, many corpora are already on GitHub. These include texts from the [Chinese Buddhist Electronic Text Association](https://github.com/cltk/chinese_text_cbeta_indices), the [Open Greek and Latin Project](http://github.com/OpenGreekAndLatin) at Leipzig, and papyri from the [Integrating Digital Papyrology Project](https://github.com/papyri/idp.data). The [Text Creation Partnership](http://www.textcreationpartnership.org/) has released some 25,000 of their texts in January of this year, and [uploaded them to GitHub](https://github.com/textcreationpartnership). One of the more interesting Git corpus projects I became aware of following this discussion is [GITenberg](http://gitenberg.github.io/). Led by [Seth Woodworth](https://github.com/sethwoodworth), the project scrapes a text from Project Gutenberg, initializes a git repository for it, adds README and CONTRIBUTING files generated from the text's metadata, and uploads the resulting repository to GitHub. They have gitified around 43,000 works this way. The project also converts Project Gutenberg vanilla plain text into [ASCIIDOC](https://en.wikipedia.org/wiki/AsciiDoc)---a good example of this is the GITenberg edition of [The Adventures of Huckleberry Finn](https://github.com/GITenberg/Adventures-of-Huckleberry-Finn_76/blob/master/book.asciidoc). This is an amazingly ambitious project that holds the promise of wide-ranging applications for editing, versioning, and disseminating literature.

One such application might lie with [the 68,000 digital texts recently created by the British Library](http://labs.bl.uk/Digital+Collections+-+Books+and+Text). James Baker, a digital curator of the British Library, left a comment on my original post, suggesting that the method I describe might be used to parse and post the Library's texts. He sent me [a few sample texts](https://github.com/JonathanReeve/git-lit/tree/master/data) of the ALTO XML documents that the Stanford Literary Lab had used. [I adapted some of the GITenberg code to read these texts, generate README files for them, and turn them into GitHub repositories](https://github.com/JonathanReeve/git-lit). I'm provisionally calling this project Git-Lit.

#Introducing Git-Lit

Git-Lit aims to parse, version control, and post each work in the British Library's corpus of digital texts. Parsing the texts will transform the machine-readable metadata into human-readable prefatory material; version controlling the texts will allow for collaborative editing and revision of the texts, effectively crowdsourcing the correction of OCR errors; and posting the texts to GitHub will ensure the texts' visibility to the greater community.  

##Why This is Important

Git-Lit addresses these issues:

1. **Electronic Texts are difficult to edit.** Imagine you're reading an e-text on a Kindle, and you notice an OCR error. How can you fix it? You can alert Amazon, who may or may not forward your message to the publisher. Since correcting the text would require the publisher to recompile and resubmit it, they will likely decide it isn't worthwhile to make the correction. Similarly, if you notice an error in a Project Gutenberg text, it could take years for the correction to make its way back to the original text, even if you're an active member of their Distributed Proofreaders collective. In both cases, there does not yet exist an efficient, streamlined way to improve the quality of electronic texts. What is needed, therefore, is an open-source, decentralized model for community-centered editing. This model already exists for software development in the form of [git](https://git-scm.com/book/en/v2/Getting-Started-About-Version-Control). By posting a text to GitHub, we can take advantage of the fork/revise/pull request workflow that programmers have long enjoyed for software collaboration.  
2. **Textual corpora are difficult to assemble.** With some exceptions (notably the NLTK corpus module), downloading a text corpus involves compiling texts from any number of heterogeneous sources. A would-be text analyst must click through a series of web pages to find the corpus he or she wants, and then either download a .zip file that must be expanded, or email the corpus assembler for a copy of the corpus. With multiple texts, this can be a labor-intensive process that is not easily scriptable or automated. Git provides an easy way to solve these problems. By making texts available through the git protocol on GitHub, anyone that wishes to download a text corpus can simply run `git clone` followed by the repository URL. Parent repositories can then be assembled for collections of texts using git submodules. That's to say, a parent corpus repository might be created for nineteenth-century _Bildungsromane_, for instance, and that repository would contain pointers to individual texts that themselves are their own repositories.
3. **ALTO XML is not very human-readable.** ALTO XML, the OCR output format used by the British Library, the Library of Congress, and others, is extremely verbose. It encodes the location of each OCRed word, and often gives the OCR certainty for each word. This is great for archival purposes, but isn't an ideal starting-point for the kinds of text analysis typically done in the digital humanities. What we need is a script to transform this verbose XML into a human-readable format like ASCIIDOC that maintains as many of the original features of the text as possible.   

##How it Works

A British Library text contains ALTO XML textual data as well as a Library of Congress METS XML metadata file. Git-Lit does the following:

1. Reads the metadata file to determine the text's title, author (if any), and other pertinent information.
2. Initializes an empty git repository within the text directory, and makes an initial commit containing the text at its raw state.
3. Generates a README file with the metadata, a CONTRIBUTING file explaining how to contribute towards improving the text, and a LICENSE file which, for now, is just the standard GNU Public License v3.
4. Commits these new files to the git repository, effectively creating a new version.
5. Creates and pushes a new GitHub repository for the text.

I ran Git-Lit on [the four sample texts in the `data` directory](https://github.com/JonathanReeve/git-lit/tree/master/data), and generated the four GitHub repositories that can be found on [the Git-Lit organization](https://github.com/Git-Lit). You can read, fork, modify, or comment on [the IPython Notebook that does this](https://github.com/JonathanReeve/git-lit/blob/master/main.ipynb) on [the project repository at GitHub](https://github.com/JonathanReeve/git-lit).

##Future Phases of this Project

As this project develops, we'll create indices for the texts in the form of submodule pointers. Category-based parent repositories might include "17th Century Novels," "18th Century Correspondence," or simply "Poetry," but the categories are not mutually exclusive by necessity. This will allow a literary scholar interested in a particular category to instantly assemble a corpus by `git clone`ing the parent repository and checking out its submodules with `git submodule update --init --recursive`.

Later, we'll write a scripts to transform the texts in more useful formats, like ASCIIDOC and TEI XML. This will make archival-quality versions of the texts, and will allow for rich scholarly markup.

##How to Contribute

Please join this initiative! To contribute, contact me, or find an issue you can tackle on [the project issue tracker](https://github.com/JonathanReeve/git-lit/issues). Also, feel free to add your own features, restructure the code, or make any other improvements. Pull requests are very welcome!
