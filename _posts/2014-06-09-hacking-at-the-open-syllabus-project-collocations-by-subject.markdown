---
layout: post
title: ! 'Hacking at the Open Syllabus Project: Collocations by Subject'
categories:
- Digital Humanities
tags: 
- natural language processing
- python
- text analysis
---

I was invited to hack around on the [Open Syllabus project](http://opensyllabusproject.org/) this past Saturday, which I was really excited to do. They've scraped the web and come up with around 1.5 million syllabi, and only just released their API to researchers this weekend. I wanted to run some computational analyses on these syllabi, to attempt questions like:

 * What were the most frequently assigned texts in freshman composition courses?
 * What disciplines exhibit the most variance between their syllabi? That's to say, which subjects have the most similar syllabi, and which have the most divergent? 
 * What disciplines have the longest syllabi? 
 * In which disciplines do technological marker words like "blog" or "Twitter" appear most frequently? 

To start with, I used a JSON file containing a subset of these syllabi--around 1000, and tagged them by subject, using their first lines and filenames as hints. ([See the quick and dirty code here](https://github.com/JonathanReeve/opensyllabus/blob/master/nltk_experiments/syl-data.py).) I then imported another 600 subject-tagged syllabi from [Graham Sack](https://github.com/grahamsack)'s corpus, resulting in a subject-tagged corpus of around 1,300 syllabi. From there, I sorted the results by subject and ran it through [NLTK](http://www.nltk.org) to find collocations--words that frequently occur next to each other. 

There were some interesting findings. Some were predictable, like "mineral resources" for Geology, "corale room" for Music, or "Homeric Hymn" for Mythology. Others were revealing about required texts frequently assigned for these subjects—Michel Foucault is likely frequently assigned in Sociology courses, "Beyond Good" suggests that Nietzsche's _Beyond Good and Evil_ is required reading in Philosophy courses, and the truncated "nhead Wilson" suggests that Twain's tragedy—although here with a mistaken word boundary—makes a frequent appearance in Literature courses. 

Some of the collocations are a little surprising, however. It's a little surprising that "Hebrew Bible" appears as the highest-rated collocation in Literature syllabi. It's even more surprising that "Gospel According" appears in Political Science syllabi. (Then again, since many of these syllbi come from Emory University, these may be features consistent with a Bible Belt institution.)

There are some stylistic features that deserve note here, too. Syllabi in the natural sciences exhibit the most use of ALL CAPS by a wide margin. Could it be that scientists have less exposure to the style manuals—those so loved by humanists—that discourage this style? Or is it possible that there is something about the exacting nature of the hard sciences that demands this kind of emphasis? 

Chemistry syllabi featured some of these all caps collocations, and also featured the highest proportions of bigrams characteristic of course policy paragraphs: "make sure," "honor code," "usual penalty," and "found guilty." What is it about Chemistry as a discipline, I feel like asking, that makes it so strict in its policies? This could, of course, be an instance of a department-wide requirement for all syllabi to include a paragraph of boilerplate course policy text (this was a requirement for the composition courses I taught at CUNY). 

Faced with these preliminary results, here are a few questions I ended up asking, with some hints as to their answers: 

 * Which disciplines care the most about dress code? Physical Education and Music. 
 * Which language course is the most concerned with grammatical accuracy? German. 
 * What is the most-mentioned historical period in (this small subset of) history courses? The Civil Rights Era. 
 * What discipline is concerned the most with weather? Geology. (Owing, probably, to class field trips.)

Of course, since this data set is so severely limited, and since this analysis was so haphazard, none of these answers may be considered even halfway accurate. Still, I achieved an even more important goal in this process—to generate more questions. 

You can read the original output of the command [here in the shared Google Drive folder we used](https://drive.google.com/?authuser=0#folders/0B7WRJQdqro24eHgxVXA2YUdlM1U), run the script yourself from [the code on github](https://github.com/JonathanReeve/opensyllabus/tree/master/nltk_experiments), or check out this heavily-curated selection of the findings, organized by subject: 

**Astronomy**: astronomers know; investigating nature; natural sciences; ultimate fate.

**Biology**: LAB EXAM; Cellular respiration; procedures outlined; FINAL EXAMINATION; SPRING BREAK. 

**Chemistry**: Honor Code; Honor Code.; liberal arts; College Honor; Make sure; honor code; academic honesty; HONOR CODE; highest standards; usual penalty; found guilty. 

**Computer science**: selected material; new StringBuffer; UNIX operating; constructor call. 

**English**: Rhetorical Analyses; unauthorized information. 

**Film Studies**: reserve Screening; Citizen Kane; Experimental Film; Francis Ford; Jean-Luc Godard; Maltese Falcon; Orson Welles; Royal Tenenbaums. 

**Geology**: SPRING BREAK; scientific method; unsure whether; Mineral Resources. 

**German**: grammatical accuracy. 

**Hist**: Civil Rights; World War; Luther King; Martin Luther; National Historic; University Press; Black Folk; Field Trip.

**Literature**: Hebrew Bible; Invisible Man; Kingsblood Royal; Black Folk; nhead Wilson; online lectures; Frederick Douglass; Human Stain; exegetical paper; 

**Music**: aesthetic sense; Chorale Room; Old Church; Black dress; Chamber Ensemble; 

**Mythology**: Classical Mythology; Homeric Hymn; Friedrich Nietzsche; Irrational Hero; Roman Appropriation; Sigmund Freud; Using Madness; 

**Philosophy**: Nicomachean Ethics; Beyond Good; Categorical Syllogisms; Ordinary Language; 

**Physical Education**: medical condition; body composition; dressed appropriately; include vigorous; full participation; physical fitness; Skill Test; Tai Chi; 

**Policial science**: University Press; New York; Conflict Resolution; Communist Manifesto; Second Treatise; Gospel According

**Psychology**: Think Critically; mental processes; critical thinking; Psychological Disorders; Human Development; theoretical perspectives; system; appropriate documentation. 

**Public Relations**: well organized; rewrite opportunities; Case Study; Social Media; Personal brand; 

**Religion**: Native American; site visit; Reflective Analysis; 

**Sociology**: Michel Foucault
