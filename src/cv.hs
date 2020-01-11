import Network.URI

data Education = Education {
  university :: Text,
  degree :: Text,
  field :: Text,
  date :: Date
  } deriving Show

data Date = Date {
  year :: Int,
  month :: Int
  } deriving Show

-- Validate the dates.
date :: Int -> Int -> Date
date y m | y < 1981 || y > 2100 = error "You're probably not alive in the year" ++ (show y)
         | m < 1 || m > 12 = error "The month " ++ (show m) ++ " doesn't exist."
         | otherwise = Date y m

data Project = Project {
  title :: Text,
  role :: Role,
  url :: URI,
  github :: URI,
  start :: Date,
  desc :: Markdown,
  updates :: [ Update ]
  } deriving Show

data Role = Creator | CoCreator | Developer | ResearchAssistant

data Update = Talk | Publication | Award | News

data Talk = Talk {
  date :: Date,
  title :: Text,
  url :: URI,
  venue :: Text
  } deriving Show

data Publication = Publication {
  date :: Date,
  title :: Text,
  venue :: Link,
  pubType :: PublicationType
  } deriving Show

data PublicationType = Tutorial | Article | Chapter | Abstract

data Link = Link { url :: URI, text :: Text }

data Award = Award {
  date :: Date,
  award :: Text,
  awardedBy :: Link
  } deriving Show

data News = News {
  date :: Date,
  desc :: Markdown
  } deriving Show

uni :: Text -> Text
uni abbr | "nyu" = "New York University"
         | "cu"  = "Columbia University"
         | "bc"  = "Brooklyn College"
         | "msu" = "Montclair State University"

education = [
  Education (uni "nyu") "Bachelor of Arts" "Interdisciplinary" (date 2004 1),
  Education (uni "bc") "Master of Arts" "English Literature" (date 2010 05),
  Education (uni "nyu") "Master of Arts" "Humanities" (date 2014 01),
  Education (uni "cu") "Master of Arts" "English and Comparative Literature" (date 2016 05),
  Education (uni "cu") "Master of Philosophy" "English and Comparative Literature" (date 2019 05),
  Education (uni "cu") "PhD Candidate" "English and Comparative Literature" (date 2019 05)
  ]

projects = [
  Project { title = "Open-Editions",
            role = Creator,
            url = "https://github.com/open-editions/",
            github = "open-editions/corpus-joyce-ulysses-tei",
            description = "Open-source, semantically annotated scholarly editions of literary texts.",
            start = date 2015 09,
            updates = [
              Update Talk { date  = date 2018 01,
                            title = "Open-Source Scholarly Editions of Works by James Joyce",
                            venue = uni "msu"},
              Update Talk { date  = date 2017 10,
                            title = "Contributing to the Open Critical Editions of James Joyce",
                            venue = "Joyce in the Digital Age Conference, Columbia U"}
              ]
          },
  Project { title = "Corpus-DB",
            role = Creator,
            url   = "https://github.com/JonathanReeve/corpus-db",
            github = "JonathanReeve/corpus-db",
            start = date 2017 03,
            desc  = "A database and API for plain text archives, for digital humanities research.",
            updates = [
                Update Award (date 2018 01) "grant awarded" (Link "NYC-DH" "https://nycdh.org/"),
                Update Award (date 2017 10) "winner" $
                  (Link "2017 NYCDH Graduate Student Project Award"
                   "https://nycdh.org/groups/nycdh-announcements-71439400/forum/topic/2017-nycdh-graduate-student-project-award-recipients/" )
                Update Award (date 2017 09) "awarded" $
                  (Link "2017 Columbia University Libraries Digital Centers Internship" "")
                ]
          },
  Project { title = "Middlemarch Critical Histories",
            role = CoCreator,
            url   = "https://github.com/lit-mod-viz/middlemarch-critical-histories",
            github= "lit-mod-viz/middlemarch-critical-histories",
            start = date 2016 01,
            desc  = "Computational analyses of the critical history of George Eliot's novel _Middlemarch_. In collaboration with Milan Terlunen, Sierra Eckert, Columbia University’s [Group for Experimental Methods in the Humanities](http://xpmethod.plaintext.in/), and the Stanford Literary Lab.",
            updates = [
              Update Talk { date = (date 2017 08),
                            title = "Frequently Cited Passages Across Time: New Methods for Studying the Critical Reception of Texts",
                            url = "https://github.com/lit-mod-viz/middlemarch-critical-histories/blob/master/papers/dh2017-poster/main.pdf)",
                            venue = "[Digital Humanities 2017](https://dh2017.adho.org/)"
                          }
              Update Publication { date = (date 2017 10),
                                    pubType = Abstract,
                                    title = "Frequently Cited Passages Across Time: New Methods for Studying the Critical Reception of Texts",
                                    url = "https://github.com/xpmethod/middlemarch-critical-histories/blob/1359d403c8c8655170babb6e1bf8f81bcb4bc0c9/dh2017-submission/middlemarch-abstract.pdf",
                                    venue = Link "Proceedings of Digital Humanities 2017" "https://dh2017.adho.org/"
                                  },
              Update Talk { date = date 2017 02,
                            title = "Middlemarch Critical Histories: Initial Findings",
                            venue = Link "Stanford Literary Lab" "" }
              ]},

  Project { title = "A Generator of Socratic Dialogues",
            role = Creator,
            description = "A generator of Socratic dialogues, using meta-Markov chains to emulate character speech.",
            url =  "http://jonreeve.com/2016/10/socratic-dialogue-generator/",
            start = date 2016 10,
            end = date 2016 11,
            updates = [
              Update Award { date = (date 2017 02),
                              award = "Winner, Best Use of DH For Fun",
                              awardedBy = Link "2016 DH Awards" "http://dhawards.org/dhawards2016/results/"}
              ]},

  Project { title =  "Git-Lit",
            description =  "A Project to Parse, Version Control, and Publish ~50,000 British Library Electronic Books",
            role = Creator,
            start = date 2015 08,
            url =  "https://git-lit.github.io/",
            updates = [
              Update Talk (date 2017 1)
                "Git-Lit: an Application of Distributed Version Control Technology Toward the Creation of 50,000 Digital Scholarly Editions"
                (Link "Modern Language Association Convention" "")
              Update Award (date 2016 07) "Winner, student bursary"
                (Link "Association of Digital Humanities Organizations" "")
              Update Publication { date = date 2016 07,
                                   title = "Git-Lit: an Application of Distributed Version Control Technology Toward the Creation of 50,000 Digital Scholarly Editions",
                                   url = "http://dh2016.adho.org/abstracts/335)",
                                   venue = Link "Digital Humanities 2016: Conference Abstracts. Kraków: Jagiellonian University & Pedagogical University, 2016."
                                           "http://dh2016.adho.org/static/dh2016_abstracts.pdf)"
                                 }
              Update Talk { date = date 2016 7,
                            title =  "Git-Lit: an Application of Distributed Version Control Technology Toward the Creation of 50,000 Digital Scholarly Editions",
                            venue = Link "Digital Humanities 2016, Kravów, Poland" "http://dh2016.adho.org/"
                          },
              Update Talk { date = date 2016 04,
                            title = "Applications of Distributed Version Control Technology to the Creation of Digital Scholarly Editions",
                            url = "http://jonreeve.com/presentations/sts2016/",
                            venue = Link "Society for Textual Scholarship, Ottawa" ""}
              Update Talk { date = date 2015 11,
                            title = "Git-Lit: An Application of Distributed Version Control Systems Towards the Creation of 50,000 Digital Scholarly Editions",
                            url = "http://jonreeve.com/presentations/media-res2/",
                            venue = Link "Media Res 2, New York University"
                              "https://digitalfellows.commons.gc.cuny.edu/2015/11/12/media-res-2-nyc-dh-lightning-talks/"}
                ]
          },

  Project { title = "Annotags",
            description = "A protocol for a literary metadata hashtag that provides the ability to livetweet books, electronic documents, and other texts.",
            role = Creator,
            start = date 2014 01,
            url = "http://annotags.github.io/",
            github = "Annotags/annotags.js",
            updates = [
              Update News (date 2014 09) "[Web app calculator](/projects/annotags) released",
              Update Award (date 2015 02) "Nominated for Best DH Tool" (Link "DH Awards, 2014" "http://dhawards.org/dhawards2014/results/")
              Update Award (date 2015 06) "Featured resource" (Link "Digital Humanities Now"
                "http://www.digitalhumanitiesnow.org/2015/07/resource-annotags-a-decentralized-textual-annotation-protocol/")
              Update Talk (date 2015 05) "Annotags: A Decentralized Literary Annotation Protocol"
                (Link "I Annotate 2015" "http://iannotate.org/")
              Update Talk {
                  date = date 2015 06,
                  title = "Annotags: A Decentralized Literary Annotation Protocol",
                  url = "http://www.jonreeve.com/presentations/keydh2015",
                  venue = Link "Keystone Digital Humanities conference" "" }
              ]
          },

  Project { title = "Chapterize",
            role = Creator, 
            start = date 2016 08,
            description = "A command-line tool for breaking a text into chapters.",
            url = "https://github.com/JonathanReeve/chapterize",
            github = "JonathanReeve/chapterize",
            pypi = "chapterize"
          },

  Project { title = "Macro-Etymological Text Analysis",
            role = Creator,
            description = "Computational methods applying etymology and language history to textual analysis.",
            start = date 2013 09,
            url = "http://github.com/JonathanReeve/macro-etym", 
            github = "JonathanReeve/macro-etym", 
            pypi = "macroetym",
            updates = [
              Update News (date 2015 01) "[Featured](http://www.tapor.ca/?id=470) in the [Text Analysis Portal for Research](http://www.tapor.ca/)"
              Update News (date 2015 01) "Listed in Alain Liu's directory of Digital Humanities Tools"
              Update News (date 2015 03) "Featured in the [Digital Research Tools](http://www.dirtdirectory.org/) (DiRT) directory"
              Update News (date 2015 05) "Released as [a command-line program in the Python Package Archive](https://pypi.python.org/pypi/macroetym)"
              Update Publication { date = date 2016 01,
                                   title = "A Macro-Etymological Analysis of James Joyce's _A Portrait of the Artist as a Young Man_",
                                   venue = "Reading Modernism with Machines, Palgrave Macmillan",
                                   pubType = Chapter },
              Update Award (date 2014 08) "Winner, student bursary" (Link "Association of Digital Humanities Organizations" "")
              Update Award (date 2014 07) "Winner" (Link "New York University Hirschhorn Thesis Award"
                                                    "https://draperprogram.wordpress.com/2014/06/12/congratulations-to-our-hirschhorn-award-nominees-and-winner/")
              Update Talk { date = date 2014 04,
                            title = "Macro-Etymological Textual Analysis: a Computational Application of Language History to Literary Criticism",
                            url = "http://jonreeve.com/presentations/dh2014/",
                            venue = Link "Digital Humanities 2014, Lausanne, Switzerland" "http://dh2014.org/" }
              ]
          },

  Project { title = "Text-Matcher",
            role = Creator,
            desc = "Fuzzy text matching and alignment algorithms.",
            start = date 2015 01,
            url = "http://github.com/JonathanReeve/text-matcher",
            github = "JonathanReeve/text-matcher",
            updates = [
              Update News (date 2016 10) "[Released command-line tool on the Python Package Archive](https://pypi.python.org/pypi/text-matcher)"
              Update News (date 2015 05) "[Modernism, Myth, and their Intertextualities: a Computational Detection of Biblical and Classical Allusion in the English-Language Novel, 1771-1930](https://github.com/JonathanReeve/allusion-detection/blob/master/paper/essay.pdf)"
              ]
          },

 Project { title = "Customeka",
           role = Creator,
           desc = "A highly customizable theme for the Omeka content management system.",
           start = date 2013 01,
           end = date 2015 01,
           url = "http://github.com/JonathanReeve/theme-customeka",
           github = "JonathanReeve/theme-customeka", 
           updates = [
             Update Publication { date = date 2016 08,
                                  pubType = Tutorial,
                                  title = "Installing Omeka",
                                  url = "https://programminghistorian.org/en/lessons/installing-omeka",
                                  venue = Link "The Programming Historian" ""
                                },
             Update Talk (date 2012 10) "Elementaire, a Customizable Omeka Theme"
               Link "The Humanities and Technology Camp" "http://newyork2012.thatcamp.org/"
             ]
         }
 ]

positions = [
      Position { org = "DH Box",
                 role = "Developer",
                 start = date 2015 11,
                 end = date 2017 07,
                 updates = [
                   Update News (date 2016 07) "Developed [corpus, a textual corpus downloader](https://github.com/DH-Box/corpus-downloader)"
                   Update News (date 2017 08) "Integrated textual corpus download functionality into the [DH Box web app](http://dhbox.org)"
                   Update News (date 2017 09) "Created [DH-USB, a digital humanities operating system designed to run from a USB drive](https://github.com/DH-Box/dh-usb)"
                   ]
          },

      Position { org ="Institute for Comparative Literature and Society, Columbia University",
                 project = "A Safer Online Public Square",
                 role = "Research Assistant, Computational Methods of Abusive Language Detection",
                 url = "http://icls.columbia.edu/initiatives/a-safer-online-public-square/",
                 start = date 2017 06,
                 end = date 2018 07
               },

      Position { project = "GVSHP Image Archive",
                 org = "Greenwich Village Society for Historic Preservation",
                 start = date 2015 05,
                 end = 2015 10,
                 updates = [ Update News (date 2015 10)
                             "Developed the [Greenwich Village Society for Historic Preservation Image Archive](http://archive.gvshp.org), an Omeka-based image archive"
                           ]
               },

      Position { project = "MLA Commons",
                 org = "Modern Language Association",
                 role = "Web Developer",
                 start = date 2013 11,
                 end = date 2015 08,
                 updates = [ Update News (date 2013 11) "Edited XML and XSL code for the [Literary Research Guide](http://mlalrg.org/public)",
                             Update News (date 2014 01) "Contributed to software used for the electronic edition of [_Literary Studies in the Digital Age_](http://dlsanthology.commons.mla.org/), and [_Digital Pedagogy in the Humanities_](https://digitalpedagogy.commons.mla.org/)",
                             Update News (date 2014 08)
                               "Primary contributor to [the 2014 and 2015 redesigns](http://updates.commons.mla.org/2014/07/28/new-theme-for-the-mla-commons/) of the [MLA Commons](http://commons.mla.org)"
                             Update News (date 2015 01)
                               "Rewrote [cbox-auth](https://github.com/mlaa/cbox-auth), the authentication system of the _MLA Commons_, to interface with the MLA member database",
                             Update News (date 2015 03)
                               "Designed [a WordPress plugin for visualizing patterns of disciplinarity](https://github.com/mlaa/bp-group-statistics) between _MLA Commons_ interest groups"
                           ]
               },

      Position { project = "DOCMAP",
                 org = "New York University History Department",
                 role = "Web developer",
                 start = date 2012 06,
                 end = date 2015 01,
                 updates = [ Update News (date 2012 09)
                               "Developed [a customization engine](https://github.com/JonathanReeve/theme-customeka) for the [Omeka content management platform](http://omeka.org)",
                             Update News (date 2015 01)
                                "Designed and developed Omeka themes deployed on [Greenwich Village History](http://gvh.aphdigital.org) and [New Jersey Digital History](http://njdigitalhistory.org/NJDHA/)"
                           ]
               },

      Position { project = "The Manifesto in Literature",
                 org = "Thomas Riggs & Company, Publishers",
                 role = "Research Writer",
                 start = date 2012 07,
                 end = date 2012 09,
                 updates = [
                   Update News (date 2012 09)
                     "Authored six articles for the 2013 reference volume [*The Manifesto in Literature*](http://www.thomasriggs.net/pages/content/index.asp?PageID=158): Richard Stallman's \"The GNU Manifesto,\" John Barlow's \"A Declaration of the Independence of Cyberspace,\" and several others"
                   ]
               }
      ]

data Teaching = Teaching { start :: Date,
                           end :: Maybe Date,
                           teachingType :: TeachingType,
                           name :: Text,
                           venue :: Text,
                           url :: URI,
                           notes :: Maybe Text
                           } deriving Show

data TeachingType = Workshop | Course | TA

teaching = [ Teaching { start = date 2018 09,
                        end = date 2018 12,
                        teachingType = Course,
                        name = "Literary Texts and Critical Methods",
                        venue = "Department of English and Comparative Literature, Columbia U"
                      },
             Teaching { start = date 2018 07,
                        end = date 2020 08,
                        teachingType = Course,
                        name = "Introduction to Computational Literary Analysis",
                        url = "https://github.com/JonathanReeve/course-computational-literary-analysis",
                        venue = "Digital Humanities Summer Minor Program, U California, Berkeley"
                      },
             Teaching { start = date 2018 06,
                        end = date 2019 06,
                        teachingType = Workshop,
                        name = "Web APIs with Python",
                        venue = Link "Digital Humanities Summer Institute, U Victoria, Canada" "https://dhsi.org",
                        notes = "Co-taught with Stephen Zweibel, Patrick Smyth, and Jojo Karlin" },
             Teaching { start = date 2017 11,
                        teachingType = Workshop,
                        name = "Text Analysis with SpaCy and Scikit-Learn",
                        url = "https://pydata.org/nyc2017/schedule/presentation/51/",
                        venue = Link "PyData NYC, Microsoft" "https://pydata.org/"
                      },
             Teaching { start = date 2017 09,
                        end = date  2019 05,
                        teachingType = Course,
                        name = "University Writing with Readings in the Data Sciences",
                        venue = Link "Department of English and Comparative Literature, Columbia U" ""
                      },
             Teaching { start = date 2017 11,
                        teachingType = Workshop,
                        name = "An Introduction to Semantic Markup and Computational Analysis of _Ulysses_",
                        venue = Link "Finnegan's Waves, U Buffalo"
                                  "https://www.buffalo.edu/ubnow/stories/2017/10/finnegans-waves.html"
                      },
             Teaching { start = date 2017 04,
                        teachingType = Workshop,
                        name = "An Introduction to Text Analysis and Visualization",
                        url = "https://github.com/JonathanReeve/dataviz-workshop-2017",
                        venue = Link "Art of Data Visualization Week, Columbia U"
                                  "http://library.columbia.edu/news/events/art-or-knowledge/dv_program.html"
                      },
             Teaching { start = 2017 02,
                        teachingType = Workshop,
                        name = "Advanced Text Analysis with SpaCy and TextaCy",
                        url = "https://github.com/JonathanReeve/advanced-text-analysis-workshop-2017/blob/master/advanced-text-analysis.ipynb",
                        venue = Link "2017 NYCDH Week, NYU" "http://dhweek.nycdh.org/",
                        notes = "[Workshop notebook](https://github.com/JonathanReeve/advanced-text-analysis-workshop-2017/blob/master/advanced-text-analysis.ipynb) featured in the [SpaCy notebook collection](https://github.com/explosion/spacy-notebooks)"
                      },
             Teaching { start = date 2016 09,
                        end = date 2016 12,
                        teachingType = TA,
                        venue = Link "Department of English and Comparative Literature, Columbia U" "",
                        name =  "James Joyce",
                        notes = "Led a weekly discussion section in Joyce’s _Ulysses_"
                      },
             Teaching { start = date 2016 04,
                        teachingType = Workshop,
                        name = "An Introduction to Version Control with Git",
                        venue = Link "Society for Textual Scholarship"
                          "https://textualsociety.org/current-conference-program/"
                      },
             
  - date: 2016-04
    type: workshop
    name: An Introduction to Visualizing Text with Python
    venue: "[Art of Data Visualization](http://library.columbia.edu/news/events/data-visualization.html), Columbia U"
  - date: 2016-02
    type: workshop
    name: '["Text Editing with Vim"](http://dhweek.nycdh.org/event/lightning-fast-text-editing-with-vim/)'
    venue: "[NYCDH Week 2016](http://dhweek.nycdh.org/), Columbia U"
  - date: 2016-01 — 2016-05
    type: course (TA)
    name: History of the English Language
    venue: "Department of English and Comparative Literature, Columbia U"
  - date: 2012-09 — 2012-12
    type: course (TA)
    name: Computing in the Humanities
    venue: "Department of English and Comparative Literature, Columbia U"
    notes:
      - Lectured in BASH scripting, command-line image manipulation
  - date: 2012-11 — 2012-05
    type: course
    name: "Writing 300: Research Paper Writing"
    venue: "English Department, City U of New York, York College"
    notes:
      - Taught two sections per semester
  - date: 2010-11 — 2011-05
    type: course
    name: "Reading 100, Writing 100"
    venue: Vietnam National U, Hanoi, Vietnam
  - date: 2009-08 — 2010-06
    type: course
    name: English 1 (composition, nonfiction readings); English 2 (composition, readings in literature)
    venue: City U of New York, Brooklyn College
  - date: 2005-03 — 2006-01
    type: course
    name: Reading 100, Writing 100
    venue: 'Missouri State U, Branch Campus, Dalian, China'

affiliations:
 - Active member, [Literary Modeling and Visualization Lab](http://xpmethod.plaintext.in/projects/literary-modeling.html), [Group for Experimental Methods in the Humanities](http://xpmethod.github.io/), Columbia U
 - Founding member, [Digital Literary Stylistics Special Interest Group](https://dls.hypotheses.org/), Association of Digital Humanities Organizations
 - Reviewer, [Digital Humanities 2018](https://dh2018.adho.org/en/) conference
 - Reviewer, [_Digital Scholarship in the Humanities_](http://dsh.oxfordjournals.org/) (formerly _Literary and Linguistic Computing_), Oxford UP
 - Reviewer, [_Journal of Data Mining and Digital Humanities_](http://jdmdh.episciences.org/)
 - Editor-At-Large, [Digital Humanities Now](http://digitalhumanitiesnow.org/)
 - "Member of professional organizations: Modern Language Association; Association for Computers in the Humanities; Society for Textual Scholarship; Humanities, Arts, and Sciences Advanced Collaboratory"
