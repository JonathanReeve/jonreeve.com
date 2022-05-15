{-# LANGUAGE OverloadedStrings #-}

module JonReeve.CV.Projects where

import JonReeve.CV.Shared

data Project = Project
  { title :: Text,
    role :: ProjectRole,
    homepage :: URI,
    github :: Maybe Text,
    pypi :: Maybe Text,
    dateRange :: DateRange,
    desc :: Markdown,
    updates :: [Update]
  }
  deriving stock (Show)

data ProjectRole = Creator | CoCreator | Developer | Collaborator | ResearchAssistant deriving (Show)

dh2020 = Venue "Digital Humanities 2020" "https://dh2020.adho.org/" "Ottawa, CA [Virtual]"

projects :: [Project]
projects =
  [ Project
      { title = "Mapping Data Ethics",
        role = Collaborator,
        homepage = "https://data-ethics.tech",
        github = Just "https://github.com/JonathanReeve/data-ethics-literature-review",
        desc = "A survey of literature and curricula surrounding data science ethics.",
        dateRange = DateRange (date 2021 01) Present,
        updates =
          [ Update
              (date 2021 12)
              ( Publication
                  Article
                  "Mapping Data Ethics Curricula [Preprint]"
                  "https://github.com/JonathanReeve/data-ethics-literature-review/blob/main/paper/jices-paper.pdf" -- TODO: Add URL
                  ( Venue
                      "[Under Review at Journal of Information, Communication, and Ethics in Society]"
                      "https://www.emeraldgrouppublishing.com/journal/jices?id=jices"
                      ""
                  )
              ),
            Update
              (date 2021 07)
              ( Talk
                  "Teaching Data Ethics on the Bleeding Edge"
                  "https://github.com/JonathanReeve/data-ethics-literature-review/blob/main/conferences/aoir-abstract/abstract.pdf" -- TODO: Add URL
                  (Venue "Association of Internet Researchers Conference, 2021" "https://aoir.org/aoir2021confinfo/aoir2021cfp/" "")
              )
          ],
        pypi = Nothing
      },
    Project
      { title = "Open-Editions",
        role = Creator,
        homepage = "https://github.com/open-editions/",
        github = Just "open-editions/corpus-joyce-ulysses-tei",
        desc = "Open-source, semantically annotated scholarly editions of literary texts.",
        dateRange = DateRange (date 2015 09) Present,
        updates =
          [ Update
              (date 2021 01)
              ( Award
                  "Honorable Mention, Emerging Open Scholarship Awards"
                  (Venue "Canadian Social Knowledge Institute" "" "")
              ),
            Update
              (date 2020 05)
              ( Publication
                  Article
                  "Open Editions Online (a collaboration with Hans Walter Gabler)"
                  "https://muse.jhu.edu/article/756836"
                  ( Venue
                      "James Joyce Quarterly"
                      "https://jjq.utulsa.edu/"
                      "U Tulsa"
                  )
              ),
            Update
              (date 2019 01)
              ( Award
                  "Second runner-up, Best DH Data Set"
                  (Venue "2019 DH Awards" "http://dhawards.org/dhawards2019/results/" "")
              ),
            Update (date 2019 01) (News "[Joycewords.com](http://joycewords.com) released"),
            Update
              (date 2018 01)
              ( Talk
                  "Open-Source Scholarly Editions of Works by James Joyce"
                  "" -- TODO: Add URL
                  (Venue "" "" (uni "msu"))
              ),
            Update
              (date 2017 10)
              ( Talk
                  "Contributing to the Open Critical Editions of James Joyce"
                  "" -- TODO: Add URL
                  ( Venue
                      "Joyce in the Digital Age Conference"
                      "" -- TODO: Add URL
                      (uni "cu")
                  )
              )
          ],
        pypi = Nothing
      },
    Project
      { title = "Corpus-DB",
        role = Creator,
        homepage = "https://github.com/JonathanReeve/corpus-db",
        github = Just "JonathanReeve/corpus-db",
        dateRange = DateRange (date 2017 03) Present,
        desc = "A database and API for plain text archives, for digital humanities research.",
        updates =
          [ Update
              (date 2020 08)
              ( Publication
                  Abstract
                  "Corpus-DB: a Scriptable Textual Corpus Database for Cultural Analytics"
                  "https://dh2020.adho.org/wp-content/uploads/2020/07/604_CorpusDBaScriptableTextualCorpusDatabaseforCulturalAnalytics.html"
                  dh2020
              ),
            Update
              (date 2020 07)
              ( Talk
                  "Corpus-DB: a Scriptable Textual Corpus Database for Cultural Analytics"
                  "https://dh2020.adho.org/wp-content/uploads/2020/07/604_CorpusDBaScriptableTextualCorpusDatabaseforCulturalAnalytics.html"
                  dh2020
              ),
            -- Add CU Libraries talk here?
            Update (date 2018 01) (Award "micro-grant awarded" (Venue "NYC-DH" "https://nycdh.org/" "")),
            Update
              (date 2017 10)
              ( Award
                  "winner"
                  ( Venue
                      "2017 NYCDH Graduate Student Project Award"
                      "https://nycdh.org/groups/nycdh-announcements-71439400/forum/topic/2017-nycdh-graduate-student-project-award-recipients/"
                      ""
                  )
              ),
            Update
              (date 2017 09)
              ( Award
                  "awarded"
                  ( Venue
                      "2017 Digital Centers Internship"
                      "" -- TODO: Add URL
                      "Columbia University Libraries"
                  )
              )
          ],
        pypi = Nothing
      },
    Project
      { title = "Middlemarch Critical Histories",
        role = CoCreator,
        homepage = "https://github.com/lit-mod-viz/middlemarch-critical-histories",
        github = Just "lit-mod-viz/middlemarch-critical-histories",
        dateRange = DateRange (date 2016 01) Present,
        desc = "Computational analyses of the critical history of George Eliot's novel _Middlemarch_. In collaboration with Milan Terlunen, Sierra Eckert, Columbia University’s [Group for Experimental Methods in the Humanities](http://xpmethod.plaintext.in/), and the Stanford Literary Lab.",
        updates =
          [ Update
              (date 2017 10)
              ( Publication
                  Abstract
                  "Frequently Cited Passages Across Time: New Methods for Studying the Critical Reception of Texts"
                  "https://github.com/xpmethod/middlemarch-critical-histories/blob/1359d403c8c8655170babb6e1bf8f81bcb4bc0c9/dh2017-submission/middlemarch-abstract.pdf"
                  (Venue "Proceedings of Digital Humanities 2017" "https://dh2017.adho.org/" "Mexico City")
              ),
            Update
              (date 2017 08)
              ( Talk
                  "Frequently Cited Passages Across Time: New Methods for Studying the Critical Reception of Texts"
                  "https://github.com/lit-mod-viz/middlemarch-critical-histories/blob/master/papers/dh2017-poster/main.pdf"
                  (Venue "Digital Humanities 2017" "https://dh2017.adho.org/" "Mexico City")
              ),
            Update
              (date 2017 02)
              ( Talk
                  "Middlemarch Critical Histories: Initial Findings"
                  "" -- No URI
                  ( Venue
                      "Stanford Literary Lab"
                      "" -- TODO: Add url
                      "Stanford University"
                  )
              )
          ],
        pypi = Nothing
      },
    Project
      { title = "Literary Style Transfer",
        role = Collaborator,
        desc = "Experiments in the transfer of literary style among genres, using neural networks. A collaboration with Katy Gero, Chris Kedzie, and Lydia Chilton.",
        dateRange = DateRange (date 2019 05) (date 2019 10),
        homepage = "https://arxiv.org/abs/1911.03385",
        github = Nothing,
        pypi = Nothing,
        updates =
          [ Update (date 2019 11) $
              Publication
                Article
                "Low-Level Linguistic Controls for Style Transfer and Content Preservation"
                "https://arxiv.org/abs/1911.03385"
                (Venue "The 12th International Conference on Natural Language Generation" "https://www.inlg2019.com/" "Artificial Intelligence Research Center of Japan")
          ]
      },
    Project
      { title = "A Generator of Socratic Dialogues",
        role = Creator,
        desc = "A generator of Socratic dialogues, using meta-Markov chains to emulate character speech.",
        homepage = "http://jonreeve.com/2016/10/socratic-dialogue-generator/",
        github = Nothing,
        pypi = Nothing,
        dateRange = DateRange (date 2016 10) (date 2016 11),
        updates =
          [ Update
              (date 2017 02)
              ( Award
                  "Winner, Best Use of DH For Fun"
                  (Venue "2016 DH Awards" "http://dhawards.org/dhawards2016/results/" "")
              )
          ]
      },
    Project
      { title = "Git-Lit",
        desc = "A Project to Parse, Version Control, and Publish ~50,000 British Library Electronic Books",
        role = Creator,
        pypi = Nothing,
        github = Nothing,
        dateRange = DateRange (date 2015 08) (Present),
        homepage = "https://git-lit.github.io/",
        updates =
          [ Update
              (date 2017 1)
              ( Talk
                  "Git-Lit: an Application of Distributed Version Control Technology Toward the Creation of 50,000 Digital Scholarly Editions"
                  ""
                  ( Venue
                      "Modern Language Association Convention"
                      ""
                      "Philadelphia, PA"
                  )
              ),
            Update
              (date 2016 07)
              ( Award
                  "Winner, student bursary"
                  (Venue "Association of Digital Humanities Organizations" "" "Lausanne, Switzerland")
              ),
            Update
              (date 2016 07)
              ( Publication
                  Abstract
                  "Git-Lit: an Application of Distributed Version Control Technology Toward the Creation of 50,000 Digital Scholarly Editions"
                  "http://dh2016.adho.org/abstracts/335"
                  ( Venue
                      "Digital Humanities 2016: Conference Abstracts. Jagiellonian University & Pedagogical University, 2016."
                      "http://dh2016.adho.org/static/dh2016_abstracts.pdf"
                      "Kraków, Poland"
                  )
              ),
            Update
              (date 2016 7)
              ( Talk
                  "Git-Lit: an Application of Distributed Version Control Technology Toward the Creation of 50,000 Digital Scholarly Editions"
                  "" -- Did this have a URL?
                  (Venue "Digital Humanities 2016" "http://dh2016.adho.org/" "Kraków, Poland")
              ),
            Update
              (date 2016 04)
              ( Talk
                  "Applications of Distributed Version Control Technology to the Creation of Digital Scholarly Editions"
                  "http://jonreeve.com/presentations/sts2016/"
                  (Venue "Society for Textual Scholarship, Ottawa" "" "")
              ),
            Update
              (date 2015 11)
              ( Talk
                  "Git-Lit: An Application of Distributed Version Control Systems Towards the Creation of 50,000 Digital Scholarly Editions"
                  "http://jonreeve.com/presentations/media-res2/"
                  (Venue "Media Res 2" "https://digitalfellows.commons.gc.cuny.edu/2015/11/12/media-res-2-nyc-dh-lightning-talks/" (uni "nyu"))
              )
          ]
      },
    Project
      { title = "Annotags",
        desc = "A protocol for a literary metadata hashtag that provides the ability to livetweet books, electronic documents, and other texts.",
        role = Creator,
        dateRange = DateRange (date 2014 01) (date 2018 01),
        homepage = "http://annotags.github.io/",
        github = Just "Annotags/annotags.js",
        pypi = Nothing,
        updates =
          [ Update
              (date 2015 06)
              ( Award
                  "Featured resource"
                  (Venue "Digital Humanities Now" "http://www.digitalhumanitiesnow.org/2015/07/resource-annotags-a-decentralized-textual-annotation-protocol/" "")
              ),
            Update
              (date 2015 06)
              ( Talk
                  "Annotags: A Decentralized Literary Annotation Protocol"
                  "http://www.jonreeve.com/presentations/keydh2015"
                  (Venue "Keystone Digital Humanities conference" "" "")
              ),
            Update
              (date 2015 05)
              ( Talk
                  "Annotags: A Decentralized Literary Annotation Protocol"
                  ""
                  (Venue "I Annotate 2015" "http://iannotate.org/" "")
              ),
            Update
              (date 2015 02)
              ( Award
                  "Nominated for Best DH Tool"
                  (Venue "DH Awards, 2014" "http://dhawards.org/dhawards2014/results/" "")
              ),
            Update (date 2014 09) (News "[Web app calculator](/projects/annotags) released")
          ]
      },
    Project
      { title = "Chapterize",
        role = Creator,
        dateRange = DateRange (date 2016 08) (Present),
        desc = "A command-line tool for breaking a text into chapters",
        homepage = "https://github.com/JonathanReeve/chapterize",
        github = Just "JonathanReeve/chapterize",
        pypi = Just "chapterize",
        updates = []
      },
    Project
      { title = "Macro-Etymological Text Analysis",
        role = Creator,
        desc = "Computational methods applying etymology and language history to textual analysis.",
        dateRange = DateRange (date 2013 09) (Present),
        homepage = "http://github.com/JonathanReeve/macro-etym",
        github = Just "JonathanReeve/macro-etym",
        pypi = Just "macroetym",
        updates =
          [ Update (date 2016 01) $
              Publication
                Chapter
                "A Macro-Etymological Analysis of James Joyce's A Portrait of the Artist as a Young Man"
                "https://link.springer.com/chapter/10.1057/978-1-137-59569-0_9"
                (Venue "Reading Modernism with Machines" "" "Palgrave Macmillan"),
            Update (date 2015 01) $ News "[Featured](http://www.tapor.ca/?id=470) in the [Text Analysis Portal for Research](http://www.tapor.ca/)",
            Update (date 2015 01) $ News "Listed in Alain Liu's directory of Digital Humanities Tools",
            Update (date 2015 03) $ News "Featured in the [Digital Research Tools](http://www.dirtdirectory.org/) (DiRT) directory",
            Update (date 2015 05) $ News "Released as [a command-line program in the Python Package Archive](https://pypi.python.org/pypi/macroetym)",
            Update (date 2014 08) $
              Award
                "Winner, student bursary"
                (Venue "Association of Digital Humanities Organizations" "" "Lausanne, Switzerland"),
            Update (date 2014 07) $
              Award
                "Winner"
                ( Venue
                    "New York University Hirschhorn Thesis Award"
                    "https://draperprogram.wordpress.com/2014/06/12/congratulations-to-our-hirschhorn-award-nominees-and-winner/"
                    (uni "nyu")
                ),
            Update (date 2014 04) $
              Talk
                "Macro-Etymological Textual Analysis: a Computational Application of Language History to Literary Criticism"
                "http://jonreeve.com/presentations/dh2014/"
                (Venue "Digital Humanities 2014" "http://dh2014.org/" "Lausanne, Switzerland")
          ]
      },
    Project
      { title = "Text-Matcher",
        role = Creator,
        desc = "Fuzzy text matching and alignment algorithms.",
        dateRange = DateRange (date 2015 01) (Present),
        homepage = "http://github.com/JonathanReeve/text-matcher",
        github = Just "JonathanReeve/text-matcher",
        pypi = Nothing,
        updates =
          [ Update (date 2020 09) $ News "Used in [\"Measuring Unreading,\" a study by Andrew Piper and TxtLab](https://txtlab.org/2020/09/measuring-unreading/)",
            Update (date 2016 10) $ News "Released [command-line tool on the Python Package Archive](https://pypi.python.org/pypi/text-matcher)",
            Update (date 2015 05) $ News "Developed for [Modernism, Myth, and their Intertextualities: a Computational Detection of Biblical and Classical Allusion in the English-Language Novel, 1771-1930](https://github.com/JonathanReeve/allusion-detection/blob/master/paper/essay.pdf)"
          ]
      },
    Project
      { title = "Customeka",
        role = Creator,
        desc = "A highly customizable theme for the Omeka content management system.",
        dateRange = DateRange (date 2013 01) (date 2015 01),
        homepage = "http://github.com/JonathanReeve/theme-customeka",
        github = Just "JonathanReeve/theme-customeka",
        pypi = Nothing,
        updates =
          [ Update (date 2017 01) $ News "Used in the photo archive of the Greenwich Village Society for Historic Preservation",
            Update (date 2016 08) $
              Publication
                Tutorial
                "Installing Omeka"
                "https://programminghistorian.org/en/lessons/installing-omeka"
                (Venue "The Programming Historian" "https://programminghistorian.org" ""),
            Update (date 2012 10) $
              Talk
                "Elementaire, a Customizable Omeka Theme"
                ""
                (Venue "The Humanities and Technology Camp" "http://newyork2012.thatcamp.org/" "")
          ]
      }
  ]
