{-# LANGUAGE OverloadedStrings #-}

module CV.Other where

-- import Network.URI
import Data.Text
-- import Formatting

import CV.Shared

data Education = Education {
  university :: Text,
  degree :: Text,
  field :: Text,
  when :: Date
  } deriving Show

education = [
  Education (uni "nyu") "Bachelor of Arts" "Interdisciplinary" (date 2004 1),
  Education (uni "bc") "Master of Arts" "English Literature" (date 2010 05),
  Education (uni "nyu") "Master of Arts" "Humanities" (date 2014 01),
  Education (uni "cu") "Master of Arts" "English and Comparative Literature" (date 2016 05),
  Education (uni "cu") "Master of Philosophy" "English and Comparative Literature" (date 2019 05),
  Education (uni "cu") "PhD Candidate" "English and Comparative Literature" (date 2019 05)
  ]


data Position = Position { posDateRange :: DateRange,
                           org :: Text,
                           project :: Text,
                           posRole :: Text,
                           posUrl :: URI,
                           updates :: [ Update ]
                           } deriving Show

positions = [
      Position { posDateRange = DateRange (date 2015 11) (date 2017 07),
                 project = "DH Box",
                 org = uni "cuny",
                 posRole = "Developer",
                 updates = [
                   Update (date 2016 07) $ News "Developed [corpus, a textual corpus downloader](https://github.com/DH-Box/corpus-downloader)",
                   Update (date 2017 08) $ News "Integrated textual corpus download functionality into the [DH Box web app](http://dhbox.org)",
                   Update (date 2017 09) $ News "Created [DH-USB, a digital humanities operating system designed to run from a USB drive](https://github.com/DH-Box/dh-usb)"
                   ]
          },

      Position { org ="Institute for Comparative Literature and Society, Columbia University",
                 project = "A Safer Online Public Square",
                 posRole = "Research Assistant, Computational Methods of Abusive Language Detection",
                 posUrl = "http://icls.columbia.edu/initiatives/a-safer-online-public-square/",
                 posDateRange = DateRange (date 2017 06)  (date 2018 07)
               },

      Position { posDateRange = DateRange (date 2015 05) (date 2015 10),
                 posRole = "Web developer",
                 org = "Greenwich Village Society for Historic Preservation",
                 project = "GVSHP Image Archive",
                 updates = [ Update (date 2015 10) $ News
                             "Developed the [Greenwich Village Society for Historic Preservation Image Archive](http://archive.gvshp.org), an Omeka-based image archive"
                           ]
               },

      Position { project = "MLA Commons",
                 org = "Modern Language Association",
                 posRole = "Web Developer",
                 posDateRange = DateRange (date 2013 11) (date 2015 08),
                 updates = [ Update (date 2013 11) $ News "Edited XML and XSL code for the [Literary Research Guide](http://mlalrg.org/public)",
                             Update (date 2014 01) $ News "Contributed to software used for the electronic edition of [_Literary Studies in the Digital Age_](http://dlsanthology.commons.mla.org/), and [_Digital Pedagogy in the Humanities_](https://digitalpedagogy.commons.mla.org/)",
                             Update (date 2014 08) $ News "Primary contributor to [the 2014 and 2015 redesigns](http://updates.commons.mla.org/2014/07/28/new-theme-for-the-mla-commons/) of the [MLA Commons](http://commons.mla.org)",
                             Update (date 2015 01) $ News
                               "Rewrote [cbox-auth](https://github.com/mlaa/cbox-auth), the authentication system of the _MLA Commons_, to interface with the MLA member database",
                             Update (date 2015 03) $ News
                               "Designed [a WordPress plugin for visualizing patterns of disciplinarity](https://github.com/mlaa/bp-group-statistics) between _MLA Commons_ interest groups"
                           ]
               },

      Position { posDateRange = DateRange (date 2012 06) (date 2015 01),
                 project = "DOCMAP",
                 org = "New York University History Department",
                 posRole = "Web developer",
                 updates = [ Update (date 2012 09) $ News
                               "Developed [a customization engine](https://github.com/JonathanReeve/theme-customeka) for the [Omeka content management platform](http://omeka.org)",
                             Update (date 2015 01) $ News
                                "Designed and developed Omeka themes deployed on [Greenwich Village History](http://gvh.aphdigital.org) and [New Jersey Digital History](http://njdigitalhistory.org/NJDHA/)"
                           ]
               },

      Position { project = "The Manifesto in Literature",
                 org = "Thomas Riggs & Company, Publishers",
                 posRole = "Research Writer",
                 posDateRange = DateRange (date 2012 07) (date 2012 09),
                 updates = [
                   Update (date 2012 09) $ News
                     "Authored six articles for the 2013 reference volume [*The Manifesto in Literature*](http://www.thomasriggs.net/pages/content/index.asp?PageID=158): Richard Stallman's \"The GNU Manifesto,\" John Barlow's \"A Declaration of the Independence of Cyberspace,\" and several others"
                   ]
               }
      ]


data Affiliation = Affiliation {affRole :: Text,
                                society  :: Venue,
                                dateRanges :: [DateRange]
                               } deriving Show

affiliations = [Affiliation "Active member"
                (Venue "Literary Modeling and Visualization Lab"
                  "http://xpmethod.plaintext.in/projects/literary-modeling.html"
                  "Group for Experimental Methods in the Humanities, Columbia U")
                [DateRange (date 2015 05) (Present)],

                Affiliation "Founding member"
                (Venue "Digital Literary Stylistics Special Interest Group"
                  "https://dls.hypotheses.org/" "Association of Digital Humanities Organizations")
                [DateRange (date 2017 05) (Present)],

                Affiliation "Reviewer"
                (Venue "Digital Humanities 2018" "https://dh2018.adho.org/en/" "Mexico City")
                [DateRange (date 2017 12) (date 2017 12)],

                Affiliation "Reviewer"
                (Venue "Digital Humanities 2020" "https://dh2019.adho.org/en/" "Ottawa, Canada")
                [DateRange (date 2019 12) (date 2019 12)],

                 Affiliation "Reviewer"
                 (Venue "Journal of Data Mining and Digital Humanities"
                 "http://jdmdh.episciences.org/"
                 "") -- TODO: Add location
                 [DateRange (date 2018 01) (date 2018 01)],

                 Affiliation "Reviewer"
                 (Venue "Digital Scholarship in the Humanities" "http://dsh.oxfordjournals.org/" "Oxford UP")
                 [DateRange (date 2018 08) (date 2018 08)],

                 Affiliation "Editor-at-Large"
                 (Venue "Digital Humanities Now" "http://digitalhumanitiesnow.org/" "")
                 [DateRange (date 2014 01) (Present)],

                 Affiliation "Member"
                 (Venue "Modern Language Association" "https://mla.org" "")
                 [DateRange (date 2010 01) (Present)],

                 Affiliation "Member"
                 (Venue "Association for Computers in the Humanities" "https://ach.org" "")
                 [DateRange (date 2014 01) (Present)],

                 Affiliation "Member"
                 (Venue "Society for Textual Scolarship" "https://textualsociety.org/" "")
                 [DateRange (date 2015 11) (date 2016 11)],

                 Affiliation "Member"
                 (Venue "HASTAC: Humanities, Arts, and Sciences Advanced Collaboratory" "https://www.hastac.org/" "")
                 [DateRange (date 2015 05) (date 2016 05)]
    ]
