{-# LANGUAGE DuplicateRecordFields #-}
-- needed for Lucid
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

-- needed for PyF

module JonReeve.CV where

import Clay hiding (Position, filter, header, html, title, type_)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import JonReeve.CV.Other as Other
import JonReeve.CV.Projects as Projects
import JonReeve.CV.Shared as Shared
import JonReeve.CV.Teaching
import JonReeve.Pandoc
import Lucid

md2Html :: Shared.Markdown -> Html ()
md2Html md =
  JonReeve.Pandoc.render $ JonReeve.Pandoc.parsePure readMarkdown md

-- -- TODO: Lucid's unsafe string
-- "TODO: markdown"

educationSection :: Html ()
educationSection = section_ [class_ "education"] $ do
  h1_ [] "Education"
  table_ [class_ "table"] $ do foldMap formatEducation education

formatEducation :: Education -> Html ()
formatEducation ed =
  tr_ [class_ "ed"] $
    mapM_ ((td_ []) . toHtml) $
      sequence [degree, field, university, formatDate . Other.when] ed

awardsSection :: Html ()
awardsSection = section_ [class_ "awards"] $ do
  h1_ [] "Awards and Fellowships"
  -- Attempt to sort
  -- ul_ [] $ mconcat $ sort $ Data.List.map formatUpdate CV.Other.miscAwards <> Data.List.map formatAwards projects
  ul_ [] $ foldMap formatUpdate Other.miscAwards <> foldMap formatAwards projects

-- | Get all publications from a project
formatAwards :: Project -> Html ()
formatAwards proj = foldMap formatUpdate publicationsOnly
  where
    publicationsOnly = filter getPub $ updates proj
    getPub update = case update of
      Update _ (Award _ _) -> True
      _ -> False

projectSection :: Html ()
projectSection = section_ [class_ "projects"] $ do
  h1_ [] "Projects"
  foldMap formatProject projects

formatProject :: Project -> Html ()
formatProject proj = section_ [class_ "project"] $ do
  h4_ [style_ "display: inline-block;"] $
    a_ [class_ "title", href_ (homepage proj)] $ toHtml $ title proj
  -- case (github proj) of
  --   Nothing -> toHtml T.empty
  --   Just gh -> p_ $ badges gh
  div_ [class_ "project-about"] $ do
    span_ [] $ toHtml $ formatDateRange (dateRange proj)
    span_ [class_ "role"] $ formatRole $ role proj
    span_ [class_ "desc"] $ md2Html $ desc proj
  ul_ [class_ "updates", style_ "margin-left: 1em"] $ mapM_ formatUpdate $ Projects.updates proj

badges :: URI -> Html ()
badges repo =
  foldMap
    (\path -> badge path repo)
    ["github/tag/", "github/stars/", "github/issues/", "github/languages/top/"]

badge :: T.Text -> URI -> Html ()
badge path repo = img_ [src_ badgeURL]
  where
    badgeURL = T.concat ["https://img.shields.io/", path, repo, opts]
    opts = ".svg?style=flat-square&colorB=494E8E"

-- data Update = Update Date Event deriving Show
formatUpdate :: Update -> Html ()
formatUpdate (Update updateDate event) = li_ [class_ "update"] $ do
  span_ [] $ toHtml $ formatDate updateDate
  span_ [] $ formatEvent event

formatEvent :: Event -> Html ()
formatEvent event = foldMap (span_ [class_ "update"]) $
  case event of
    News md ->
      [ toHtml (chip "news"),
        md2Html md
      ]
    Award awardAward awardVenue ->
      [ toHtml (chip "award"),
        toHtml awardAward,
        formatVenue awardVenue
      ]
    Talk talkTitle uri talkVenue ->
      [ toHtml (chip "talk"),
        a_ [href_ uri] (toHtml talkTitle),
        formatVenue talkVenue
      ]
    Publication pubType pubTitle pubURI pubVenue ->
      [ toHtml $ chip $ T.toLower $ (T.pack . show) pubType,
        a_ [href_ pubURI] (toHtml pubTitle),
        formatVenue pubVenue
      ]

formatRole :: ProjectRole -> Html ()
formatRole r = chip $ case r of
  Creator -> "creator"
  CoCreator -> "co-creator"
  Collaborator -> "collaborator"
  Developer -> "developer"
  ResearchAssistant -> "research assistant"

teachingSection :: Html ()
teachingSection = section_ [class_ "teaching"] $ do
  h1_ [] "Teaching"
  ul_ [] $ foldMap formatTeaching sortedTeaching

chip :: T.Text -> Html ()
chip chipText = span_ [class_ "chip"] $ toHtml chipText

formatTeaching :: Teaching -> Html ()
formatTeaching teachingItem = li_ [class_ "teaching"] $
  case teachingItem of
    Workshop workshopDates workshopName workshopVenue workshopURL workshopNotes -> do
      span_ [] $ toHtml $ formatDates workshopDates
      span_ [class_ "chip"] "workshop"
      span_ [] $ strong_ $ a_ [href_ workshopURL] $ toHtml workshopName
      span_ [] $ formatVenue workshopVenue
    Course dates name role venue url updates notes -> do
      span_ [] $ toHtml $ formatDateRanges dates
      span_ [class_ "chip"] "course"
      span_ [] $ strong_ $ a_ [href_ url] $ toHtml name
      span_ [] ", "
      formatTeachingRole role
      span_ [] $ formatVenue venue
      ul_ [class_ "updates", style_ "margin-left: 1em"] $ mapM_ formatUpdate $ updates

formatTeachingRole :: TeachingRole -> Html ()
formatTeachingRole r = span_ $ case r of
  Instructor -> "instructor"
  TA -> "teaching assistant"
  TAInstructor -> "instructor / teaching assistant"

formatVenue :: Venue -> Html ()
formatVenue (Venue venueName venueURL loc) = do
  ", "
  a_ [class_ "venue", href_ venueURL] $ do
    span_ [class_ "name"] $ toHtml venueName
    case loc of
      "" -> toHtml ""
      _ -> span_ [class_ "location"] $ toHtml $ T.concat [" (", loc, ")"]

affiliationsSection :: Html ()
affiliationsSection = section_ [class_ "affiliations"] $ do
  h1_ [] "Affiliations and Professional Activities"
  ul_ [] $ foldMap formatAffiliation affiliations

-- data Affiliation = Affiliation {affRole :: Text,
--                                 society  :: Venue,
--                                 dateRanges :: [DateRange]
--                                } deriving Show

formatAffiliation :: Affiliation -> Html ()
formatAffiliation (Affiliation role venue ranges) = li_ [class_ "affiliation"] $ do
  mapM_
    (span_ [])
    [ toHtml (formatDateRanges ranges),
      " ",
      toHtml role,
      formatVenue venue
    ]

-- data Position = Position { posDateRange :: DateRange,
--                            org :: Text,
--                            project :: Text,
--                            posRole :: Text,
--                            posUrl :: URI,
--                            updates :: [ Update ]
--                            } deriving Show
formatPosition :: Position -> Html ()
formatPosition pos = div_ [class_ "position", style_ "margin-bottom: 1em"] $ do
  mapM_
    (span_ [])
    [ toHtml $ formatDateRange (posDateRange pos),
      " ",
      strong_ [] $ toHtml $ org pos
    ]

  ul_ [] $ do
    li_ [] $ do
      toHtml $ T.concat [posRole pos, ", "]
      em_ [] $ toHtml $ project pos
    foldMap formatUpdate (posUpdates pos)

positionsSection :: Html ()
positionsSection = section_ [class_ "positions"] $ do
  h1_ [] "Positions and Freelance Clients"
  ul_ [] $ foldMap formatPosition positions

publicationsSection :: Html ()
publicationsSection = section_ [class_ "publications"] $ do
  h1_ [] "Publications"
  ul_ [] $ foldMap formatPublication projects -- <> foldMap formatTeachingPublication sortedTeaching

-- TODO
-- formatTeachingPublication :: Teaching -> Html ()
-- formatTeachingPublication teachingItems = foldMap formatUpdate teachingPublications where
--   teachingPublications = concatMap teachingUpdates $ hasPub teachingItems

-- | Get all publications from a project
formatPublication :: Project -> Html ()
formatPublication proj = foldMap formatUpdate publicationsOnly
  where
    publicationsOnly = sort $ filter getPub $ updates proj
    getPub update = case update of
      Update _ (Publication _ _ _ _) -> True
      _ -> False

talksSection :: Html ()
talksSection = section_ [class_ "talks"] $ do
  h1_ [] "Talks and Conference Presentations"
  ul_ $ talks
  where
    talks :: Html ()
    talks = foldMap formatTalks projects

formatTalks :: Project -> Html ()
formatTalks proj = foldMap formatUpdate talksOnly
  where
    talksOnly = filter getTalk $ updates proj
    getTalk update = case update of
      Update date (Talk title uri venue) -> True
      _ -> False

languagesSection :: Html ()
languagesSection = do
  h1_ [] "Languages and Skills"
  ul_ [] $ do
    mapM_
      langItem
      [ "**Programming languages**: Python, Haskell, PHP, R. Learning JavaScript, Julia, Elm, Nix.",
        "**Markup and style languages**: TEI XML, HTML, RDF, Markdown, XSL, CSS, LaTeX, BibLaTeX.",
        "**Shell languages**: BASH, ZSH, Fish, Nushell.",
        "**Operating systems**: Linux (NixOS, Ubuntu, Fedora, Arch), MacOS, Windows.",
        "**Programs**: Emacs, Vim, LibreOffice, MS Office.",
        "**Natural languages**: English, French, Mandarin Chinese, Esperanto. Learning Japanese, Italian, German, Spanish, Portuguese, Latin, and Irish.",
        "**Skills**: Natural language processing, data science, data analytics, data visualization, web development, dev ops (NixOS, server scripting, Docker), cluster computing."
      ]
  where
    langItem :: T.Text -> Html ()
    langItem item = li_ [] $ md2Html item

header :: Html ()
header = head_ [] $ do
  link_ [rel_ "stylesheet", href_ "assets/css/spectre.min.css"]
  link_ [href_ "https://fonts.googleapis.com/css?family=Montserrat|Raleway", rel_ "stylesheet"]
  style_ [type_ "text/css"] $ Clay.render pageStyle
  style_ [type_ "text/css"] $ toHtml printStyle

-- | Print CSS style, for printing this out.
printStyle :: T.Text
printStyle =
  T.concat
    [ "@media print {",
      TL.toStrict $ Clay.render printCss,
      "}; @page { margin: 3cm; }"
    ]

printCss :: Css
printCss = do
  -- Don't display any buttons
  "button" ? display none
  -- Don't display header nav
  "#headerWrapper" ? display none
  "footer" ? display none
  ".container" ? do
    maxWidth none
    sym padding none

-- | Define your site CSS here
pageStyle :: Css
pageStyle = do
  "span::after" ? do
    content (stringContent " ")
  "section" ? do
    marginTop (em 2)
    marginBottom (em 2)
  ".update p" ? display inline
  ".desc p" ? display inline

cv :: Html ()
cv = do
  educationSection
  projectSection
  publicationsSection
  talksSection
  awardsSection
  teachingSection
  positionsSection
  languagesSection
  affiliationsSection

html :: Html ()
html =
  html_ [] $ do
    header
    body_ [class_ "container grid-lg"] $ do
      h1_ [style_ "text-align: center"] "Curriculum Vitae"
      h2_ [style_ "text-align: center"] "Jonathan Reeve"
      cv

main :: IO ()
main = renderToFile "cv.html" html
