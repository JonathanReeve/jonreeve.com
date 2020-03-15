{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-} -- needed for PyF
{-# LANGUAGE ExtendedDefaultRules #-} -- needed for Lucid
{-# LANGUAGE DuplicateRecordFields #-}

module CV where

import CV.Projects
import CV.Teaching
import CV.Shared
import CV.Other

import PyF
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as TIO
import Lucid
import Clay hiding (title, Position, type_, header, html, filter)

import Rib

-- import Text.Pandoc

-- md2Html :: CV.Shared.Markdown -> Html ()
-- md2Html md = do
--   result <- runPure $ do
--     doc <- readMarkdown def md
--     writeHtml5 def doc
--   return $ case result of
--     Left err -> error "Couldn't convert this markdown."
--     Right res -> res

educationSection :: Html ()
educationSection = section_ [ class_ "education" ] $ do
  h1_ [] "Education"
  table_ [ class_ "table" ] $ do foldMap formatEducation education

formatEducation :: Education -> Html ()
formatEducation ed = tr_ [ class_ "ed" ] $ mapM_ ((td_ []) . toHtml) $
  sequence [ degree, field, university, formatDate . when ] ed

projectSection :: Html ()
projectSection = section_ [ class_ "projects" ] $ do
  h1_ [] "Projects"
  foldMap formatProject projects

formatProject :: Project -> Html ()
formatProject proj = section_ [ class_ "projects" ] $ do
  h4_ [ style_ "display: inline-block;" ] $
    a_ [ class_ "title", href_ (homepage proj) ] $ toHtml $ title proj
  -- case (github proj) of
  --   Nothing -> toHtml T.empty
  --   Just gh -> do
  --     div_ [ class_ "projectLinks" ] $ do
  --       span_ [] $ a_ [ class_ "github", href_ (T.concat ["https://github.com/", gh]) ] "Source code repository"
  --       span_ [] $ a_ [ class_ "github", href_ (T.concat ["https://github.com/", gh, "/graphs/contributors"]) ] "Contributors"
  --       -- badges gh
  div_ [ class_ "project-about" ] $ do
    span_ [] $ toHtml $ formatDateRange (dateRange proj)
    span_ [ class_ "role" ] $ formatRole $ role proj
    span_ [ class_ "desc" ] $ toHtml $ desc proj
  ul_ [ class_ "updates", style_ "margin-left: 1em" ] $ mapM_ formatUpdate $ CV.Projects.updates proj

badges :: URI -> Html ()
badges repo = foldMap (\path -> badge path repo)
  ["github/tag/", "github/stars/", "github/issues/", "github/languages/top/"]

badge :: T.Text -> URI -> Html ()
badge path repo = img_ [src_ url] where
  url = T.concat ["https://img.shields.io/", path, repo, opts]
  opts = ".svg?style=flat-square&colorB=494E8E"

-- data Update = Update Date Event deriving Show
formatUpdate :: Update -> Html ()
formatUpdate (Update date event)  = li_ [ class_ "update" ] $ do
  span_ [] $ toHtml $ formatDate date
  span_ [] $ formatEvent event

formatEvent :: Event -> Html ()
formatEvent event = foldMap (span_ [ class_ "update" ]) $
  case event of
    News md -> [ toHtml (chip "news")
              , toHtml md
              ]
    Award award venue -> [ toHtml (chip "award")
                        , toHtml award
                        , formatVenue venue
                        ]
    Talk title uri venue -> [ toHtml (chip "talk")
                           , a_ [ href_ uri ] (toHtml title)
                           , formatVenue venue
                           ]
    Publication pubType title uri venue -> [ toHtml $ chip $ (T.pack . show) pubType
                                          , a_ [ href_ uri ] (toHtml title)
                                          , formatVenue venue
                                          ]

formatRole :: ProjectRole -> Html ()
formatRole role = chip $ case role of
  Creator -> "creator"
  CoCreator -> "co-creator"
  Developer -> "developer"
  ResearchAssistant -> "research assistant"

teachingSection :: Html ()
teachingSection = section_ [ class_ "teaching" ] $ do
  h1_ [] "Teaching"
  ul_ [] $ foldMap formatTeaching teaching

chip :: T.Text -> Html ()
chip text = span_ [ class_ "chip" ] $ toHtml text

formatTeaching :: Teaching -> Html ()
formatTeaching teachingItem = li_ [ class_ "teaching" ] $ 
  case teachingItem of
    Workshop dates name venue url notes -> do
      span_ [] $ toHtml $ formatDates dates
      span_ [ class_ "chip" ] "workshop"
      span_ [] $ strong_ $ a_ [ href_ url ] $ toHtml (T.concat [name, ","])
      span_ [] $ formatVenue venue
    Course dates name role venue url notes -> do
      span_ [] $ toHtml $ formatDateRanges dates
      span_ [ class_ "chip" ] "course"
      span_ [] $ strong_ $ a_ [ href_ url ] $ toHtml (T.concat [name, ","])
      span_ [] $ formatVenue venue

formatVenue :: Venue -> Html ()
formatVenue (Venue name url loc) = do
  a_ [ class_ "venue", href_ url ] $ do
    span_ [ class_ "name" ] $ toHtml name
    span_ [ class_ "location" ] $ toHtml $ T.concat ["(", loc, ")"]

affiliationsSection :: Html ()
affiliationsSection = section_ [ class_ "affiliations" ] $ do
  h1_ [] "Affiliations and Professional Activities"
  ul_ [] $ foldMap formatAffiliation affiliations


-- data Affiliation = Affiliation {affRole :: Text,
--                                 society  :: Venue,
--                                 dateRanges :: [DateRange]
--                                } deriving Show

formatAffiliation :: Affiliation -> Html ()
formatAffiliation (Affiliation role venue ranges) = li_ [ class_ "affiliation" ] $ do
  mapM_ (span_ [])
    [ toHtml (T.concat [role, ","])
    , formatVenue venue
    , toHtml (formatDateRanges ranges)
    ]

-- data Position = Position { posDateRange :: DateRange,
--                            org :: Text,
--                            project :: Text,
--                            posRole :: Text,
--                            posUrl :: URI,
--                            updates :: [ Update ]
--                            } deriving Show
formatPosition :: Position -> Html ()
formatPosition pos = div_ [ class_ "position", style_ "margin-bottom: 1em"] $ do
                       mapM_ (span_ []) [ toHtml $ formatDateRange (posDateRange pos)
                                        , strong_ [] $ toHtml $ org pos
                                        ]
                       ul_ [] $ do
                         li_ [] $ do
                           toHtml $ T.concat [posRole pos, ", "]
                           em_ [] $ toHtml $ project pos
                         foldMap formatUpdate (posUpdates pos)

positionsSection :: Html ()
positionsSection = section_ [ class_ "positions" ] $ do
  h1_ [] "Positions and Freelance Clients"
  ul_ [] $ foldMap formatPosition positions

publicationsSection :: Html ()
publicationsSection = section_ [ class_ "publications" ] $ do
  h1_ [] "Publications"
  ul_ [] $ foldMap formatPublication projects

-- | Get all publications from a project
formatPublication :: Project -> Html ()
formatPublication proj = foldMap formatUpdate publicationsOnly where
    publicationsOnly = (filter (getPub) $ updates proj)
    getPub update = case update of
      Update date (Publication kind title url venue) -> True
      otherwise -> False

talksSection :: Html ()
talksSection = section_ [ class_ "talks" ] $ do
  h1_ [] "Talks and Conference Presentations"
  ul_ $ foldMap formatTalks projects

formatTalks :: Project -> Html ()
formatTalks proj = foldMap formatUpdate talksOnly where
  talksOnly = (filter (getTalk) $ updates proj)
  getTalk update = case update of
    Update date (Talk title uri venue) -> True
    otherwise -> False

header :: Html ()
header = head_ [] $ do
  link_ [rel_ "stylesheet", href_ "assets/css/spectre.min.css"]
  link_ [href_ "https://fonts.googleapis.com/css?family=Montserrat|Raleway", rel_ "stylesheet"]
  style_ [type_ "text/css"] $ render pageStyle
  style_ [type_ "text/css"] $ toHtml printStyle

-- | Print CSS style, for printing this out.
printStyle :: T.Text
printStyle = T.concat ["@media print {"
                      , TL.toStrict $ render printCss
                      , "}; @page { margin: 3cm; }"
                      ]

printCss :: Css
printCss = do
  "button" ? display none

-- | Define your site CSS here
pageStyle :: Css
pageStyle = do
  "span::after" ? do
    content (stringContent " ")
  "section" ? do
    marginTop (em 2)
    marginBottom (em 2)

cv = do
  educationSection
  projectSection
  publicationsSection
  talksSection
  teachingSection
  positionsSection
  affiliationsSection

html =
  html_ [] $ do
    header
    body_ [ class_ "container grid-lg" ] $ do
      h1_ [style_ "text-align: center"] "Curriculum Vitae"
      h2_ [style_ "text-align: center"] "Jonathan Reeve"
      cv

main :: IO ()
main = renderToFile "cv.html" html
