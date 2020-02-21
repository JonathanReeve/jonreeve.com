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
import Lucid

educationSection :: Html ()
educationSection = section_ [ class_ "education" ] $
  table_ [] $ do foldMap educationLine education

educationLine :: Education -> Html ()
educationLine ed = tr_ [ class_ "ed" ] $ mapM_ ((td_ []) . toHtml) $
  sequence [ degree, field, university, formatDate . when ] ed

projectSection :: Html ()
projectSection = section_ [ class_ "projects" ] $
  foldMap projectCard projects

projectCard :: Project -> Html ()
projectCard proj = section_ [ class_ "projects" ] $ do
  span_ [] $ toHtml $ formatDateRange (dateRange proj)
  a_ [ class_ "title", href_ (homepage proj) ] $ toHtml $ title proj
  span_ [ class_ "role" ] $ formatRole $ role proj
  a_ [ class_ "github", href_ (formatGitHub $ github proj) ] "Source code repository"
  ul_ [ class_ "updates" ] $ mapM_ formatUpdate $ CV.Projects.updates proj

-- data Update = Update Date Event deriving Show
formatUpdate :: Update -> Html ()
formatUpdate (Update date event)  = li_ [ class_ "update" ] $ do
  toHtml $ formatDate date
  formatEvent event

formatEvent :: Event -> Html ()
formatEvent event = case event of
  News md -> toHtml $ md
  Award award venue -> toHtml award >> formatVenue venue
  Talk title uri venue -> a_ [ href_ uri ] (toHtml title) >> formatVenue venue
  Publication pubType title uri venue -> do
    toHtml $ chip $ (T.pack . show) pubType
    a_ [ href_ uri ] (toHtml title) >> formatVenue venue

formatGitHub :: Maybe T.Text -> URI
formatGitHub maybeGHSlug = case maybeGHSlug of
  Nothing -> ""
  Just slug -> T.concat ["https://github.com/", slug]

formatRole :: ProjectRole -> Html ()
formatRole role = chip $ case role of
  Creator -> "creator"
  CoCreator -> "co-creator"
  Developer -> "developer"
  ResearchAssistant -> "research assistant"

teachingSection :: Html ()
teachingSection = section_ [ class_ "teaching" ] $ ul_ [] $
  foldMap formatTeaching teaching

chip :: T.Text -> Html ()
chip text = span_ [ class_ "chip" ] $ toHtml text

formatTeaching :: Teaching -> Html ()
formatTeaching teachingItem = case teachingItem of
  Workshop dates name venue url notes -> do
    li_ [] $ span_ [ class_ "chip" ] "workshop"
    span_ [] $ toHtml $ formatDates dates
    span_ [] $ formatVenue venue
    a_ [ href_ url ] $ toHtml name
  Course dates name role venue url notes -> do
    li_ [] $ span_ [ class_ "chip" ] "course"
    span_ [] $ toHtml $ formatDateRanges dates
    span_ [] $ formatVenue venue
    a_ [ href_ url ] $ toHtml name

formatVenue :: Venue -> Html ()
formatVenue (Venue name url loc) = do
  a_ [ class_ "venue", href_ url ] $ do
    span_ [ class_ "name" ] $ toHtml name
    span_ [ class_ "location" ] $ toHtml $ T.concat ["(", loc, ")"]

cv = do
  educationSection
  projectSection
  teachingSection

main :: IO ()
main = renderToFile "cv.html" cv
