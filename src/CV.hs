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
educationLine ed = tr_ [ class_ "ed" ] $ do
  td_ [] (toHtml (degree ed))
  td_ [] (toHtml (field ed))
  td_ [] (toHtml (university ed))
  td_ [] (toHtml (formatDate (when ed)))

projectSection :: Html ()
projectSection = section_ [ class_ "projects" ] $
  foldMap projectCard projects

projectCard :: Project -> Html ()
projectCard proj = section_ [ class_ "projects" ] $ do
  span_ [] $ toHtml $ formatDateRange (dateRange proj)
  a_ [ class_ "title", href_ (homepage proj) ] $ toHtml $ title proj
  span_ [ class_ "role" ] $ formatRole $ role proj
  span_ [ class_ "github" ] $ toHtml $ formatGitHub $ github proj

formatGitHub :: T.Text -> URI
formatGitHub ghSlug = T.concat ["https://github.com/", ghSlug]

formatRole :: ProjectRole -> Html ()
formatRole role = case role of
  Creator -> "creator"
  CoCreator -> "co-creator"
  Developer -> "developer"
  ResearchAssistant -> "research assistant"
