{-# LANGUAGE OverloadedStrings #-}

-- | This is the CSS for the site.
module JonReeve.CSS where

import Clay
-- (key, MediaType)
import Clay.Media qualified as Media
import Clay.Stylesheet
import Prelude hiding (rem)

myBlue :: Color
myBlue = "#494E8E"

myLightgray :: Color
myLightgray = "#fafafa"

-- lightblue = "#705E9E"
-- darkblue = "#002"
-- mediumGray = "#333"

-- Define sizes of screens
xl :: Double
xl = 1200

lg :: Double
lg = 992

md :: Double
md = 768

sm :: Double
sm = 576

fontSizeFor :: (Double, Double) -> Css
fontSizeFor (thisWidth, thisSize) =
  query Media.screen [Media.minWidth (px thisWidth)] $
    fontSize (px thisSize)

fontSizes :: Css
fontSizes = mapM_ fontSizeFor [(xl, 23), (lg, 24), (md, 25), (sm, 27)]

forMedia :: Double -> Css -> Css
forMedia w = query Media.screen [Media.minWidth (px w)]

pageMedia :: MediaType
pageMedia = MediaType "page"

-- | Define your site CSS here
pageStyle :: Css
pageStyle = do
  html ? do
    fontSizes
  body ? do
    fontFamily ["Raleway"] [sansSerif]
  "body.has-docked-nav #main-header" ? do
    width (pct 100)
    opacity 0.95
    zIndex 2
    position fixed
  -- top 0
  -- left 0
  "#headerWrapper" ? do
    backgroundColor white
    borderTop (px 2) solid myBlue
    borderBottom (px 2) solid myBlue
    header ? do
      paddingTop nil
      paddingBottom nil
      paddingLeft (em 1)
      paddingRight (em 2)
      minHeight (em 4)
      margin auto auto auto auto
      maxWidth (em 65)
      ".nav" ? do
        flexDirection row
        li ? marginTop nil
  ".container" ? do
    maxWidth (em 55)
    sym padding (em 3)
  ".header" ? do
    marginBottom $ em 2
  "li.pages" ? do
    listStyleType none
    marginTop $ em 1
  h1 <> h2 <> h3 <> h4 ? marginTop (em 1)
  b ? fontSize (em 1.2)
  main_ ? do
    fontFamily ["Raleway"] [sansSerif]
    li ? listStylePosition outside
  ".csl-entry" ? do
    marginLeft (em 2)
    textIndent (indent (em (-2)))
  figure ? do
    textAlign center
    figcaption ? textAlign center
    img ? maxWidth (pct 80)
  -- Repeating this temporarily, due to <figure> not working for org mode
  article ? img ? maxWidth (pct 80)
  -- Sometimes markdown processing adds an additional paragraph we have to subsume
  "span.update p" ? display inline
  ".chip" ? do
    margin (rem 0.1) (rem 0.3) (rem 0.1) (rem 0.3)
    backgroundColor "#5755dd42"
  footer ? do
    backgroundColor myBlue
    color myLightgray
    "a" ? color "#ddddec"
    "a.icon img" ? width (em 3)
    ".column" ? do
      display flex
      justifyContent center
      alignContent center
    p ? marginBottom nil
    ".icons" ? do
      color myLightgray
      fontSize (em 2)
      display flex
      alignSelf center
      forMedia sm $ flexDirection column
      forMedia lg $ flexDirection row
      "svg" # hover ? do
        opacity 0.8

  "section#greeting" ? do
    marginTop (em 4)
    marginBottom (em 2)
    fontSize (em 2)
    ".icons" ? do
      display flex
      justifyContent spaceBetween
      img ? do
        height (em 4)
        marginTop (em 2)
  "section#postList" ? do
    "li.post" ? do
      marginTop (em 5)
      listStyleType none
    ".postTitle" ? fontSize (em 2)
  printCss

fill :: Color -> Css
fill = key "fill"

printCss :: Css
printCss = query Media.print [] $ do
  html ? fontSize (rem 0.9)
  -- Don't display any buttons
  "button" ? display none
  -- Don't display header nav
  "#headerWrapper" ? display none
  "footer" ? display none
  ".container" ? do
    maxWidth none
    sym padding (em 0)
  "table" ? fontSize (rem 0.8)

-- pageCss :: Css
-- pageCss = query pageMedia [] $ do
--   "margin" ? (cm 3)
