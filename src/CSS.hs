-- | This is the CSS for the site.

{-# LANGUAGE OverloadedStrings #-}

module CSS where

import Clay
import Clay.Stylesheet (key)
import qualified Clay.Media as Media
import Prelude hiding (rem, span)

myBlue = "#494E8E"
myLightgray = "#fafafa"
-- lightblue = "#705E9E"
-- darkblue = "#002"
-- mediumGray = "#333"


-- Define sizes of screens
xl = 1200
lg = 992
md = 768
sm = 576

fontSizeFor (w, s) = query Media.screen [Media.minWidth (px w)] $ fontSize (px s)

fontSizes :: Css
fontSizes = mapM_ fontSizeFor [(xl, 23), (lg, 24), (md, 25), (sm, 27)]

forMedia w = query Media.screen [Media.minWidth (px w)]

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
  ".container" ? do
    maxWidth (em 60)
    paddingAll (em 3)
  "#headerWrapper" ? do
    backgroundColor white
    borderTop solid (px 2) myBlue
    borderBottom solid (px 2) myBlue
    header ? do
      paddingTop nil
      paddingBottom nil
      minHeight (em 4)
      margin auto auto auto auto
      maxWidth (em 65)
      ".nav" ? do
        flexDirection row
  ".header" ? do
    marginBottom $ em 2
  "li.pages" ? do
    listStyleType none
    marginTop $ em 1
  h1 <> h2 <> h3 <> h4 ? marginTop (em 1)
  b ? fontSize (em 1.2)
  p ? sym margin nil
  main_ ? do
    fontFamily ["Raleway"] [sansSerif]
  "a.icon img" ? width (em 3)
  -- Sometimes markdown processing adds an additional paragraph we have to subsume
  "span.update p" ? display inline
  ".chip" ? margin (rem 0.1) (rem 0.3) (rem 0.1) (rem 0.3)
  footer ? do
    backgroundColor myBlue
    color myLightgray
    "a" ? color "#ddddec"
    ".icons" ? do
      color myLightgray
      display flex
      fontSize (em 2)
      flexDirection row
      alignSelf center
      -- textAlign right
      justifyContent spaceBetween
    ".icons svg" # hover ? do
      fill lightgray
  "section#greeting" ? do
    marginTop (em 4)
    marginBottom (em 2)
    fontSize (em 2)
    ".icons" ? do
      display flex
      justifyContent spaceBetween
      span ? do
        fontSize (em 4)
        fontWeight (weight 600)
        paddingLeft (pct 5)
        paddingRight (pct 5)
  "section#postList" ? do
    "li.post" ? do
      marginTop (em 5)
      listStyleType none
    ".postTitle" ? fontSize (em 2)

fill :: Color -> Css
fill = key "fill"

paddingAll :: Size a -> Css
paddingAll val = padding val val val val
