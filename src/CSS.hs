-- | This is the CSS for the site.

{-# LANGUAGE OverloadedStrings #-}

module CSS where

import Clay
import Prelude hiding (rem)


-- | Define your site CSS here
pageStyle :: Css
pageStyle = do
  html ? do
    fontFamily ["Raleway"] [sansSerif]
  body ? do
    margin (em 4) (pc 20) (em 1) (pc 20)
  ".header" ? do
    marginBottom $ em 2
  "li.pages" ? do
    listStyleType none
    marginTop $ em 1
  "b" ? fontSize (em 1.2)
  "p" ? sym margin (px 0)
  main_ ? do
    fontFamily ["Raleway"] [sansSerif]
  "a.icon img" ? width (em 3)
  -- Sometimes markdown processing adds an additional paragraph we have to subsume
  "span.update p" ? display inline
  ".chip" ? margin (rem 0.1) (rem 0.3) (rem 0.1) (rem 0.3)
  footer ? do
    backgroundColor "#5755d9"
    color "#fff"
    "a" ? color "#ddddec"
