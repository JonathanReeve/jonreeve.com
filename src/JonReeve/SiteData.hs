{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Site-wide config and data.
module JonReeve.SiteData where

import PyF

domain :: Text
domain = "https://jonreeve.com"

siteName :: Text
siteName = "Jonathan Reeve: Computational Literary Analysis"

name :: Text
name = "Jonathan Reeve"

greeting :: Text
greeting =
  [fmt|Hi. My name is Jonathan Reeve. I work in
                **computational literary analysis**. I write computer
                programs that help us understand novels and poetry.|]

coda :: Text
coda =
  [fmt|I believe in openness. This work is licensed under a [Creative
            Commons Attribution-NonCommercial-ShareAlike 4.0 International
            License](https://creativecommons.org/licenses/by-nc-sa/4.0/), unless
            otherwise stated. All content ©Jonathan Reeve 2023. Hand-coded with
            love, using exclusively free and open-source software, including
            [Ema](https://github.com/srid/ema) and
            [Haskell](https://haskell.org/). Hosted on
            [GitHub](https://github.com) and served with
            [Netlify](https://netlify.com). Icons by Nhor, via [The Noun
            Project](https://thenounproject.com). [Buy me a
            coffee](https://www.buymeacoffee.com/vaIVQZH) or support me [via
            Libera Pay](https://liberapay.com/JonathanReeve/donate).|]
