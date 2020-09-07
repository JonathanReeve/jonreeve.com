{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Clay ((?), Css, em, pc, px, sym, rem)
import qualified Clay as C
import Control.Monad
import Data.Aeson (FromJSON, fromJSON)
import qualified Data.Aeson as Aeson
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text (Text)
import Development.Shake
import GHC.Generics (Generic)
import Lucid
import Lucid.Base
import Main.Utf8
import Path
import Rib (IsRoute, Pandoc)
import qualified Rib
import qualified Rib.Parser.Pandoc as Pandoc

import qualified CV

-- | Route corresponding to each generated static page.
--
-- The `a` parameter specifies the data (typically Markdown document) used to
-- generate the final page text.
data Route a where
  Route_Index :: Route [(Route Pandoc, Pandoc)]
  Route_CV :: Route [(Route Pandoc, Pandoc)]
  Route_Tags :: Route (Map Text [(Route Pandoc, Pandoc)])
  Route_Article :: FilePath -> Route Pandoc

-- | The `IsRoute` instance allows us to determine the target .html path for
-- each route. This affects what `routeUrl` will return.
instance IsRoute Route where
  routeFile = \case
    Route_Index ->
      pure "index.html"
    Route_Tags ->
      pure "tags/index.html"
    Route_CV ->
      pure "cv.html"
    Route_Article srcPath -> do
      let (year, month, _day, slug) = parseJekyllFilename srcPath
      pure $ year ++ "/" ++ month ++ "/" ++ slug ++ "/index.html"

parseJekyllFilename :: FilePath -> (String, String, String, String)
parseJekyllFilename fn =
  case T.splitOn "-" (T.pack $ fn) of
    y : m : d : rest ->
      (T.unpack y, T.unpack m, T.unpack d, T.unpack $ T.intercalate "-" rest)
    _ ->
      error "Malformed filename"

-- | Main entry point to our generator.
--
-- `Rib.run` handles CLI arguments, and takes three parameters here.
--
-- 1. Directory `content`, from which static files will be read.
-- 2. Directory `dest`, under which target files will be generated.
-- 3. Shake action to run.
--
-- In the shake action you would expect to use the utility functions
-- provided by Rib to do the actual generation of your static site.
main :: IO ()
main = withUtf8 $ Rib.run "content" "dest" generateSite

-- | Shake action for generating the static site
generateSite :: Action ()
generateSite = do
  -- Copy over the static files
  Rib.buildStaticFiles [ "assets/**"
                       , "images/**"
                       , "projects/**"
                       , "presentations/**"
                       ]
  let writeHtmlRoute :: Route a -> a -> Action ()
      writeHtmlRoute r = Rib.writeRoute r . Lucid.renderText . renderPage r
  -- Build individual sources, generating .html for each.
  articles <-
    Rib.forEvery ["posts/*.md"] $ \srcPath -> do
      let r = Route_Article srcPath
      doc <- Pandoc.parse Pandoc.readMarkdown srcPath
      writeHtmlRoute r doc
      pure (r, doc)
  writeHtmlRoute Route_CV $ articles
  writeHtmlRoute Route_Tags $ groupByTag articles
  writeHtmlRoute Route_Index $ reverse articles
  where
    groupByTag as =
      Map.fromListWith (<>) $ flip concatMap as $ \(r, doc) ->
        (,[(r, doc)]) <$> tags (getMeta doc)

-- | Define your site HTML here
renderPage :: Route a -> a -> Html ()
renderPage route val = html_ [lang_ "en"] $ do
  head_ $ do
    meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
    title_ routeTitle
    link_ [rel_ "stylesheet", href_ "/assets/css/spectre.min.css"]
    link_ [rel_ "stylesheet", href_ "https://fonts.googleapis.com/css?family=Montserrat|Raleway"]
    style_ [type_ "text/css"] $ C.render Main.pageStyle
  body_ $ do
    header_ [class_ "navbar"] $ do
      section_ [class_ "navbar-section"] $ do
        a_ [class_ "navbar-brand", href_ $ Rib.routeUrl Route_Index] $ do
          "Jonathan Reeve: "
          span_ [] "Computational Literary Analysis"
      section_ [class_ "navbar-section"] $ do
        ul_ [class_ "nav"] $ do
          li_ [class_ "nav-item"] $ a_ [href_ $ Rib.routeUrl Route_Tags] "Tags"
          li_ [class_ "nav-item"] $ a_ [href_ $ Rib.routeUrl Route_CV] "CV"

    content
    footer_ [ class_ "container" ] $ do
      script_ [ makeAttribute "data-goatcounter" "https://jonreeve.goatcounter.com/count"
              , async_ T.empty
              , src_ "//gc.zgo.at/count.js"
              ] T.empty
      div_ [ class_ "columns" ] $ do
        div_ [ class_ "column col-8" ] $ CV.md2Html coda
        div_ [ class_ "column col-4" ] $ do
          iconLink "github" "http://github.com/JonathanReeve"
          iconLink "twitter" "http://twitter.com/j0_0n"
          iconLink "envelope" "mailto:jonathan@jonreeve.com"

  where
    iconLink :: T.Text -> T.Text -> Html ()
    iconLink svg target = a_ [ class_ "icon", href_ target ] $
      img_ [ src_ (T.concat [ "/assets/images/", svg, "-square.svg" ]) ]

    routeTitle :: Html ()
    routeTitle = case route of
      Route_Index -> "Posts"
      Route_Tags -> "Tags"
      Route_CV -> "CV"
      Route_Article _ -> toHtml $ title $ getMeta val
    content :: Html ()
    content = case route of
      Route_Index -> do
        section_ [id_ "greeting"] $ do
          CV.md2Html greeting
          img_ [src_ "assets/images/noun_Book_1593490.svg"]
          span_ [] "+"
          img_ [src_ "assets/images/noun_retro computer_1905469.svg"]
          span_ [] "="
          img_ [src_ "assets/images/noun_education_1909997.svg"]
        main_ [class_ "container" ] $ forM_ val $ \(r, src) -> do
          li_ [class_ "pages"] $ do
            let meta = getMeta src
            h2_ [] $ a_ [href_ (Rib.routeUrl r)] $ toHtml $ title meta
            p_ [class_ "tags"] $ do
              "in "
              mapM_ (a_ [class_ "chip", href_ ""] . toHtml) (tags meta)
      Route_Tags -> do
        h1_ routeTitle
        div_ $ forM_ (sortOn (T.toLower . fst) $ Map.toList val) $ \(tag, rs) -> do
          a_ [id_ tag] mempty
          h2_ $ toHtml tag
          ul_ $ forM_ rs $ \(r, src) -> do
            li_ [class_ "pages"] $ do
              let meta = getMeta src
              b_ $ a_ [href_ (Rib.routeUrl r)] $ toHtml $ title meta
      Route_CV -> do
        h1_ "Curriculum Vitae"
        main_ [class_ "container" ] $ CV.cv
      Route_Article _ -> do
        h1_ routeTitle
        article_ $
          Pandoc.render val

-- | Define your site CSS here
pageStyle :: Css
pageStyle = do
  C.html ? do
    C.fontFamily ["Raleway"] [C.sansSerif]
  C.body ? do
    C.margin (em 4) (pc 20) (em 1) (pc 20)
  ".header" ? do
    C.marginBottom $ em 2
  "li.pages" ? do
    C.listStyleType C.none
    C.marginTop $ em 1
  "b" ? C.fontSize (em 1.2)
  "p" ? sym C.margin (px 0)
  C.main_ ? do
    C.fontFamily ["Raleway"] [C.sansSerif]
  "a.icon img" ? C.width (em 3)
  -- Sometimes markdown processing adds an additional paragraph we have to subsume
  "span.update p" ? C.display C.inline
  ".chip" ? C.margin (C.rem 0.1) (C.rem 0.3) (C.rem 0.1) (C.rem 0.3)
  C.footer ? do
    C.backgroundColor "#5755d9"
    C.color "#fff"
    "a" ? C.color "#ddddec"

-- | Metadata in our markdown sources
data SrcMeta
  = SrcMeta
      { title :: Text,
        tags :: [Text]
      }
  deriving (Show, Eq, Generic, FromJSON)

-- | Get metadata from Markdown's YAML block
getMeta :: Pandoc -> SrcMeta
getMeta src = case Pandoc.extractMeta src of
  Nothing -> error "No YAML metadata"
  Just (Left e) -> error $ T.unpack e
  Just (Right val) -> case fromJSON val of
    Aeson.Error e -> error $ "JSON error: " <> e
    Aeson.Success v -> v

greeting :: T.Text
greeting = T.concat [ "Hi. My name is Jonathan Reeve. I'm a PhD candidate in **computational literary analysis** at Columbia University. "
                    , "I write computer programs that help us understand novels and poetry."
                    ]

coda :: T.Text
coda = T.concat [ "I believe in openness. This work is licensed under a "
                , "[Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-nc-sa/4.0/), "
                , "unless otherwise stated. "
                , "All content ©Jonathan Reeve 2020. Hand-coded with love, using exclusively free and open-source software, including "
                , "[Rib](https://github.com/srid/rib), [Haskell](https://haskell.org/), [Doom Emacs](), and [Pop!_OS](https://pop.system76.com/). "
                , "Hosted on [GitHub](https://github.com) and served with [Netlify](https://netlify.com). "
                , "Icons by Nhor, via [The Noun Project](https://thenounproject.com). "
                , "[Buy me a coffee](https://www.buymeacoffee.com/vaIVQZH) or support me [via Libera Pay](https://liberapay.com/JonathanReeve/donate) or Bitcoin: 3Qvm1DwzFGk3L1Eb6yeg5Nbc6db8sZUnbK."
                ]
