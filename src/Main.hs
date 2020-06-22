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

import Clay ((?), Css, em, pc, px, sym)
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
import Main.Utf8
import Path
import Rib (IsRoute, Pandoc)
import qualified Rib
import qualified Rib.Parser.Pandoc as Pandoc

-- | Route corresponding to each generated static page.
--
-- The `a` parameter specifies the data (typically Markdown document) used to
-- generate the final page text.
data Route a where
  Route_Index :: Route [(Route Pandoc, Pandoc)]
  Route_Tags :: Route (Map Text [(Route Pandoc, Pandoc)])
  Route_Article :: Path Rel File -> Route Pandoc

-- | The `IsRoute` instance allows us to determine the target .html path for
-- each route. This affects what `routeUrl` will return.
instance IsRoute Route where
  routeFile = \case
    Route_Index ->
      pure [relfile|index.html|]
    Route_Tags ->
      pure [relfile|tags/index.html|]
    Route_Article srcPath -> do
      fn <- fmap fst $ splitExtension $ filename srcPath
      let (year, month, _day, slug) = parseJekyllFilename fn
      parseRelFile $ year <> "/" <> month <> "/" <> slug <> "/index.html"
    where
      parseJekyllFilename :: Path Rel File -> (String, String, String, String)
      parseJekyllFilename fn =
        case T.splitOn "-" (T.pack $ toFilePath fn) of
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
main = withUtf8 $ Rib.run [reldir|content|] [reldir|dest|] generateSite

-- | Shake action for generating the static site
generateSite :: Action ()
generateSite = do
  -- Copy over the static files
  Rib.buildStaticFiles
    [ [relfile|assets/**|],
      [relfile|projects/**|],
      [relfile|presentations/**|]
    ]
  let writeHtmlRoute :: Route a -> a -> Action ()
      writeHtmlRoute r = Rib.writeRoute r . Lucid.renderText . renderPage r
  -- Build individual sources, generating .html for each.
  articles <-
    Rib.forEvery [[relfile|posts/*.md|]] $ \srcPath -> do
      let r = Route_Article srcPath
      doc <- Pandoc.parse Pandoc.readMarkdown srcPath
      writeHtmlRoute r doc
      pure (r, doc)
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
    style_ [type_ "text/css"] $ C.render pageStyle
    link_ [rel_ "stylesheet", href_ "assets/css/spectre.min.css"]
    link_ [rel_ "stylesheet", href_ "https://fonts.googleapis.com/css?family=Montserrat|Raleway"]
  body_ $ do
    div_ [class_ "header"] $ do
      a_ [href_ $ Rib.routeUrl Route_Index] "Back to Home"
      " | "
      a_ [href_ $ Rib.routeUrl Route_Tags] "Tags"
    h1_ routeTitle
    content
    footer_ [ class_ "container" ] $ do
      div_ [ class_ "columns" ] $ do
        div_ [ class_ "column col-8" ] $ do
          "Here is the coda"
        div_ [ class_ "column col-4" ] $ do
          a_ [ href_ "http://github.com/JonathanReeve", class_ "fab fa-github" ] ""
          a_ [ href_ "http://twitter.com/j0_0n", class_ "fab fa-twitter" ] ""
          a_ [ href_ "mailto:jonathan@jonreeve.com", class_ "fas fa-envelope" ] ""

  where
    routeTitle :: Html ()
    routeTitle = case route of
      Route_Index -> "Posts"
      Route_Tags -> "Tags"
      Route_Article _ -> toHtml $ title $ getMeta val
    content :: Html ()
    content = case route of
      Route_Index ->
        main_ [class_ "container" ] $ forM_ val $ \(r, src) ->
          li_ [class_ "pages"] $ do
            let meta = getMeta src
            b_ $ a_ [href_ (Rib.routeUrl r)] $ toHtml $ title meta
      Route_Tags ->
        div_ $ forM_ (sortOn (T.toLower . fst) $ Map.toList val) $ \(tag, rs) -> do
          h2_ $ toHtml tag
          ul_ $ forM_ rs $ \(r, src) -> do
            li_ [class_ "pages"] $ do
              let meta = getMeta src
              b_ $ a_ [href_ (Rib.routeUrl r)] $ toHtml $ title meta
      Route_Article _ ->
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
