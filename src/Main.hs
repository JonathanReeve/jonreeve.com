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
import Rib (IsRoute, Pandoc)
import qualified Rib
import qualified Rib.Parser.Pandoc as Pandoc
import PyF

import qualified CV
import CSS

-- | Route corresponding to each generated static page.
--
-- The `a` parameter specifies the data (typically Markdown document) used to
-- generate the final page text.
data Route a where
  Route_Index :: Route [(Route Pandoc, Pandoc)]
  Route_CV :: Route [(Route Pandoc, Pandoc)]
  Route_Tags :: Route (Map Text [(Route Pandoc, Pandoc)])
  Route_Article :: FilePath -> Route Pandoc
  Route_Feed :: Route [(Route Pandoc, Pandoc)]

-- | The `IsRoute` instance allows us to determine the target .html path for
-- each route. This affects what `routeUrl` will return.
instance IsRoute Route where
  routeFile = \case
    Route_Index -> pure "index.html"
    Route_Tags -> pure "tags/index.html"
    Route_CV -> pure "cv.html"
    Route_Article srcPath -> do
      let (year, month, _day, slug) = parseJekyllFilename srcPath
      pure $ year ++ "/" ++ month ++ "/" ++ slug ++ "/index.html"
    Route_Feed -> pure "feed.xml"

parseJekyllFilename :: FilePath -> (String, String, String, String)
parseJekyllFilename fn =
  case T.splitOn "-" (T.pack fn) of
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
      let r = Route_Article $ cleanPath srcPath
      doc <- Pandoc.parse Pandoc.readMarkdown srcPath
      writeHtmlRoute r doc
      pure (r, doc)
  writeHtmlRoute Route_CV articles
  writeHtmlRoute Route_Tags $ groupByTag articles
  writeHtmlRoute Route_Index $ reverse articles
  writeHtmlRoute Route_Feed articles
  where
    cleanPath path = (drop 6) (take (length path - 3) path)
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
    style_ [type_ "text/css"] $ C.render CSS.pageStyle
  body_ $ do
    div_ [id_ "headerWrapper"] $ do
      header_ [class_ "navbar"] $ do
        section_ [class_ "navbar-section"] $ do
          a_ [class_ "navbar-brand", href_ $ Rib.routeUrl Route_Index] $ do
            "Jonathan Reeve: "
            span_ [] "Computational Literary Analysis"
        section_ [class_ "navbar-section"] $ do
          ul_ [class_ "nav"] $ do
            navItem Route_Index "posts"
            navItem Route_CV "cv"
            navItem Route_Tags "tags"
            navItem Route_Feed "feed"
    div_ [class_ "container"] $ do
      content
    footer_ [ ] $ do
      div_ [ class_ "container" ] $ do
        div_ [ class_ "columns" ] $ do
          div_ [ class_ "column col-8" ] $ CV.md2Html coda
          div_ [ class_ "column col-4" ] $ do
            div_ [ class_ "icons" ] $ do
              a_ [href_ "http://github.com/JonathanReeve"] gitHubIcon
              a_ [href_ "http://twitter.com/j0_0n"] twitterIcon
              a_ [href_ "mailto:jonathan@jonreeve.com"] emailIcon
        script_ [ makeAttribute "data-goatcounter" "https://jonreeve.goatcounter.com/count"
                , async_ T.empty
                , src_ "//gc.zgo.at/count.js"
                ] T.empty
        script_ [ src_ "/assets/js/jquery-3.5.1.min.js" ] T.empty
        script_ [ src_ "/assets/js/main.js" ] T.empty
        script_ [ src_ "https://hypothes.is/embed.js", async_ T.empty ] T.empty
  where
    navItem :: Route a -> Html () -> Html ()
    navItem navRoute label = li_ [class_ "nav-item"] $ a_ [href_ $ Rib.routeUrl navRoute ] label

    routeTitle :: Html ()
    routeTitle = case route of
      Route_Index -> "Posts"
      Route_Tags -> "Tags"
      Route_CV -> "CV"
      Route_Article _ -> toHtml $ title $ getMeta val
      Route_Feed -> "Feed"
    content :: Html ()
    content = case route of
      Route_Index -> do
        section_ [id_ "greeting"] $ do
          CV.md2Html greeting
          div_ [class_ "icons"] $ do
            img_ [src_ "assets/images/noun_Book_1593490.svg"]
            span_ [] "+"
            img_ [src_ "assets/images/noun_retro computer_1905469.svg"]
            span_ [] "="
            img_ [src_ "assets/images/noun_education_1909997.svg"]
        main_ [class_ "container" ] $ forM_ val $ \(r, src) -> do
          section_ [id_ "postList"] $ do
            li_ [class_ "post" ] $ do
              let meta = getMeta src
              h2_ [class_ "postTitle"] $ a_ [href_ (Rib.routeUrl r)] $ toHtml $ title meta
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
        main_ [class_ "container" ] CV.cv
      Route_Article srcPath -> do
        h1_ routeTitle
        article_ $
          Pandoc.render val
      Route_Feed -> h1_ "RSS feed in development. Coming soon."

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
greeting = [fmt|Hi. My name is Jonathan Reeve. I'm a PhD candidate in
                **computational literary analysis** at Columbia University. I write computer
                programs that help us understand novels and poetry.|]

coda :: T.Text
coda = [fmt|I believe in openness. This work is licensed under a [Creative
            Commons Attribution-NonCommercial-ShareAlike 4.0 International
            License](https://creativecommons.org/licenses/by-nc-sa/4.0/), unless
            otherwise stated. All content ©Jonathan Reeve 2020. Hand-coded with
            love, using exclusively free and open-source software, including
            [Rib](https://github.com/srid/rib) and
            [Haskell](https://haskell.org/). Hosted on
            [GitHub](https://github.com) and served with
            [Netlify](https://netlify.com). Icons by Nhor, via [The Noun
            Project](https://thenounproject.com). [Buy me a
            coffee](https://www.buymeacoffee.com/vaIVQZH) or support me [via
            Libera Pay](https://liberapay.com/JonathanReeve/donate) or Bitcoin:
            3Qvm1DwzFGk3L1Eb6yeg5Nbc6db8sZUnbK.|]



path_ :: Applicative m => [Attribute] -> HtmlT m ()
path_ = with (makeElementNoEnd "path")

viewBox_ :: Text -> Attribute
viewBox_ = makeAttribute "viewBox"

d_ :: Text -> Attribute
d_ = makeAttribute "d"

fill_ :: Text -> Attribute
fill_ = makeAttribute "fill"

svgObj_ :: Functor m => [Attribute] -> HtmlT m a -> HtmlT m a
svgObj_ = with (makeElement "svg")

svgIcon_ :: Text -> Html ()
svgIcon_ svgPath = svgObj_ [ xmlns_ "http://www.w3.org/2000/svg"
                           , viewBox_ "0 0 448 512"
                           , fill_ "#fafafa", width_ "3em" , height_ "3em"
                           ] $ path_ [ d_ svgPath ]

twitterIcon :: Html ()
twitterIcon = svgIcon_ [fmt|M400 32H48C21.5 32 0 53.5 0 80v352c0 26.5 21.5 48
48 48h352c26.5 0 48-21.5 48-48V80c0-26.5-21.5-48-48-48zm-48.9 158.8c.2 2.8.2
5.7.2 8.5 0 86.7-66 186.6-186.6 186.6-37.2 0-71.7-10.8-100.7-29.4 5.3.6 10.4.8
15.8.8 30.7 0 58.9-10.4 81.4-28-28.8-.6-53-19.5-61.3-45.5 10.1 1.5 19.2 1.5
29.6-1.2-30-6.1-52.5-32.5-52.5-64.4v-.8c8.7 4.9 18.9 7.9 29.6 8.3a65.447 65.447
0 0 1-29.2-54.6c0-12.2 3.2-23.4 8.9-33.1 32.3 39.8 80.8 65.8 135.2 68.6-9.3-44.5
24-80.6 64-80.6 18.9 0 35.9 7.9 47.9 20.7 14.8-2.8 29-8.3 41.6-15.8-4.9
15.2-15.2 28-28.8 36.1 13.2-1.4 26-5.1 37.8-10.2-8.9 13.1-20.1 24.7-32.9 34z|]

gitHubIcon :: Html ()
gitHubIcon = svgIcon_ [fmt|M400 32H48C21.5 32 0 53.5 0 80v352c0 26.5 21.5 48 48
48h352c26.5 0 48-21.5 48-48V80c0-26.5-21.5-48-48-48zM277.3 415.7c-8.4
1.5-11.5-3.7-11.5-8 0-5.4.2-33 .2-55.3 0-15.6-5.2-25.5-11.3-30.7 37-4.1 76-9.2
76-73.1 0-18.2-6.5-27.3-17.1-39 1.7-4.3 7.4-22-1.7-45-13.9-4.3-45.7 17.9-45.7
17.9-13.2-3.7-27.5-5.6-41.6-5.6-14.1 0-28.4 1.9-41.6 5.6 0
0-31.8-22.2-45.7-17.9-9.1 22.9-3.5 40.6-1.7 45-10.6 11.7-15.6 20.8-15.6 39 0
63.6 37.3 69 74.3 73.1-4.8 4.3-9.1 11.7-10.6 22.3-9.5 4.3-33.8
11.7-48.3-13.9-9.1-15.8-25.5-17.1-25.5-17.1-16.2-.2-1.1 10.2-1.1 10.2 10.8 5
18.4 24.2 18.4 24.2 9.7 29.7 56.1 19.7 56.1 19.7 0 13.9.2 36.5.2 40.6 0 4.3-3
9.5-11.5 8-66-22.1-112.2-84.9-112.2-158.3 0-91.8 70.2-161.5 162-161.5S388 165.6
388 257.4c.1 73.4-44.7 136.3-110.7
158.3zm-98.1-61.1c-1.9.4-3.7-.4-3.9-1.7-.2-1.5 1.1-2.8 3-3.2 1.9-.2 3.7.6 3.9
1.9.3 1.3-1 2.6-3 3zm-9.5-.9c0 1.3-1.5 2.4-3.5 2.4-2.2.2-3.7-.9-3.7-2.4 0-1.3
1.5-2.4 3.5-2.4 1.9-.2 3.7.9 3.7 2.4zm-13.7-1.1c-.4 1.3-2.4 1.9-4.1
1.3-1.9-.4-3.2-1.9-2.8-3.2.4-1.3 2.4-1.9 4.1-1.5 2 .6 3.3 2.1 2.8
3.4zm-12.3-5.4c-.9 1.1-2.8.9-4.3-.6-1.5-1.3-1.9-3.2-.9-4.1.9-1.1 2.8-.9 4.3.6
1.3 1.3 1.8 3.3.9 4.1zm-9.1-9.1c-.9.6-2.6 0-3.7-1.5s-1.1-3.2 0-3.9c1.1-.9 2.8-.2
3.7 1.3 1.1 1.5 1.1 3.3 0
4.1zm-6.5-9.7c-.9.9-2.4.4-3.5-.6-1.1-1.3-1.3-2.8-.4-3.5.9-.9 2.4-.4 3.5.6 1.1
1.3 1.3 2.8.4 3.5zm-6.7-7.4c-.4.9-1.7 1.1-2.8.4-1.3-.6-1.9-1.7-1.5-2.6.4-.6
1.5-.9 2.8-.4 1.3.7 1.9 1.8 1.5 2.6z|]

emailIcon :: Html ()
emailIcon = svgIcon_ [fmt|M400 32H48C21.49 32 0 53.49 0 80v352c0 26.51 21.49 48
48 48h352c26.51 0 48-21.49 48-48V80c0-26.51-21.49-48-48-48zM178.117
262.104C87.429 196.287 88.353 196.121 64 177.167V152c0-13.255 10.745-24
24-24h272c13.255 0 24 10.745 24 24v25.167c-24.371 18.969-23.434 19.124-114.117
84.938-10.5 7.655-31.392 26.12-45.883
25.894-14.503.218-35.367-18.227-45.883-25.895zM384 217.775V360c0 13.255-10.745
24-24 24H88c-13.255 0-24-10.745-24-24V217.775c13.958 10.794 33.329 25.236 95.303
70.214 14.162 10.341 37.975 32.145 64.694 32.01 26.887.134 51.037-22.041
64.72-32.025 61.958-44.965 81.325-59.406 95.283-70.199z|]
