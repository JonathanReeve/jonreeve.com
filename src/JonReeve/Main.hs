{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module JonReeve.Main where

-- Needed for citation processing

-- My modules

import Clay qualified as C
import Data.Aeson (FromJSON, fromJSON)
import Data.Aeson qualified as Aeson
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Ema qualified
import JonReeve.CSS qualified as CSS
import JonReeve.CV qualified as CV
import JonReeve.Pandoc qualified as Pandoc
import JonReeve.RSS qualified as RSS
import JonReeve.SiteData qualified as SiteData
import JonReeve.Types
import Lucid
import Lucid.Base
import Optics.Core (Prism')
import PyF
import Text.Pandoc (Pandoc)

parseJekyllFilename :: FilePath -> (String, String, String, String)
parseJekyllFilename fn =
  case T.splitOn "-" (T.pack fn) of
    y : m : d : rest ->
      (T.unpack y, T.unpack m, T.unpack d, T.unpack $ T.intercalate "-" rest)
    _ ->
      error "Malformed filename"

-- | Convert the posts we've read into Post types that can be read
-- by the RSS/Atom module.
toPosts :: Prism' FilePath SR -> Model -> [RSS.Post]
toPosts rp model =
  flip fmap (Map.toList $ modelPosts model) $ \(fp, doc) ->
    let r = R_BlogPost fp
     in toPost r doc
  where
    toPost :: R -> Pandoc -> RSS.Post
    toPost r doc = RSS.Post postDate postUrl postContent postTitle
      where
        postDate = date (getMeta doc)
        postUrl = SiteData.domain <> routeUrl rp r
        postContent = pandocToText doc
        postTitle = title (getMeta doc)
    pandocToText :: Pandoc -> T.Text
    pandocToText doc = LT.toStrict $ Lucid.renderText $ Pandoc.render doc

stylesheet url = link_ [rel_ "stylesheet", href_ url]

script url = script_ [src_ url, async_ T.empty] T.empty

-- | Define your site HTML here
renderPage :: Prism' FilePath SR -> R -> Model -> Html ()
renderPage rp route val = html_ [lang_ "en"] $ do
  head_ $ do
    meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
    title_ routeTitle
    base_ [href_ "/"]
    mapM_
      stylesheet
      [ "/assets/css/spectre.min.css",
        "//fonts.googleapis.com/css?family=Montserrat|Raleway",
        "//cdn.jsdelivr.net/gh/highlightjs/cdn-release@10.2.0/build/styles/default.min.css"
      ]
    link_ [rel_ "icon", href_ "/images/favicon.svg", sizes_ "any", type_ "image/svg+xml"]
    -- <link rel="icon" href="images/favicon.svg" sizes="any" type="image/svg+xml">
    style_ [type_ "text/css"] $ C.render CSS.pageStyle
    style_ [type_ "text/css"] ("@page{margin: 3cm;}" :: Html ())
  body_ $ do
    div_ [id_ "headerWrapper"] $ do
      header_ [class_ "navbar"] $ do
        section_ [class_ "navbar-section"] $ do
          a_ [class_ "navbar-brand", href_ $ routeUrl rp R_Index] $ do
            "Jonathan Reeve: "
            span_ [] "Computational Literary Analysis"
        section_ [class_ "navbar-section"] $ do
          ul_ [class_ "nav"] $ do
            navItem (SR_Html R_Index) "posts"
            navItem (SR_Html R_CV) "cv"
            navItem (SR_Html R_Tags) "tags"
            navItem SR_Feed "feed"
    div_ [class_ "container"] $ do
      content
    footer_ [] $ do
      div_ [class_ "container"] $ do
        div_ [class_ "columns"] $ do
          div_ [class_ "column col-8"] $ CV.md2Html SiteData.coda
          div_ [class_ "column col-4"] $ do
            div_ [class_ "icons"] $ do
              a_ [href_ "http://github.com/JonathanReeve"] gitHubIcon
              a_ [href_ "http://twitter.com/j0_0n"] twitterIcon
              a_ [href_ "mailto:jonathan@jonreeve.com"] emailIcon
        -- a_ [href_ "https://matrix.to/#/@jon.reeve:matrix.org"] chatIcon
        script_
          [ makeAttribute "data-goatcounter" "https://jonreeve.goatcounter.com/count",
            async_ T.empty,
            src_ "//gc.zgo.at/count.js"
          ]
          T.empty
        script_ [src_ "/assets/js/jquery-3.5.1.min.js"] T.empty
        script_ [src_ "/assets/js/main.js"] T.empty
        script_ [src_ "//cdn.jsdelivr.net/gh/highlightjs/cdn-release@10.2.0/build/highlight.min.js"] T.empty
        script_ [src_ "https://polyfill.io/v3/polyfill.min.js?features=es6"] T.empty
        script_ [id_ "MathJax-script", async_ "", src_ "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"] T.empty
        script_ [] ("hljs.initHighlightingOnLoad();" :: Html ())
  where
    navItem :: SR -> Html () -> Html ()
    navItem navRoute label = li_ [class_ "nav-item"] $ a_ [href_ url] label
      where
        url = case navRoute of
          SR_Html r -> routeUrl rp r
          SR_Feed -> "feed.xml"

    routeTitle :: Html ()
    routeTitle = case route of
      R_Index -> "Posts"
      R_Tags -> "Tags"
      R_CV -> "CV"
      R_BlogPost k ->
        toHtml $ title $ maybe (error "missing!") getMeta $ modelLookup (traceShowId k) val
    content :: Html ()
    content = case route of
      R_Index -> do
        section_ [id_ "greeting"] $ do
          CV.md2Html SiteData.greeting
          div_ [class_ "icons"] $ do
            img_ [src_ "assets/images/noun_Book_1593490.svg"]
            img_ [src_ "assets/images/plus.svg"]
            img_ [src_ "assets/images/noun_retro computer_1905469.svg"]
            img_ [src_ "assets/images/equal.svg"]
            img_ [src_ "assets/images/noun_education_1909997.svg"]
        main_ [class_ "container"] $
          forM_ (List.reverse $ Map.toList $ modelPosts val) $ \(r, src) -> do
            section_ [id_ "postList"] $ do
              li_ [class_ "post"] $ do
                let meta = getMeta src
                div_ [vocab_ "https://schema.org", typeof_ "blogPosting"] $ do
                  h2_ [class_ "postTitle", property_ "headline"] $ a_ [href_ (routeUrl rp $ R_BlogPost r)] $ toHtml $ title meta
                  p_ [class_ "meta"] $ do
                    span_ [class_ "date", property_ "datePublished", content_ (date meta)] $ toHtml $ T.concat ["(", date meta, ")"]
                    span_ [class_ "tags", property_ "keywords", content_ (T.intercalate "," (tags meta))] $ do
                      " in"
                      mapM_ mkTag (tags meta)
        where
          mkTag :: Text -> Html ()
          mkTag tag = a_ [class_ "chip", rel_ "tag", href_ ("/tags/#" <> tag)] $ toHtml tag
      R_Tags -> do
        h1_ routeTitle
        let rposts = Map.toList $ modelPosts val
            tags = groupByTag rposts
        div_ $
          forM_ (sortOn (T.toLower . fst) $ Map.toList tags) $ \(tag, rs) -> do
            a_ [id_ tag] mempty
            h2_ $ toHtml tag
            ul_ $
              forM_ rs $ \(r, src) -> do
                li_ [class_ "pages"] $ do
                  let meta = getMeta src
                  b_ $ a_ [href_ (routeUrl rp $ R_BlogPost r)] $ toHtml $ title meta
      R_CV -> do
        main_ [class_ "container"] $ do
          h1_ "Curriculum Vitae"
          h2_ "Jonathan Reeve"
          CV.cv
      R_BlogPost srcPath -> do
        h1_ routeTitle
        let (y, m, d, _) = parseJekyllFilename srcPath
        p_ [fmt|Posted {y}-{m}-{d}|]
        article_ $
          case modelLookup srcPath val of
            Nothing -> error "missing"
            Just doc -> Pandoc.render doc
        hr_ []
        p_ [] "I welcome your comments and annotations in the Hypothes.is sidebar to the right. →"
        script_ [src_ "https://hypothes.is/embed.js"] T.empty

groupByTag :: Foldable t => t (a, Pandoc) -> Map Text [(a, Pandoc)]
groupByTag as =
  Map.fromListWith (<>) $
    flip concatMap as $ \(r, doc) ->
      (,[(r, doc)]) <$> tags (getMeta doc)

-- | This is rather complicated.
-- @#+tags:@ and @#+filetags:@ aren't recognized by Pandoc as metadata tags.
-- But @keywords@ is. So we'll use keywords but then parse them here as tags.
data SrcMeta = SrcMeta
  { title :: Text,
    date :: Text,
    tags :: [Text]
  }
  deriving (Show, Eq, Generic)

instance FromJSON SrcMeta where
  parseJSON (Aeson.Object v) = do
    title <- v Aeson..: "title"
    date <- v Aeson..: "date"
    tags <- v Aeson..: "keywords"
    return SrcMeta {title = title, date = date, tags = map T.strip (T.splitOn ";" tags)}

-- | Get metadata from Markdown's YAML block
getMeta :: Pandoc -> SrcMeta
getMeta src = case Pandoc.extractMeta src of
  Nothing -> error "No YAML metadata"
  Just (Left e) -> error e
  Just (Right val) -> case fromJSON val of
    Aeson.Error e -> do
      error $ "JSON error: " <> show e <> " in " <> show val
    Aeson.Success v -> v

-- Schema.org RDFa
vocab_, typeof_, property_ :: Text -> Attribute
vocab_ = makeAttribute "vocab"
typeof_ = makeAttribute "typeof"
property_ = makeAttribute "property"

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
svgIcon_ svgPath =
  svgObj_
    [ xmlns_ "http://www.w3.org/2000/svg",
      viewBox_ "0 0 448 512",
      fill_ "#fafafa",
      width_ "3em",
      height_ "3em"
    ]
    $ path_ [d_ svgPath]

twitterIcon :: Html ()
twitterIcon =
  svgIcon_
    [fmt|M400 32H48C21.5 32 0 53.5 0 80v352c0 26.5 21.5 48
48 48h352c26.5 0 48-21.5 48-48V80c0-26.5-21.5-48-48-48zm-48.9 158.8c.2 2.8.2
5.7.2 8.5 0 86.7-66 186.6-186.6 186.6-37.2 0-71.7-10.8-100.7-29.4 5.3.6 10.4.8
15.8.8 30.7 0 58.9-10.4 81.4-28-28.8-.6-53-19.5-61.3-45.5 10.1 1.5 19.2 1.5
29.6-1.2-30-6.1-52.5-32.5-52.5-64.4v-.8c8.7 4.9 18.9 7.9 29.6 8.3a65.447 65.447
0 0 1-29.2-54.6c0-12.2 3.2-23.4 8.9-33.1 32.3 39.8 80.8 65.8 135.2 68.6-9.3-44.5
24-80.6 64-80.6 18.9 0 35.9 7.9 47.9 20.7 14.8-2.8 29-8.3 41.6-15.8-4.9
15.2-15.2 28-28.8 36.1 13.2-1.4 26-5.1 37.8-10.2-8.9 13.1-20.1 24.7-32.9 34z|]

gitHubIcon :: Html ()
gitHubIcon =
  svgIcon_
    [fmt|M400 32H48C21.5 32 0 53.5 0 80v352c0 26.5 21.5 48 48
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
emailIcon =
  svgIcon_
    [fmt|M400 32H48C21.49 32 0 53.49 0 80v352c0 26.51 21.49 48
48 48h352c26.51 0 48-21.49 48-48V80c0-26.51-21.49-48-48-48zM178.117
262.104C87.429 196.287 88.353 196.121 64 177.167V152c0-13.255 10.745-24
24-24h272c13.255 0 24 10.745 24 24v25.167c-24.371 18.969-23.434 19.124-114.117
84.938-10.5 7.655-31.392 26.12-45.883
25.894-14.503.218-35.367-18.227-45.883-25.895zM384 217.775V360c0 13.255-10.745
24-24 24H88c-13.255 0-24-10.745-24-24V217.775c13.958 10.794 33.329 25.236 95.303
70.214 14.162 10.341 37.975 32.145 64.694 32.01 26.887.134 51.037-22.041
64.72-32.025 61.958-44.965 81.325-59.406 95.283-70.199z|]

chatIcon :: Html ()
chatIcon =
  svgIcon_
    [fmt|M447.1 0h-384c-35.25 0-64 28.75-64 63.1v287.1c0 35.25
28.75 63.1 64 63.1h96v83.98c0 9.836 11.02 15.55 19.12 9.7l124.9-93.68h144c35.25
0 64-28.75 64-63.1V63.1C511.1 28.75 483.2 0 447.1 0zM464 352c0 8.75-7.25 16-16
16h-160l-80 60v-60H64c-8.75 0-16-7.25-16-16V64c0-8.75 7.25-16 16-16h384c8.75 0
16 7.25 16 16V352z|]

routeUrl :: Prism' FilePath SR -> R -> Text
routeUrl rp htmlR =
  Ema.routeUrl rp $ SR_Html htmlR
