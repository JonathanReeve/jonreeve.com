{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

module JonReeve.Types where

import Data.Default
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Ema
import Optics.Core (prism')
import System.FilePath (takeFileName, (-<.>), (</>))
import Text.Pandoc.Definition (Pandoc (..))

-- ------------------------
-- Our site route
-- ------------------------

-- | Site Route
data SR
  = SR_Html R
  | SR_Feed
  | SR_Static FilePath
  deriving stock (Eq, Show)

-- | Html route
data R
  = R_Index
  | R_BlogPost FilePath
  | R_Tags
  | R_CV
  deriving stock (Eq, Show)

-- ------------------------
-- Our site model
-- ------------------------

data Model = Model
  { modelPosts :: Map FilePath Pandoc
  }
  deriving stock (Eq, Show)

instance Default Model where
  def = Model mempty

emptyModel :: Model
emptyModel = Model mempty

modelLookup :: FilePath -> Model -> Maybe Pandoc
modelLookup k =
  Map.lookup k . modelPosts

modelInsert :: FilePath -> Pandoc -> Model -> Model
modelInsert k v model =
  let xs = Map.insert k v (modelPosts model)
   in model
        { modelPosts = xs
        }

modelDelete :: FilePath -> Model -> Model
modelDelete k model =
  model
    { modelPosts = Map.delete k (modelPosts model)
    }

-- | Convert "posts/2022-03-12-rethinking.org" to "2022/03/rethinking.html"
-- for backwards compatibility with my existing URLs.
permalink :: FilePath -> FilePath
permalink fp = year </> month </> rest -<.> ".html"
  where
    (year, month, _, rest) = case T.splitOn "-" (T.pack (takeFileName fp)) of
      y : m : d : title -> (T.unpack y, T.unpack m, T.unpack d, T.unpack $ T.intercalate "-" title)
      _ -> error "Malformed filename"

-- | Convert "2022/03/rethinking.html" to "posts/2022-03-12-rethinking.org"
-- | XXX: wait, nevermind, this is impossible
unPermaLink :: FilePath -> FilePath
unPermaLink fp = undefined

-- TODO: Use generics for this
instance IsRoute SR where
  type RouteModel SR = Model
  routePrism model =
    toPrism_ $ prism' encodeRoute decodeRoute
    where
      encodeRoute = \case
        SR_Static fp -> fp
        SR_Html r ->
          -- TODO:  toString $ T.intercalate "/" (Slug.unSlug <$> toList slugs) <> ".html"
          case r of
            R_Index -> "index.html"
            R_BlogPost fp ->
              case modelLookup (traceShowId fp) model of
                Nothing -> error "404"
                Just doc -> permalink fp
            R_Tags -> "tags.html"
            R_CV -> "cv.html"
        SR_Feed -> "feed.xml"

      decodeRoute fp = do
        -- TODO: other static toplevels
        if "assets/" `T.isPrefixOf` toText fp
          then pure $ SR_Static fp
          else
            if "images/" `T.isPrefixOf` toText fp
              then pure $ SR_Static fp
              else
                if fp == "tags.html"
                  then pure $ SR_Html R_Tags
                  else
                    if fp == "cv.html"
                      then pure $ SR_Html R_CV
                      else
                        if fp == "feed.xml"
                          then pure $ SR_Feed
                          else do
                            if fp == "index.html" || fp == "" -- FIXME
                              then pure $ SR_Html R_Index
                              else do
                                basePath <- toString <$> T.stripSuffix ".html" (toText fp)
                                pure $ SR_Html $ R_BlogPost $ basePath <> ".org"

  -- Routes to write when generating the static site.
  routeUniverse (Map.keys . modelPosts -> posts) =
    (fmap SR_Static ["assets", "images"])
      <> fmap
        SR_Html
        ( [ R_Index,
            R_CV,
            R_Tags
          ]
            <> fmap R_BlogPost posts
        )
