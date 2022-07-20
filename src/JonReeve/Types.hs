{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module JonReeve.Types where

import Data.Default
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Ema
import Ema.Route.Generic.TH
import Optics.Core (preview, prism', review)
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
  | R_Tags
  | R_CV
  | R_BlogPost BlogPostR
  deriving stock (Eq, Show)

newtype BlogPostR = BlogPostR FilePath
  deriving stock (Eq, Show, Generic)

-- ------------------------
-- Our site model
-- ------------------------

data Model = Model
  { modelPosts :: Map FilePath Pandoc
  }
  deriving stock (Eq, Show, Generic)

deriveGeneric ''R
deriveIsRoute
  ''R
  [t|
    '[ WithModel Model,
       WithSubRoutes
         '[ FileRoute "index.html",
            FileRoute "tags.html",
            FileRoute "cv.html",
            BlogPostR
          ]
     ]
    |]

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

instance IsRoute BlogPostR where
  type RouteModel BlogPostR = Map FilePath Pandoc
  routePrism _model =
    toPrism_ $ prism' encode decode
    where
      encode (BlogPostR slug) = slug -<.> "" </> "index.html"
      decode fp = do
        _ <- T.stripPrefix "posts" (toText fp) -- HACK: check that it is inside posts
        baseFile <- toString <$> T.stripSuffix "/index.html" (toText fp)
        pure $ BlogPostR $ baseFile -<.> ".org"
  routeUniverse = fmap BlogPostR . Map.keys

-- | Convert "posts/2022-03-12-rethinking.org" to "2022/03/rethinking.html"
-- for backwards compatibility with my existing URLs.
permalink :: FilePath -> FilePath
permalink fp = year </> month </> rest -<.> "" </> "index.html"
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
          review (fromPrism_ $ routePrism @R model) r
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
                                SR_Html <$> preview (fromPrism_ $ routePrism @R model) fp

  -- Routes to write when generating the static site.
  routeUniverse m =
    fmap SR_Static ["assets", "images"]
      <> fmap
        SR_Html
        ( [ R_Index,
            R_CV,
            R_Tags
          ]
            <> fmap R_BlogPost (routeUniverse $ modelPosts m)
        )
