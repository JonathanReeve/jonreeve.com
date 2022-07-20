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
import System.FilePath ((</>))
import Text.Pandoc.Definition (Pandoc (..))

-- ------------------------
-- Our site model
-- ------------------------

data Model = Model
  { modelPosts :: Map BlogPostR (FilePath, Pandoc)
  }
  deriving stock (Eq, Show, Generic)

instance Default Model where
  def = Model mempty

emptyModel :: Model
emptyModel = Model mempty

modelLookup :: BlogPostR -> Model -> Maybe (FilePath, Pandoc)
modelLookup k =
  Map.lookup k . modelPosts

modelInsert :: BlogPostR -> (FilePath, Pandoc) -> Model -> Model
modelInsert k v model =
  let xs = Map.insert k v (modelPosts model)
   in model
        { modelPosts = xs
        }

modelDelete :: BlogPostR -> Model -> Model
modelDelete k model =
  model
    { modelPosts = Map.delete k (modelPosts model)
    }

-- ------------------------
-- Our site route types
-- ------------------------

newtype BlogPostR = BlogPostR FilePath
  deriving stock (Eq, Show, Ord, Generic)

mkBlogPostR :: FilePath -> Maybe BlogPostR
mkBlogPostR fp = do
  ["posts", fn] <- pure $ T.splitOn "/" $ T.pack fp
  (year : month : _ : slug) <- pure $ T.splitOn "-" fn
  slugWithoutExt <- T.stripSuffix ".org" $ T.intercalate "-" slug
  pure $ BlogPostR $ toString $ T.intercalate "-" [year, month, slugWithoutExt]

instance IsRoute BlogPostR where
  type RouteModel BlogPostR = Map BlogPostR (FilePath, Pandoc)
  routePrism model =
    toPrism_ $ prism' encode decode
    where
      encode (BlogPostR slug) = slug </> "index.html"
      decode fp = do
        r <- BlogPostR . toString <$> T.stripSuffix "/index.html" (toText fp)
        guard $ Map.member r model
        pure r
  routeUniverse = Map.keys

-- | Html route
data R
  = R_Index
  | R_Tags
  | R_CV
  | R_BlogPost BlogPostR
  deriving stock (Eq, Show)

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

-- | Site Route
data SR
  = SR_Html R
  | SR_Feed
  | SR_Static FilePath
  deriving stock (Eq, Show)

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
