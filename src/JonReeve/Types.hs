{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

module JonReeve.Types where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Ema
import System.FilePath ((-<.>), (</>), takeFileName)
import Text.Pandoc.Definition (Pandoc (..))

-- ------------------------
-- Our site route
-- ------------------------

-- | Site Route
data SR = SR_Html R | SR_Feed
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
permalink fp = year </> month </> rest -<.> ".html" where
  (year, month, _, rest) = case T.splitOn "-" (T.pack (takeFileName fp)) of
    y : m : d : title -> (T.unpack y, T.unpack m, T.unpack d, T.unpack $ T.intercalate "-" title)
    _                 -> error "Malformed filename"

-- | Convert "2022/03/rethinking.html" to "posts/2022-03-12-rethinking.org"
-- | XXX: wait, nevermind, this is impossible
unPermalink :: FilePath -> FilePath
unPermaLink fp = error

instance Ema Model (Either FilePath SR) where
  encodeRoute model = \case
    Left fp -> fp
    Right (SR_Html r) ->
      -- TODO:  toString $ T.intercalate "/" (Slug.unSlug <$> toList slugs) <> ".html"
      case r of
        R_Index -> "index.html"
        R_BlogPost fp ->
          case modelLookup fp model of
            Nothing -> error "404"
            Just doc -> permalink fp
        R_Tags -> "tags.html"
        R_CV -> "cv.html"
    Right SR_Feed -> "feed.xml"

  decodeRoute _model fp = do
    -- TODO: other static toplevels
    if "assets/" `T.isPrefixOf` toText fp
      then pure $ Left $ "content" </> fp
      else
        if "images/" `T.isPrefixOf` toText fp
          then pure $ Left $ "content" </> fp
          else
            if fp == "tags.html"
              then pure $ Right $ SR_Html R_Tags
              else
                if fp == "cv.html"
                  then pure $ Right $ SR_Html R_CV
                  else
                    if fp == "feed.xml"
                      then pure $ Right $ SR_Feed
                      else do
                        if null fp
                          then pure $ Right $ SR_Html R_Index
                          else do
                            basePath <- toString <$> T.stripSuffix ".html" (toText fp)
                            pure $ Right $ SR_Html $ R_BlogPost $ basePath <> ".org"

  -- Routes to write when generating the static site.
  allRoutes (Map.keys . modelPosts -> posts) =
    (fmap (Left . ("content" </>)) ["assets", "images"])
      <> fmap
        ( Right
            . SR_Html
        )
        ( [ R_Index,
            R_CV,
            R_Tags
          ]
            <> fmap R_BlogPost posts
        )
