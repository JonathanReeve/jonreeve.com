{-# LANGUAGE DeriveAnyClass #-}

module JoeReeve.Types where

import Data.Map.Strict qualified as Map
import JoeReeve.Main ()
import Text.Pandoc.Definition (Pandoc (..))

-- ------------------------
-- Our site route
-- ------------------------

-- | Site Route
data SR = SR_Html R | SR_Feed
  deriving (Eq, Show)

-- | Html route
data R
  = R_Index
  | R_BlogPost FilePath
  | R_Tags
  | R_CV
  deriving (Eq, Show)

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
