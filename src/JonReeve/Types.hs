{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module JonReeve.Types where

import Data.Default
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Ema
import Ema.Route.Generic.TH
import Optics.Core (prism')
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
  pure $ BlogPostR $ toString $ T.intercalate "/" [year, month, slugWithoutExt]

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
  deriving stock (Eq, Show, Generic)

deriveGeneric ''R
deriveIsRoute
  ''R
  [t|
    '[ WithModel Model,
       WithSubRoutes
         '[ FileRoute "index.html",
            FileRoute "tags.html",
            FileRoute "cv/index.html",
            BlogPostR
          ]
     ]
    |]

-- | Static file route
newtype StaticR = StaticR {unStaticR :: FilePath}
  deriving stock (Eq, Show, Ord, Generic)

instance IsRoute StaticR where
  type RouteModel StaticR = ()
  routePrism () =
    toPrism_ . prism' unStaticR $ \fp -> do
      _ <- asum $ allowedStaticFolders <&> \dir -> T.stripPrefix (toText dir) (toText fp)
      pure $ StaticR fp
  routeUniverse () =
    fmap StaticR allowedStaticFolders

-- | Folders under ./content to serve as static content
allowedStaticFolders :: [FilePath]
allowedStaticFolders = ["assets", "images", "presentations", "projects"]

-- | Site Route
data SR
  = SR_Html R
  | SR_Feed
  | SR_Static StaticR
  deriving stock (Eq, Show, Generic)

deriveGeneric ''SR
deriveIsRoute
  ''SR
  [t|
    '[ WithModel Model,
       WithSubRoutes
         '[ R,
            FileRoute "feed.xml",
            StaticR
          ]
     ]
    |]
