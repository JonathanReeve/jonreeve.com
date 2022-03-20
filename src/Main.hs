{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Control.Monad.Logger
import Data.Map.Strict qualified as Map
import Data.Some (Some)
import Data.Text qualified as T
import Ema (Ema (..))
import Ema qualified
import Ema.CLI qualified
import System.FilePath ((</>))
import System.UnionMount qualified as UnionMount
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

-- | Once we have a "model" and "route" (as defined above), we should define the
-- @Ema@ typeclass to tell Ema how to decode/encode our routes, as well as the
-- list of routes to generate the static site with.
--
-- We use `Either` to represent either a static file route or a Markdown
-- generated route.
instance Ema Model (Either FilePath SR) where
  encodeRoute _model = \case
    Left fp -> fp
    Right sr ->
      -- TODO:  toString $ T.intercalate "/" (Slug.unSlug <$> toList slugs) <> ".html"
      undefined

  decodeRoute _model fp = do
    -- TODO: other static toplevels
    if "assets/" `T.isPrefixOf` toText fp
      then pure $ Left fp
      else do
        if null fp
          then pure $ Right $ SR_Html R_Index
          else do
            basePath <- toString <$> T.stripSuffix ".html" (toText fp)
            pure $ Right $ SR_Html $ R_BlogPost $ basePath <> ".md"

  -- Routes to write when generating the static site.
  allRoutes (Map.keys . modelPosts -> posts) =
    [Left "assets"]
      <> fmap Right []

-- ------------------------
-- Main entry point
-- ------------------------

log :: MonadLogger m => Text -> m ()
log = logInfoNS "jonreeve"

logD :: MonadLogger m => Text -> m ()
logD = logDebugNS "jonreeve"

main :: IO ()
main =
  -- runEma handles the CLI and starts the dev server (or generate static site
  -- if `gen` argument is passed).  It is designed to work well with ghcid
  -- (which is what the bin/run script uses).
  void $
    Ema.runEma render $ \_act model -> do
      let model0 = emptyModel
      -- This is the place where we can load and continue to modify our "model".
      -- You will use `LVar.set` and `LVar.modify` to modify the model.
      --
      -- It is a run in a (long-running) thread of its own.
      --
      -- We use the FileSystem helper to directly "mount" our files on to the
      -- LVar.
      let pats = [((), "**/*.org")]
          ignorePats = [".*"]
          contentDir = "content"
      void . UnionMount.mountOnLVar contentDir pats ignorePats model model0 $ \() fp action -> do
        logD $ "fsnotify changed: " <> toText fp
        case action of
          UnionMount.Refresh _ () -> do
            mData <- readSource $ contentDir </> fp
            pure $ maybe id (\d -> modelInsert fp d) mData
          UnionMount.Delete ->
            pure $ modelDelete fp
  where
    -- Parse .org -> Pandoc
    readSource :: (MonadIO m, MonadLogger m) => FilePath -> m (Maybe Pandoc)
    readSource fp =
      runMaybeT $ do
        logD $ "Reading " <> toText fp
        s <- readFileText fp
        -- TODO: pandoc parser  for org
        pure $ Pandoc mempty mempty

newtype BadMarkdown = BadMarkdown Text
  deriving stock (Show)
  deriving anyclass (Exception)

-- ------------------------
-- Our site HTML
-- ------------------------

render :: Some Ema.CLI.Action -> Model -> Either FilePath SR -> Ema.Asset LByteString
render act model = \case
  Left fp ->
    -- This instructs ema to treat this route "as is" (ie. a static file; no generation)
    -- The argument `fp` refers to the absolute path to the static file.
    Ema.AssetStatic fp
  Right r ->
    -- Generate a Html route; hot-reload is enabled.
    Ema.AssetGenerated Ema.Html $ renderHtml act model r

renderHtml :: Some Ema.CLI.Action -> Model -> SR -> LByteString
renderHtml emaAction model r = do
  show model