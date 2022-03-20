{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Monad.Logger
import Data.Map.Strict qualified as Map
import Data.Some (Some)
import Data.Text qualified as T
import Ema (Ema (..))
import Ema qualified
import Ema.CLI qualified
import JoeReeve.Main ()
import JoeReeve.Pandoc qualified as Pandoc
import JoeReeve.Types
import System.FilePath ((</>))
import System.UnionMount qualified as UnionMount
import Text.Pandoc.Definition (Pandoc (..))

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
          -- Add or update this file to the model.
          UnionMount.Refresh _ () -> do
            mData <- readOrgFile $ contentDir </> fp
            pure $ maybe id (\d -> modelInsert fp d) mData
          -- Remove this file from the model.
          UnionMount.Delete ->
            pure $ modelDelete fp
  where
    -- Parse .org -> Pandoc
    readOrgFile :: (MonadIO m, MonadLogger m) => FilePath -> m (Maybe Pandoc)
    readOrgFile fp =
      runMaybeT $ do
        logD $ "Reading " <> toText fp
        doc <- liftIO $ Pandoc.parse Pandoc.readOrg fp
        -- TODO: pandoc parser  for org
        pure doc

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
renderHtml _emaAction model r = do
  show model
