{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Monad.Logger
import Data.Some (Some)
import Ema qualified
import Ema.CLI qualified
import JoeReeve.Main (renderPage, toPosts)
import JoeReeve.Pandoc qualified as Pandoc
import JoeReeve.RSS qualified as RSS
import JoeReeve.Types
import Lucid qualified
import System.FilePath ((</>))
import System.UnionMount qualified as UnionMount
import Text.Pandoc.Definition (Pandoc (..))

-- ------------------------
-- Main entry point
-- ------------------------

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
      let pats = [((), "posts/**/*.org")]
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
        pure doc

-- ------------------------
-- Our site HTML
-- ------------------------

render :: Some Ema.CLI.Action -> Model -> Either FilePath SR -> Ema.Asset LByteString
render act model = \case
  Left fp ->
    -- This instructs ema to treat this route "as is" (ie. a static file; no generation)
    -- The argument `fp` refers to the absolute path to the static file.
    Ema.AssetStatic fp
  Right (SR_Html r) ->
    -- Generate a Html route; hot-reload is enabled.
    Ema.AssetGenerated Ema.Html $ renderHtml act model r
  Right SR_Feed ->
    Ema.AssetGenerated Ema.Other $ RSS.renderFeed $ toPosts model

renderHtml :: Some Ema.CLI.Action -> Model -> R -> LByteString
renderHtml _emaAction model r = do
  Lucid.renderBS $ renderPage r model

log :: MonadLogger m => Text -> m ()
log = logInfoNS "jonreeve"

logD :: MonadLogger m => Text -> m ()
logD = logDebugNS "jonreeve"
