{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Monad.Logger
import Data.Default
import Ema
import JonReeve.Main (renderPage, toPosts)
import JonReeve.Pandoc qualified as Pandoc
import JonReeve.RSS qualified as RSS
import JonReeve.Types
import Lucid qualified
import Optics.Core
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
  Ema.runSite_ @(Either FilePath SR) ()

instance EmaSite (Either FilePath SR) where
  siteInput _ _ = do
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
    Dynamic <$> UnionMount.mount contentDir pats ignorePats def (const $ handleUpdate contentDir)
    where
      handleUpdate contentDir fp action = do
        logD $ "fsnotify changed: " <> toText fp
        case action of
          -- Add or update this file to the model.
          UnionMount.Refresh _ () -> do
            mData <- readOrgFile $ contentDir </> fp
            pure $ maybe id (\d -> modelInsert fp d) mData
          -- Remove this file from the model.
          UnionMount.Delete ->
            pure $ modelDelete fp
      -- Parse .org -> Pandoc
      readOrgFile :: (MonadIO m, MonadLogger m) => FilePath -> m (Maybe Pandoc)
      readOrgFile fp =
        runMaybeT $ do
          logD $ "Reading " <> toText fp
          liftIO $ Pandoc.parse Pandoc.readOrg fp
  siteOutput = render

-- ------------------------
-- Our site HTML
-- ------------------------

render :: Prism' FilePath (Either FilePath SR) -> Model -> Either FilePath SR -> Ema.Asset LByteString
render rp model = \case
  Left fp ->
    -- This instructs ema to treat this route "as is" (ie. a static file; no generation)
    -- The argument `fp` refers to the absolute path to the static file.
    Ema.AssetStatic fp
  Right (SR_Html r) ->
    -- Generate a Html route; hot-reload is enabled.
    Ema.AssetGenerated Ema.Html $ renderHtml rp model r
  Right SR_Feed ->
    Ema.AssetGenerated Ema.Other $ RSS.renderFeed $ toPosts rp model

renderHtml :: Prism' FilePath (Either FilePath SR) -> Model -> R -> LByteString
renderHtml rp model r = do
  Lucid.renderBS $ renderPage rp r model

log :: MonadLogger m => Text -> m ()
log = logInfoNS "jonreeve"

logD :: MonadLogger m => Text -> m ()
logD = logDebugNS "jonreeve"
