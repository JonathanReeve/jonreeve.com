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
import qualified Data.Text as T
import Text.Pandoc.Builder (setMeta)
import Text.Pandoc (handleError)
import Text.Pandoc.Class (runIO)
import Text.Pandoc.Citeproc (processCitations)
import Data.Maybe (fromJust)

-- ------------------------
-- Main entry point
-- ------------------------

main :: IO ()
main =
  -- runEma handles the CLI and starts the dev server (or generate static site
  -- if `gen` argument is passed).  It is designed to work well with ghcid
  -- (which is what the bin/run script uses).
  Ema.runSite_ @SR ()

instance EmaSite SR where
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
        case mkBlogPostR fp of
          Nothing -> pure id
          Just blogR -> case action of
            -- Add or update this file to the model.
            UnionMount.Refresh _ () -> do
              mData <- readOrgFile $ contentDir </> fp
              pure $ maybe id (\d -> modelInsert blogR (fp, d)) mData
            -- Remove this file from the model.
            UnionMount.Delete ->
              pure $ modelDelete blogR
      -- Parse .org -> Pandoc
      readOrgFile :: (MonadIO m, MonadLogger m) => FilePath -> m (Maybe Pandoc)
      readOrgFile fp =
        runMaybeT $ do
          logD $ "Reading " <> toText fp
          -- liftIO $ Pandoc.parse Pandoc.readOrg fp
          doc <- liftIO $ Pandoc.parse Pandoc.readOrg fp
          let docWithMeta = setMeta (T.pack "bibliography") (T.pack "content/bibliography.bib") doc :: Pandoc
          let docWithMeta' = setMeta (T.pack "csl") (T.pack "content/modern-language-association.csl") docWithMeta :: Pandoc
          docProcessed <- liftIO $ runIO $ processCitations docWithMeta'
          -- liftIO $ print docProcessed
          liftIO $ handleError docProcessed
  siteOutput = render

-- ------------------------
-- Our site HTML
-- ------------------------

render :: Prism' FilePath SR -> Model -> SR -> Ema.Asset LByteString
render rp model = \case
  SR_Static (StaticR fp) ->
    -- This instructs ema to treat this route "as is" (ie. a static file; no generation)
    -- The argument `fp` refers to the absolute path to the static file.
    Ema.AssetStatic $ "content" </> fp
  SR_Html r ->
    -- Generate a Html route; hot-reload is enabled.
    Ema.AssetGenerated Ema.Html $ renderHtml rp model r
  SR_Feed ->
    Ema.AssetGenerated Ema.Other $ RSS.renderFeed $ toPosts rp model

renderHtml :: Prism' FilePath SR -> Model -> R -> LByteString
renderHtml rp model r = do
  Lucid.renderBS $ renderPage rp r model

log :: MonadLogger m => Text -> m ()
log = logInfoNS "jonreeve"

logD :: MonadLogger m => Text -> m ()
logD = logDebugNS "jonreeve"
