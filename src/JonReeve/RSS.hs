{-# LANGUAGE OverloadedStrings #-}

module JonReeve.RSS where

import Data.Maybe
import Data.Text
import Data.XML.Types as XML
import JonReeve.SiteData as SiteData
import Text.Atom.Feed qualified as Atom
import Text.Atom.Feed.Export qualified as Export
import Text.XML
import Text.XML as C
import Prelude hiding (take)

data Post = Post
  { _postedOn :: Text,
    _url :: Text,
    _content :: Text,
    _title :: Text
  }

toEntry :: Post -> Atom.Entry
toEntry (Post date url content title) =
  ( Atom.nullEntry
      url -- The ID field. Must be a link to validate.
      -- (Atom.TextString (take 20 content)) -- Title
      (Atom.TextString title)
      date
  )
    { Atom.entryAuthors = [Atom.nullPerson {Atom.personName = SiteData.name}],
      Atom.entryLinks = [Atom.nullLink url],
      Atom.entryContent = Just (Atom.HTMLContent content)
    }

feed :: [Post] -> Atom.Feed
feed posts =
  Atom.nullFeed
    (SiteData.domain <> "/feed.xml") -- ID
    (Atom.TextString SiteData.siteName) -- Title
    ( case posts of -- Updated
        Post latestPostDate _ _ _ : _ -> latestPostDate
        _ -> ""
    )

renderFeed :: [Post] -> LByteString
renderFeed posts =
  fromMaybe "RSS feed broken! :(" $
    fmap (renderLBS def) $
      elementToDoc $
        Export.xmlFeed $
          (feed posts)
            { Atom.feedEntries = fmap toEntry posts,
              Atom.feedLinks = [Atom.nullLink SiteData.domain]
            }

elementToDoc :: XML.Element -> Maybe C.Document
elementToDoc el =
  either (const Nothing) Just $ fromXMLDocument $ XML.Document (Prologue [] Nothing []) el []
