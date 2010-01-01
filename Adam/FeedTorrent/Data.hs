-- | Data declarations

{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}module Adam.FeedTorrent.Data where

import Data.Generics.SYB.WithClass.Derive

import Adam.FeedTorrent.Imports

-- | Given a list of old (processed) and new (unprocessed) feeds,
--   returns all feeds that didn't exist (are not in old) and removes
--   all items from the other new feeds that are also in the
--   corresponding old feed.
mergeAllFeeds :: [Feed] -> [Feed] -> [Feed]
mergeAllFeeds feeds newFeeds = for newFeeds $ \newFeed -> maybe newFeed (mergeFeeds newFeed)
                                                          ((find ((feedId newFeed ==) . feedId)) feeds)

-- | Removes all processed items from a neew feed.
mergeFeeds :: Feed -> Feed -> Feed
mergeFeeds new old = new { items = items new \\ items old }

-- | Representation of an RSS document.
data Feed = Feed { channelTitle :: String
                   , feedId :: String
                   , items :: [Item]
                   , location :: FilePath }
          deriving (Eq, Show, Read)

-- | Representation of an item in an RSS document. Each item contains
--   a link to a torrent.
data Item = Item { itemTitle :: String
                 , itemGuid :: String
                 , enclosureUrl :: String }
             deriving (Eq, Show, Read)

-- | Wrapper for Urls to avoid mixing up argument orders.
newtype Url = Url String
            deriving (Eq,Show,Read)
$(derive[''Url])

-- | Converts from Url to String.
fromUrl :: Url -> String
fromUrl (Url s) = s

-- | Generates a probabilistically unique filename from an Url.
uniqueFilename :: Url -> String
uniqueFilename = md5' . fromUrl
