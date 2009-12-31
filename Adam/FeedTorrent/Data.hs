module Adam.FeedTorrent.Data where

import Adam.FeedTorrent.PreludeImports

mergeAllFeeds :: [Feed] -> [Feed] -> [Feed]
mergeAllFeeds feeds newFeeds = for newFeeds $ \newFeed -> maybe newFeed (mergeFeeds newFeed)
                                                          ((find ((feedId newFeed ==) . feedId)) feeds)

mergeFeeds :: Feed -> Feed -> Feed
mergeFeeds new old = new { items = items new \\ items old }

data Feed = Feed { channelTitle :: String
                   , feedId :: String
                   , items :: [Item]
                   , location :: FilePath }
          deriving (Eq, Show, Read)

data Item = Item { itemTitle :: String
                 , itemGuid :: String
                 , enclosureUrl :: String }
             deriving (Eq, Show, Read)

uniqueFilename :: String -> String
uniqueFilename = md5'

