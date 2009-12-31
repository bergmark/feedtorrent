module Adam.FeedTorrent.Data where

import Adam.FeedTorrent.PreludeImports

data Item = Item { itemTitle :: String
                 , itemGuid :: String
                 , enclosureUrl :: String }
             deriving (Eq, Show, Read)

uniqueFilename :: String -> String
uniqueFilename = md5'

data Feed = Feed { channelTitle :: String
                   , feedId :: String
                   , items :: [Item]
                   , location :: FilePath }
          deriving (Eq, Show, Read)
