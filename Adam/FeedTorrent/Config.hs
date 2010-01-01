-- | Handling of the configuration file.

{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}
module Adam.FeedTorrent.Config where

import Data.Generics.SYB.WithClass.Derive
import Text.RJson

import Adam.FeedTorrent.Imports
import Adam.FeedTorrent.Data

-- | Configuration record, contains all user settings.
data Config = Config { feedUrls :: [Url]
                     , torrentDir :: FilePath
                     , feedDir :: FilePath
                     , newFeedDir :: FilePath
                     } deriving (Eq, Show, Read)

$(derive[''Config])
configFromJson :: String -> Either String Config
configFromJson = fromJsonString (undefined :: Config)

-- | Default configuration settings.
defaultConfig :: Config
defaultConfig =
  Config { feedUrls = []
         , torrentDir = "torrents"
         , feedDir = "feeds"
         , newFeedDir = "newfeeds" }

-- | Default path to config file.
jsonFile :: FilePath
jsonFile = "config.json"

-- | Parses and fetches the config from disk, or returns a JSON parse
--   error message. If the config file does not exist one is created
--   at the specified path.
getConfig :: IO (Either String Config)
getConfig = do
  t <- doesFileExist jsonFile
  unless t $ writeConfig defaultConfig
  configFromJson <$> readFile jsonFile

-- | Writes a configuration to disk.
writeConfig :: Config -> IO ()
writeConfig = writeFile jsonFile . show . toJson

-- | Path to feed that have not been processed. There will be no file
--   here if the file has been processed.
newFeedPath :: Config -> Feed -> FilePath
newFeedPath cfg feed = newFeedDir cfg </> feedId feed

-- | Path to feed that have been processed. There will be only be a
--   file here if the feed has ever been processed.
oldFeedPath :: Config -> Feed -> FilePath
oldFeedPath cfg feed = feedDir cfg </> feedId feed

-- | The local path to a torrent file belonging to an item. If the
-- | torrent hasn't been fetched the file won't exist.
enclosurePath :: Config -> Item -> FilePath
enclosurePath cfg item = torrentDir cfg </> itemTitle item ++ ".torrent"
