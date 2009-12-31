{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}
module Adam.FeedTorrent.Config where

import Data.Generics.SYB.WithClass.Derive
import Text.RJson

import Adam.FeedTorrent.Url
import Adam.FeedTorrent.PreludeImports
import Adam.FeedTorrent.Data

data Config = Config { feedUrls :: [Url]
                     , torrentDir :: FilePath
                     , feedDir :: FilePath
                     , newFeedDir :: FilePath
                     } deriving (Eq, Show, Read)

$(derive[''Config])
configFromJson :: String -> Either String Config
configFromJson = fromJsonString (undefined :: Config)

defaultConfig :: Config
defaultConfig =
  Config { feedUrls = []
         , torrentDir = "torrents"
         , feedDir = "feeds"
         , newFeedDir = "newfeeds" }

jsonFile :: FilePath
jsonFile = "config.json"

getConfig :: IO (Either String Config)
getConfig = do
  t <- doesFileExist jsonFile
  when (not t) $ writeConfig defaultConfig
  configFromJson <$> readFile jsonFile

writeConfig :: Config -> IO ()
writeConfig = writeFile jsonFile . show . toJson

newFeedPath :: Config -> Feed -> FilePath
newFeedPath cfg feed = newFeedDir cfg </> feedId feed

oldFeedPath :: Config -> Feed -> FilePath
oldFeedPath cfg feed = feedDir cfg </> feedId feed

enclosurePath :: Config -> Item -> FilePath
enclosurePath cfg item = torrentDir cfg </> itemTitle item ++ ".torrent"
