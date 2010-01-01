module Main (main) where

import Prelude hiding (log)

import Adam.FeedTorrent.Imports
import Adam.FeedTorrent.Data
import Adam.FeedTorrent.Parser
import Adam.FeedTorrent.Download
import Adam.FeedTorrent.Config
import Adam.FeedTorrent.Args
import Adam.FeedTorrent.Test (runTest)

-- | Prints error messages. For now it is only putStrLn.
displayError :: String -> IO ()
displayError = putStrLn

-- | Prints log messages. For now it is only putStrLn.
log :: String -> IO ()
log = putStrLn

-- | Parses cmd line arguments and dispatches to application commands.
main :: IO ()
main = do
  args <- parseArgs
  ecfg <- getConfig
  case ecfg of
    Left msg -> displayError $ "Error parsing config file: " ++ msg
    Right cfg -> do
      when (argCommandFeeds args) $ cmdFeeds cfg
      when (argCommandTorrents args) $ cmdTorrents cfg
      when (argCommandTest args) $ cmdTest cfg

-- | Fetches all feeds specified in the config.
cmdFeeds :: Config -> IO ()
cmdFeeds cfg = do
  newdirde <- doesDirectoryExist (newFeedDir cfg)
  dirde <- doesDirectoryExist (feedDir cfg)
  if not newdirde then
      displayError $ "new feed dir (" ++ newFeedDir cfg ++ ") does not exist."
    else if not dirde then
      displayError $  "feed dir (" ++ feedDir cfg ++ ") does not exist."
    else
      forM_ (feedUrls cfg) (\url -> do
                log $ "Downloading " ++ fromUrl url
                dlres <- downloadToUnique url (newFeedDir cfg)
                case dlres of
                  Left err -> print err
                  Right fp -> log $ "Wrote file to " ++ newFeedDir cfg </> fp
                )


-- | All processed feeds.
feedXmls :: Config -> IO [FilePath]
feedXmls cfg = map (feedDir cfg </>) <$> (getDirectoryContents' . feedDir) cfg

-- | All unprocessed feeds.
newFeedXmls :: Config -> IO [FilePath]
newFeedXmls cfg = map (newFeedDir cfg </>) <$> (getDirectoryContents' . newFeedDir) cfg

-- | Compares unprocessed and processed feeds and downloads new
--   torrents.
cmdTorrents :: Config -> IO ()
cmdTorrents cfg = do
  feeds <- mapM parseFile =<< feedXmls cfg
  newFeeds <- mapM parseFile =<< newFeedXmls cfg

  let mergedFeeds = mergeAllFeeds feeds newFeeds

  -- Only download torrents that aren't already stored.
  forM_ mergedFeeds (\feed -> do
                  forM_ (items feed)
                    (\item -> do
                        putStrLn $ "Downloading " ++ enclosureUrl item
                        file <- getTorrentUnlessExists cfg item
                        case file of
                          Left e -> error . show $ e
                          Right filename -> putStrLn $ "Downloaded " ++ filename)
                  mv (newFeedPath cfg feed) (oldFeedPath cfg feed))

-- | Runs some tests.
cmdTest :: Config -> IO ()
cmdTest = runTest (cmdFeeds,cmdTorrents)
