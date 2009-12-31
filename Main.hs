module Main (main) where

import Prelude hiding (log)

import Adam.FeedTorrent.PreludeImports
import Adam.FeedTorrent.Data
import Adam.FeedTorrent.Url
import Adam.FeedTorrent.Parser
import Adam.FeedTorrent.Download
import Adam.FeedTorrent.Config
import Adam.FeedTorrent.Args
import Adam.FeedTorrent.Test (runTest)

displayError :: String -> IO ()
displayError = putStrLn

log :: String -> IO ()
log = putStrLn

main :: IO ()
main = do
  args <- parseArgs
  ecfg <- getConfig
  case ecfg of
    Left msg -> displayError $ "Error parsing config file: " ++ msg
    Right cfg -> do
      when (arg_commandFeeds args) $ cmd_feeds cfg
      when (arg_commandTorrents args) $ cmd_torrents cfg
      when (arg_commandTest args) $ cmd_test cfg

cmd_feeds :: Config -> IO ()
cmd_feeds cfg = do
  newdirde <- doesDirectoryExist (newFeedDir cfg)
  dirde <- doesDirectoryExist (feedDir cfg)
  if not newdirde then
      displayError $ "new feed dir (" ++ newFeedDir cfg ++ ") does not exist."
    else if not dirde then
      displayError $  "feed dir (" ++ feedDir cfg ++ ") does not exist."
    else do
      forM_ (feedUrls cfg) (\url -> do
                log $ "Downloading " ++ fromUrl url
                dlres <- downloadToUnique url (newFeedDir cfg)
                case dlres of
                  Left err -> print err
                  Right fp -> log $ "Wrote file to " ++ newFeedDir cfg </> fp
                )


feedXmls :: Config -> IO [FilePath]
feedXmls cfg = map (feedDir cfg </>) <$> (getDirectoryContents' . feedDir) cfg
newFeedXmls :: Config -> IO [FilePath]
newFeedXmls cfg = map (newFeedDir cfg </>) <$> (getDirectoryContents' . newFeedDir) cfg

cmd_torrents :: Config -> IO ()
cmd_torrents cfg = do
  feeds <- mapM parseFile =<< feedXmls cfg
  newFeeds <- mapM parseFile =<< newFeedXmls cfg

  mergedFeeds <- return $ mergeAllFeeds feeds newFeeds

  -- Only download torrents that aren't already stored.
  forM_ mergedFeeds (\feed -> do
                  forM_ (items feed)
                    (\item -> do
                        putStrLn $ "Downloading " ++ (enclosureUrl item)
                        file <- getTorrentUnlessExists cfg item
                        case file of
                          Left e -> error . show $ e
                          Right filename -> putStrLn $ "Downloaded " ++ filename)
                  mv (newFeedPath cfg feed) (oldFeedPath cfg feed))

cmd_test :: Config -> IO ()
cmd_test = runTest (cmd_feeds,cmd_torrents)
