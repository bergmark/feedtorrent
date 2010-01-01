-- | Tests various aspects of the program. Homegrown xUnit like
--   testing, may switch to HUnit later.
module Adam.FeedTorrent.Test (runTest) where

import Adam.FeedTorrent.Data
import Adam.FeedTorrent.Config
import Adam.FeedTorrent.Download ()
import Adam.FeedTorrent.Imports

-- | Performs the actual tests using a custom config.
runTest :: (Cmd,Cmd) -> Config -> IO ()
runTest (getFeeds,getTorrents) _ = do
  cfg <- getSampleConfig

  -- Clear directories
  createDirectoryIfMissing False (torrentDir cfg)
  clearDir (torrentDir cfg)
  createDirectoryIfMissing False (feedDir cfg)
  clearDir (feedDir cfg)
  createDirectoryIfMissing False (newFeedDir cfg)
  clearDir (newFeedDir cfg)

  doesFileExist (wwwDir </> "1.orig.xml") >>=
    flip when (mvW "1.xml" "1.2.xml" >> mvW "1.orig.xml" "1.xml")

  -- Download feeds
  getFeeds cfg
  -- Assert feeds end up where they should.
  filesInDir (newFeedDir cfg) (map uniqueFilename . feedUrls $ cfg)
  -- Download torrents
  getTorrents cfg
  -- Assert torrents end up where they should.
  filesInDir (torrentDir cfg) ["1.a.1.torrent", "1.a.2.torrent", "2.a.1.torrent", "2.a.2.torrent"]

  -- Change 1.xml to a file that adds 1.a.3.torrent and removes 1.a.1.torrent
  mvW "1.xml" "1.orig.xml"
  mvW "1.2.xml" "1.xml"

  getFeeds cfg
  filesInDir (newFeedDir cfg) (map uniqueFilename . feedUrls $ cfg)

  -- Assert only the new item's torrent gets downloaded. 2.* unchanged.
  clearDir (torrentDir cfg)
  getTorrents cfg
  filesInDir (torrentDir cfg) ["1.a.3.torrent"]

 where
  wwwDir :: FilePath
  wwwDir = "/Users/adam/www/feedtorrent"
  mvW :: FilePath -> FilePath -> IO ()
  mvW a = mv (wwwDir </> a) . (wwwDir </>)

-- | Custom config for testing. Right now it uses the same directories
--   as the default config and may thus overwrite production files.
getSampleConfig :: IO Config
getSampleConfig =
  return defaultConfig { feedUrls = map Url ["http://ecmascript.se/feedtorrent/1.xml"
                                            ,"http://ecmascript.se/feedtorrent/2.xml"] }

-- | Asserts that the given files exist in the given directory, and
--   that no other files are there.
filesInDir :: FilePath -> [FilePath] -> IO ()
filesInDir dir expected = do
  c <- getDirectoryContents' dir
  let inter = ((expected \\ c) `union` (c \\ expected))
  if null inter then ok True
    else do
      putStrLn ""
      putStrLn $ "Expected: " ++ show (sort expected)
      putStrLn $ "Got: " ++ show (sort c)
      putStrLn $ "Intersection: " ++ show inter

-- eq :: Eq a => a -> a -> IO ()
-- eq a b = ok (a == b)

-- | Asserts that a value is True.
ok :: Bool -> IO ()
ok p = putStr $ if p then "." else "X"

-- | Shorthand for commands.
type Cmd = Config -> IO ()
