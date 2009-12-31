module Adam.FeedTorrent.Test (runTest) where

import Adam.FeedTorrent.Data
import Adam.FeedTorrent.Config
import Adam.FeedTorrent.Download ()
import Adam.FeedTorrent.PreludeImports
import Adam.FeedTorrent.Url

type Cmd = Config -> IO ()


ok :: Bool -> IO ()
ok p = putStr $ if p then "." else "X"

-- eq :: Eq a => a -> a -> IO ()
-- eq a b = ok (a == b)

filesInDir :: [FilePath] -> FilePath -> IO ()
filesInDir expected dir = do
  c <- getDirectoryContents' dir
  inter <- return $ ((expected \\ c) `union` (c \\ expected))
  if null inter then ok True
    else do
      putStrLn ""
      putStrLn $ "Expected: " ++ show (sort expected)
      putStrLn $ "Got: " ++ show (sort c)
      putStrLn $ "Intersection: " ++ show inter

runTest :: (Cmd,Cmd) -> Config -> IO ()
runTest (getFeeds,getTorrents) cfg = do
  -- Clear directories
  createDirectoryIfMissing False (torrentDir cfg)
  clearDir (torrentDir cfg)
  createDirectoryIfMissing False (feedDir cfg)
  clearDir (feedDir cfg)
  createDirectoryIfMissing False (newFeedDir cfg)
  clearDir (newFeedDir cfg)

  doesFileExist (wwwDir </> "1.orig.xml") >>= \fe ->
    when fe (mvW "1.xml" "1.2.xml" >> mvW "1.orig.xml" "1.xml")

  -- Download feeds
  getFeeds cfg
  -- Assert feeds end up where they should.
  filesInDir (map (uniqueFilename . fromUrl) . feedUrls $ cfg) (newFeedDir cfg)
  -- Download torrents
  getTorrents cfg
  -- Assert torrents end up where they should.
  flip filesInDir (torrentDir cfg) ["1.a.1.torrent", "1.a.2.torrent", "2.a.1.torrent", "2.a.2.torrent"]

  -- Change 1.xml to a file that adds 1.a.3.torrent and removes 1.a.1.torrent
  mvW "1.xml" "1.orig.xml"
  mvW "1.2.xml" "1.xml"

  getFeeds cfg
  filesInDir (map (uniqueFilename . fromUrl) . feedUrls $ cfg) (newFeedDir cfg)

  -- Assert only the new item's torrent gets downloaded. 2.* unchanged.
  clearDir (torrentDir cfg)
  getTorrents cfg
  flip filesInDir (torrentDir cfg) ["1.a.3.torrent"]

 where
  wwwDir :: FilePath
  wwwDir = "/Users/adam/www/feedtorrent"
  mvW :: FilePath -> FilePath -> IO ()
  mvW a b = mv (wwwDir </> a) (wwwDir </> b)
