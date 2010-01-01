-- | Downloading of files through HTTP.
--
--   Code modified from http://book.realworldhaskell.org/read/extended-example-web-client-programming.html
--   available under license terms specified at http://creativecommons.org/licenses/by-nc/3.0/
--

module Adam.FeedTorrent.Download where

import Network.HTTP
import System.IO
import Data.Maybe
import Network.URI (parseURI)
import Adam.FeedTorrent.Imports
import Adam.FeedTorrent.Config
import Adam.FeedTorrent.Data

-- | Checks whether the torrent file for an Item is stored on disk.
doesTorrentExist :: Config -> Item -> IO Bool
doesTorrentExist cfg item = doesFileExist $ enclosurePath cfg item

-- | If a torrent has not been saved do disk, it is fetched through HTTP.
getTorrentUnlessExists :: Config -> Item -> IO (Either DownloadError FilePath)
getTorrentUnlessExists cfg item = do
    fe <- doesTorrentExist cfg item
    if fe
      then return (Left TorrentExists)
      else getTorrent cfg item

-- | Fetches a torrent from the web and stores it on disk.
getTorrent :: Config -> Item -> IO (Either DownloadError FilePath)
getTorrent cfg item =
    do resp <- download . Url . enclosureUrl $ item
       case resp of
         Left x -> return (Left x)
         Right doc ->
             do file <- openBinaryFile filename WriteMode
                hPutStr file doc
                hClose file
                return (Right filename)
    where filename = enclosurePath cfg item

-- | Creates a unique file name from the Url given and stores the file
--   under the given dir with that name.
downloadToUnique :: Url -> FilePath -> IO (Either DownloadError FilePath)
downloadToUnique url destDir = do
  res <- downloadTo url (destDir </> filename)
  return $ maybeToLeft filename res
  where filename = uniqueFilename $ url

-- | Downloads a file and stores the data at the specified path.
downloadTo :: Url -> FilePath -> IO (Maybe DownloadError)
downloadTo url dest = download url >>= \dres -> case dres of
  Left de -> return (Just de)
  Right contents -> writeFile dest contents >> return Nothing

-- | Downloads a file and return the contents.
download :: Url -> IO (Either DownloadError String)
download url = if isNothing uri then return (Left InvalidUrlFormat) else do
  resp <- simpleHTTP request
  case resp of
    Left x -> return . Left . ConnectionError . show $ x
    Right r ->
      case rspCode r of
        (2,_,_) -> return . Right .rspBody $ r
        (3,_,_) -> -- A HTTP redirect
          case findHeader HdrLocation r of
            Nothing -> return . Left . MissingRedirectHeader . show $ r
            Just url' -> download . Url $ url'
        _ -> return . Left . InvalidResponseCode . show $ r
  where
    request = Request { rqURI = fromJust uri
                      , rqMethod = GET
                      , rqHeaders = []
                      , rqBody = "" }
    uri = parseURI . fromUrl $ url

-- | Holds possible failures for downloads.
data DownloadError = TorrentExists
                    | DownloadFailed
                    | ConnectionError String
                    | MissingRedirectHeader String
                    | InvalidResponseCode String
                    | InvalidUrlFormat
                    deriving (Show, Eq, Read)
