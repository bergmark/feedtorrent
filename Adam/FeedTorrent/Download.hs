module Adam.FeedTorrent.Download where

import Network.HTTP
import System.IO
import Data.Maybe
import Network.URI (parseURI)
import Adam.FeedTorrent.PreludeImports
import Adam.FeedTorrent.Config
import Adam.FeedTorrent.Data
import Adam.FeedTorrent.Url

doesTorrentExist :: Config -> Item -> IO Bool
doesTorrentExist cfg item = doesFileExist $ enclosurePath cfg item

getTorrentUnlessExists :: Config -> Item -> IO (Either DownloadError FilePath)
getTorrentUnlessExists cfg item = do
    fe <- doesTorrentExist cfg item
    if fe
      then return (Left TorrentExists)
      else getTorrent cfg item

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

maybeToLeft :: a -> Maybe b -> Either b a
maybeToLeft b Nothing  = Right b
maybeToLeft _ (Just a) = Left a
maybeToRight :: a -> Maybe b -> Either a b
maybeToRight b Nothing  = Left b
maybeToRight _ (Just a) = Right a

downloadToUnique :: Url -> FilePath -> IO (Either DownloadError FilePath)
downloadToUnique url destDir = do
  res <- downloadTo url (destDir </> filename)
  return $ maybeToLeft filename res
  where filename = uniqueFilename . fromUrl $ url

downloadTo :: Url -> FilePath -> IO (Maybe DownloadError)
downloadTo url dest = download url >>= \dres -> case dres of
  Left de -> return (Just de)
  Right contents -> writeFile dest contents >> return Nothing

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

data DownloadError = TorrentExists
                    | DownloadFailed
                    | ConnectionError String
                    | MissingRedirectHeader String
                    | InvalidResponseCode String
                    | InvalidUrlFormat
                    deriving (Show, Eq, Read)
