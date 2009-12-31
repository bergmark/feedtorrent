module Adam.FeedTorrent.PreludeImports
  -- Built-ins and hackages.
  ( module Control.Applicative
  , module Control.Arrow
  , module Control.Monad
  , module Data.List
  , module Data.Maybe
  , module System
  , module System.Directory
  , module System.FilePath

  -- My own cabals.
  , module Adam.Shellscript

  -- Local declarations.
  , getDirectoryContents'
  , for
  , md5'
  , toRight
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.ByteString.Lazy.Char8 (pack)
import Data.List
import Data.Maybe
import Data.Digest.Pure.MD5 (md5)
import System
import System.Directory
import System.FilePath

import Adam.Shellscript

getDirectoryContents' :: FilePath -> IO [FilePath]
getDirectoryContents' dir = (\\ [".",".."]) <$> getDirectoryContents dir

md5' :: String -> String
md5' = show . md5 . pack

for :: [a] -> (a -> b) -> [b]
for = flip map

toRight :: (b -> b') -> Either a b -> Either a b'
toRight _ (Left a) = Left a
toRight f (Right b) = Right (f b)
