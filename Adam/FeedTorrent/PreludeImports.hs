-- | Convinience imports of standard functions used throughout the
--   app.
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
  , maybeToLeft
  , maybeToRight
  , md5'
  , toRight
  , lif
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

-- | The contents of a folder execept "." and "..".
getDirectoryContents' :: FilePath -> IO [FilePath]
getDirectoryContents' dir = (\\ [".",".."]) <$> getDirectoryContents dir

-- | md5 for String.
md5' :: String -> String
md5' = show . md5 . pack

-- | Like forM, except no monad.
for :: [a] -> (a -> b) -> [b]
for = flip map

-- | Applies an operation to the right of an Either.
toRight :: (b -> b') -> Either a b -> Either a b'
toRight _ (Left a) = Left a
toRight f (Right b) = Right (f b)

-- | If statement with all arguments in a monad.
lif :: Monad m => m Bool -> m a -> m a -> m a
lif p c a = p >>= \res -> if res then c else a

-- | Convert a maybe to a Left, or into a Right if Nothing.
maybeToLeft :: a -> Maybe b -> Either b a
maybeToLeft b Nothing  = Right b
maybeToLeft _ (Just a) = Left a

-- | Convert a maybe to a Right, or into a Left if Nothing.
maybeToRight :: a -> Maybe b -> Either a b
maybeToRight b Nothing  = Left b
maybeToRight _ (Just a) = Right a
