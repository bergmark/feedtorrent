{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}
module Adam.FeedTorrent.Url where

import Data.Generics.SYB.WithClass.Derive

newtype Url = Url String
            deriving (Eq,Show,Read)
$(derive[''Url])

fromUrl :: Url -> String
fromUrl (Url s) = s
