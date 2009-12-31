module Adam.FeedTorrent.Parser (parseFile, parse) where

import Text.XML.HaXml hiding (path)
import Text.XML.HaXml.Parse ()

import Adam.FeedTorrent.Data
import Adam.FeedTorrent.Config ()
import Adam.FeedTorrent.PreludeImports

parseFile :: FilePath -> IO Feed
parseFile fp = (\contents -> parse contents (takeBaseName fp) fp) <$> readFile fp

{- | Parse the data from a given string, with the given name to use
in error messages. -}
parse :: String -> String -> FilePath -> Feed
parse content name fp =
    Feed { channelTitle = getText doc (tag "title")
         , items = getEnclosures doc
         , feedId = name
         , location = fp }
    where parseResult = xmlParse name (stripUnicodeBOM content)
          doc = getContent parseResult

          getContent :: Document i -> Content i
          getContent (Document _ _ e _) = CElem e undefined

          {- | Some Unicode documents begin with a binary sequence;
             strip it off before processing. -}
          stripUnicodeBOM :: String -> String
          stripUnicodeBOM ('\xef':'\xbb':'\xbf':y) = y
          stripUnicodeBOM y = y

{- | Pull out the channel part of the document. -}
getChannel :: CFilter i
getChannel = tag "rss" /> tag "channel"

getText :: Content i -> CFilter i -> String
getText item path = contentToStringDefault "N/A" (keep /> path /> txt $ item)

getEnclosures :: Content i -> [Item]
getEnclosures doc =
    concatMap procItem $ getItems doc
    where procItem :: Content i -> [Item]
          procItem item = concatMap (procEnclosure title guid) enclosure
              where title = getText item (tag "title")
                    guid  = getText item (tag "guid")
                    enclosure = (keep /> tag "enclosure") item

          getItems :: CFilter i
          getItems = getChannel /> tag "item"

          procEnclosure :: String -> String -> Content i -> [Item]
          procEnclosure title guid enclosure =
              map makeItem (showattr "url" enclosure)
              where makeItem :: Content i -> Item
                    makeItem y = Item { itemTitle = title
                                      , itemGuid = guid
                                      , enclosureUrl = contentToString [y] }

{- | Convert [Content] to a printable String, with a default if the
passed-in [Content] is [], signifying a lack of a match. -}
contentToStringDefault :: String -> [Content i] -> String
contentToStringDefault msg [] = msg
contentToStringDefault _ y = contentToString y

{- | Convert [Content] to a printable string, taking care to unescape it.

An implementation without unescaping would simply be:

> contentToString = concatMap (show . content)

Because HaXml's unescaping only works on Elements, we must make sure that
whatever Content we have is wrapped in an Element, then use txt to
pull the insides back out. -}
contentToString :: [Content i] -> String
contentToString =
    concatMap procContent
    where procContent y =
              verbatim $ keep /> txt $ CElem (unesc (fakeElem y)) undefined
          fakeElem :: Content i -> Element i
          fakeElem y = Elem "fake" [] [y]
          unesc :: Element i -> Element i
          unesc = xmlUnEscape stdXmlEscaper
