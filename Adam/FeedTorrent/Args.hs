-- | Handles parsing of command line arguments.
module Adam.FeedTorrent.Args (parseArgs, AppArgs (..)) where

import System.Console.ParseArgs hiding (parseArgs)

import Adam.FeedTorrent.Imports

-- | Reads the arguments.
parseArgs :: IO AppArgs
parseArgs = do
  res <- parseArgsIO ArgsComplete argDecl
  return AppArgs { argConfigFile = stringArg res "config-file"
                 , argCommandFeeds = optionalArgToBool res "feeds"
                 , argCommandTorrents = optionalArgToBool res "torrents"
                 , argCommandTest = optionalArgToBool res "test"
                 }

stringArg :: Args String -> String -> String
stringArg res = fromJust . getArg res

optionalArgToBool :: Args String -> String -> Bool
optionalArgToBool res index = not . isNothing $ (getArg res index :: Maybe String)

argDecl :: [Arg String]
argDecl = [
  Arg { argIndex = "config-file"
      , argName = Just "config-file"
      , argAbbr = Just 'c'
      , argData = requiredArg
      , argDesc = "Config file location." }
  , commandArg "feeds" (Just 'f') "Fetches feed files specified in config."
  , commandArg "torrents" (Just 't') "Fetches all torrents inside any unprocessed RSS documents."
  , commandArg "test" Nothing "Runs a few tests."
  ]

commandArg :: String -> Maybe Char -> String -> Arg String
commandArg argName' abbr desc = Arg {
  argIndex = argName'
  , argName = Just argName'
  , argAbbr = abbr
  , argData = Nothing
  , argDesc = desc
  }

requiredArg :: Maybe DataArg
requiredArg = argDataRequired "string" (ArgtypeString . fromJust)

-- | Contains the information extracted from the args.
data AppArgs = AppArgs { argConfigFile :: String
                       , argCommandFeeds :: Bool
                       , argCommandTorrents :: Bool
                       , argCommandTest :: Bool
                       } deriving (Show, Read)
