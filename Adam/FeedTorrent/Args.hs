-- | Handles parsing of command line arguments.
module Adam.FeedTorrent.Args (parseArgs, AppArgs (..)) where

import System.Console.ParseArgs hiding (parseArgs)

import Adam.FeedTorrent.Imports

-- | Reads the arguments.
parseArgs :: IO AppArgs
parseArgs = do
  res <- parseArgsIO ArgsComplete [argConfig, argCommandFeeds, argCommandTorrents, argCommandTest]
  return AppArgs { arg_configFile = stringArg res "config-file"
                 , arg_commandFeeds = optionalArgToBool res "feeds"
                 , arg_commandTorrents = optionalArgToBool res "torrents"
                 , arg_commandTest = optionalArgToBool res "test"
                 }

stringArg :: Args String -> String -> String
stringArg res index = fromJust (getArg res index)

optionalArgToBool :: Args String -> String -> Bool
optionalArgToBool res index = not . isNothing $ (getArg res index :: Maybe String)

argConfig :: Arg String
argConfig = Arg { argIndex = "config-file"
                , argName = Just "config-file"
                , argAbbr = Just 'c'
                , argData = requiredArg
                , argDesc = "Config file location." }

argCommandFeeds :: Arg String
argCommandFeeds = commandArg "feeds" (Just 'f') "Fetches feed files specified in config."

argCommandTorrents :: Arg String
argCommandTorrents = commandArg "torrents" (Just 't') "Fetches all torrents inside any unprocessed RSS documents."

argCommandTest :: Arg String
argCommandTest = commandArg "test" Nothing "Runs a few tests."

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
data AppArgs = AppArgs { arg_configFile :: String
                       , arg_commandFeeds :: Bool
                       , arg_commandTorrents :: Bool
                       , arg_commandTest :: Bool
                       } deriving (Show, Read)
