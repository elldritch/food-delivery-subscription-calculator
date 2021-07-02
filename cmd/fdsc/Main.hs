module Main (main) where

import Data.Time (addUTCTime, getCurrentTime)
import FDSC.UberEats (SessionID (..), getOrdersSince, render)
import Options.Applicative (Parser, ParserInfo, execParser)
import Options.Applicative.Builder
  ( fullDesc,
    header,
    help,
    info,
    long,
    progDesc,
    strOption,
  )
import Options.Applicative.Extra (helper)
import Relude

data CLIOptions = CLIOptions
  { service :: Text,
    auth :: Text
  }

optionsParser :: Parser CLIOptions
optionsParser =
  CLIOptions
    <$> strOption (long "service" <> help "Food delivery service")
    <*> strOption (long "auth" <> help "Authentication token")

argParser :: ParserInfo CLIOptions
argParser =
  info (optionsParser <**> helper) (fullDesc <> progDesc p <> header h)
  where
    p =
      "Calculate whether a food delivery subscription would have saved you \
      \money."
    h = "fdsc - food delivery subscription calculator"

main :: IO ()
main = do
  CLIOptions {..} <- execParser argParser
  unless (service == "uber-eats") $
    die "Food delivery service name not recognized."

  now <- getCurrentTime
  orders <-
    getOrdersSince
      (SessionID auth)
      $ addUTCTime (fromInteger $ -1 * 60 * 60 * 24 * 30 * 6) now

  putStrLn $ intercalate "\n" $ render <$> orders
