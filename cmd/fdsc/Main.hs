module Main (main) where

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

  -- Make a request to past orders API.
  -- Paginate through orders until a certain time span is reached.

  orders <- getOrdersSince (SessionID auth) undefined

  -- Two axes: coupon Y/N and subscription Y/N.
  -- Compute hypothetical price in each quadrant, and actual price.
  -- Use delivery and service fee from historical item.

  -- Summarize findings and make a recommendation.
  putStrLn $ intercalate "\n" $ render <$> orders
