{-# LANGUAGE ScopedTypeVariables #-}
module Options.Applicative.Extended
    ( module Options.Applicative
    , simpleRunParser

    , timeOption
    ) where

import qualified Data.Time                as Time
import           Data.Time.Format.ISO8601 (iso8601ParseM)
import           Options.Applicative

timeOption :: Mod OptionFields Time.UTCTime -> Parser Time.UTCTime
timeOption opts = option (maybeReader iso8601ParseM)
    (opts <> metavar "YYYY-MM-DDThh:mm:ssZ")

simpleRunParser :: forall a. Parser a -> IO a
simpleRunParser p =
    customExecParser parseOptionsPrefs parseOptionsInfo
  where
    parseOptionsInfo :: ParserInfo a
    parseOptionsInfo = info (helper <*> p) fullDesc

    parseOptionsPrefs :: ParserPrefs
    parseOptionsPrefs = prefs showHelpOnError where
