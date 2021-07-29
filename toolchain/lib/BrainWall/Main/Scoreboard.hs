module BrainWall.Main.Scoreboard
    ( main
    ) where

import qualified BrainWall.Database           as Db
import           BrainWall.Main.Prosecutor    (Scoreboard (..),
                                               defaultProsecutor,
                                               globalScoreboard)
import qualified Data.HashMap.Strict          as HMS
import           Data.List                    (sortOn)
import           Data.Ord                     (Down (..))
import qualified Data.Time                    as Time
import qualified Options.Applicative.Extended as OA
import           System.Log.FastLogger        as Log
import           Text.Printf                  (printf)

data Options = Options
    { optionsBefore :: Maybe Time.UTCTime
    } deriving (Show)

parseOptions :: OA.Parser Options
parseOptions = Options
    <$> OA.optional (OA.timeOption (OA.long "before"))

prettyScoreboard :: Scoreboard -> [Db.Team] -> String
prettyScoreboard scoreboard teams = unlines
    [printf "%3d | %12d | %s" n s t | (n, (s, t)) <- ordered]
  where
    teamsById = HMS.fromList [(Db.teamId t, t) | t <- teams]
    ordered = zip [1 :: Int ..] . sortOn (Down . fst) $ do
        (tid, score) <- HMS.toList $ scoreboardTeams scoreboard
        let team = teamsById HMS.! tid
        pure (score, Db.teamName team)

main :: IO ()
main = do
    options <- OA.simpleRunParser parseOptions
    dbConfig <- Db.configFromEnv
    Log.withFastLogger (Log.LogStderr Log.defaultBufSize) $ \logger ->
        Db.withDatabase dbConfig $ \db -> do
            features <- Db.getFeatures db
            teams <- Db.getTeams db
            scoreboard <- globalScoreboard
                logger db (defaultProsecutor features) (optionsBefore options)
            putStr $ prettyScoreboard scoreboard teams
