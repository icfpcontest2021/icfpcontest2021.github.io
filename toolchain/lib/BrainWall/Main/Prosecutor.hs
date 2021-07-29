{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module BrainWall.Main.Prosecutor
    ( main

    , Prosecutor (..)
    , Judgement (..)
    , prosecuteByJudgementId
    , prosecuteBySolutionId

    , Scoreboard (..)
    , globalScoreboard
    , refreshScoreboard

    , defaultProsecutor
    ) where

import qualified BrainWall.Database           as Db
import           BrainWall.Graph              (Graph)
import qualified BrainWall.Graph              as Graph
import           BrainWall.Json
import           BrainWall.Polygon
import           BrainWall.Problem
import           Control.Concurrent           (getNumCapabilities, threadDelay)
import qualified Control.Concurrent.Async     as Async
import           Control.DeepSeq              (NFData, rnf)
import           Control.Lens                 (review)
import           Control.Monad                (foldM, guard)
import           Data.Bifunctor               (first)
import qualified Data.ByteString              as B
import qualified Data.HashMap.Strict          as HMS
import qualified Data.HashSet                 as HS
import           Data.Int                     (Int64)
import qualified Data.List                    as L
import qualified Data.List.NonEmpty           as NonEmpty
import           Data.Maybe                   (maybeToList)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import qualified Data.Time                    as Time
import           Data.Traversable             (for)
import qualified Data.UUID                    as Uuid
import qualified Data.Vector                  as V
import qualified Database.PostgreSQL.Simple   as Pg
import           GHC.Generics                 (Generic)
import qualified Options.Applicative.Extended as OA
import           System.Log.FastLogger        as Log

data Prosecutor problem solution = Prosecutor
    { prosecutorParseProblem      :: B.ByteString -> Either String problem
    , prosecutorParseSolution     :: B.ByteString -> Either String solution
    , -- | Which bonuses can be awarded by a problem?
      prosecutorProblemBonuses    :: problem -> [(Int, T.Text)]
    , prosecutorProblemDifficulty :: problem -> Int
    , -- | Which bonuses were used by a problem?  We need to check if they
      -- were available.
      prosecutorSolutionBonuses   :: solution -> [(Int, T.Text)]
    , prosecutorJudge             :: problem -> solution -> Judgement
    }

data Judgement
    = Invalid String                 -- ^ Invalid solution
    | Valid Int64 [(Int, T.Text)]  -- ^ Score, obtained bonuses

data Options = Options
    { optionsJobs       :: Maybe Int
    , optionsSolutionId :: Maybe Db.SolutionId
    }

parseOptions :: OA.Parser Options
parseOptions = Options
    <$> OA.optional (OA.option OA.auto (
            OA.short 'j' <> OA.help "number of jobs"))
    <*> OA.optional (fmap Db.SolutionId $ OA.argument
            (OA.maybeReader Uuid.fromString) $
        OA.metavar "SOLUTION_ID")

type MetagameGraph = HMS.HashMap Db.ProblemId MetagameInfo

-- | A smaller version of 'Db.Solution', we don't want to keep everything in
-- Solution in-memory when looking at the puzzle graph.
data MetagameInfo = MetagameInfo
    { miJudgementId    :: !(Maybe Db.JudgementId)
    , miValid          :: !Bool
    , miClaimedBonuses :: [(Db.ProblemId, T.Text)]
    , miAwardedBonuses :: [(Db.ProblemId, T.Text, Bool)]
    } deriving (Generic)

instance NFData MetagameInfo

fastGetMetagamaGraph
    :: Db.Database -> Prosecutor p s -> Db.TeamId -> IO MetagameGraph
fastGetMetagamaGraph db Prosecutor {..} tid = do
    rows <- Db.getSolutionsByTeam db tid
    pure . HMS.fromList $ do
        (pid, Db.Solution {..}) <- rows
        let miClaimedBonuses =
                case prosecutorParseSolution (T.encodeUtf8 solutionBody) of
                    Left _ -> []
                    Right sol -> map (first Db.ProblemId) $
                        prosecutorSolutionBonuses sol
            miAwardedBonuses = solutionBonuses
            miJudgementId = solutionJudgement
            miValid = case solutionStatus of
                Just Db.JudgementValid -> True
                _                      -> False
            info = MetagameInfo {..}
        rnf info `seq` pure (pid, MetagameInfo {..})

-- | Compute the dependency graph of used dependencies.  This determines how
-- invalidity propagates accross the graph.
makeMetagameUsedDependees :: MetagameGraph -> Graph Db.ProblemId
makeMetagameUsedDependees graph = Graph.fromEdges $ do
    (pid, mgi) <- HMS.toList graph
    (dependency, _) <- miClaimedBonuses mgi
    pure (dependency, pid)

playMetagame
    :: Db.Database -> Prosecutor p s -> Db.TeamId -> Db.ProblemId -> IO ()
playMetagame db prosecutor tid _pid0 = do
    fullGraph <- fastGetMetagamaGraph db prosecutor tid
    let usedDependeesGraph = makeMetagameUsedDependees fullGraph

    -- Predicate to see if a bonus is available.
    let bonusAvailable for' (from, bonus) = case HMS.lookup from fullGraph of
            Nothing  -> False
            Just mgi -> (for', bonus, True) `elem` miAwardedBonuses mgi

    -- Determine which nodes in the metagameGraph are invalid (on their own,
    -- not transitive).
    let baseInvalid = HMS.keysSet $ HMS.filterWithKey
            (\pid mgi -> not $
                miValid mgi && all (bonusAvailable pid) (miClaimedBonuses mgi))
            fullGraph

    -- Determine all invalid nodes.
    let fullInvalid = Graph.reachable baseInvalid usedDependeesGraph

    -- Update each record in the metagame graph.
    Db.updateJudgementsBonusOk db $ do
        (pid, mgi) <- HMS.toList fullGraph
        jid <- maybeToList $ miJudgementId mgi
        pure (jid, not $ pid `HS.member` fullInvalid)

prosecuteByJudgementId
    :: Log.FastLogger
    -> Db.Database -> Prosecutor p s -> Db.JudgementId -> Db.Solution -> IO ()
prosecuteByJudgementId logger conn
        prosecutor@Prosecutor {..} jid Db.Solution {..} = do
    logger $ "Solving " <> Log.toLogStr (show solutionId) <> "\n"
    Db.Problem {..} <- Db.getEnabledProblem conn solutionProblem
    problem <- either (fail . mappend "Could not parse problem: ") pure .
        prosecutorParseProblem $ T.encodeUtf8 problemBody
    let awardedBonuses l = do
            (pid, bonus) <- prosecutorProblemBonuses problem
            pure (Db.ProblemId (fromIntegral pid), bonus, (pid, bonus) `elem` l)

    case prosecutorParseSolution (T.encodeUtf8 solutionBody) of
        Left err -> do
            Db.completeJudgement conn jid
                Db.JudgementError
                Nothing
                (Just err)
                (awardedBonuses [])
            logger $ "Error parsing problem: " <> Log.toLogStr err <> "\n"
        Right solution -> case prosecutorJudge problem solution of
            Invalid err -> do
                Db.completeJudgement conn jid
                    Db.JudgementInvalid
                    Nothing
                    (Just err)
                    (awardedBonuses [])
                logger $ "Invalid solution: " <> Log.toLogStr err <> "\n"
            Valid dislikes bonuses -> do
                Db.completeJudgement conn jid
                    Db.JudgementValid
                    (Just dislikes)
                    Nothing
                    (awardedBonuses bonuses)
                logger $ "Valid solution: " <> Log.toLogStr dislikes <> "\n"

    Pg.withTransaction (Db.dbConnection conn) $
        playMetagame conn prosecutor solutionTeam problemId

prosecuteBySolutionId
    :: Log.FastLogger -> Db.Database -> Prosecutor p s -> Db.SolutionId -> IO ()
prosecuteBySolutionId logger conn prosecutor solutionId = do
    (jid, solution) <- Db.startJudgement conn solutionId
    prosecuteByJudgementId logger conn prosecutor jid solution

defaultProsecutor :: Db.Features -> Prosecutor Problem Solution
defaultProsecutor features = Prosecutor
    { prosecutorParseProblem      = decodeWith (decodeProblem features)
    , prosecutorParseSolution     = decodeWith (decodeSolution features)
    , prosecutorProblemBonuses    = \Problem {..} -> do
        Bonus {..} <- V.toList problemBonuses
        pure (bonusProblem, review bonusTypeFromText bonusBonus)
    , prosecutorProblemDifficulty = \Problem {..} ->
        let Figure {..} = problemFigure in
        V.length figureVertices * V.length figureEdges * V.length (polygonVertices $ unHole problemHole)
    , prosecutorSolutionBonuses   = \solution ->
        [ (claimedBonusProblem, review bonusTypeFromText claimedBonusBonus)
        | ClaimedBonus {..} <- V.toList $ solutionBonuses solution
        ]
    , prosecutorJudge             = \problem solution ->
        case judgeSolution problem solution of
            Left errs      -> Invalid $ NonEmpty.head errs
            Right dislikes -> Valid (fromIntegral dislikes) $ do
                (Bonus {..}, ok) <- awardBonusesForSolution problem solution
                guard ok
                pure (bonusProblem, review bonusTypeFromText bonusBonus)
    }

data Scoreboard = Scoreboard
    { scoreboardTeams       :: HMS.HashMap Db.TeamId Int64
    , scoreboardMinDislikes :: HMS.HashMap Db.ProblemId Int64
    } deriving (Show)

instance Semigroup Scoreboard where
    x <> y = Scoreboard
        (HMS.unionWith (+) (scoreboardTeams x) (scoreboardTeams y))
        (HMS.unionWith min (scoreboardMinDislikes x) (scoreboardMinDislikes y))

instance Monoid Scoreboard where
    mempty = Scoreboard HMS.empty HMS.empty

problemScoreboard
    :: Log.FastLogger -> Db.Database -> Prosecutor p s -> Db.ProblemId
    -> Maybe Time.UTCTime
    -> IO Scoreboard
problemScoreboard logger conn Prosecutor {..} pid mbBefore = do
    Db.Problem {..} <- Db.getEnabledProblem conn pid
    difficulty <- case prosecutorParseProblem (T.encodeUtf8 problemBody) of
        Right p   -> pure $ prosecutorProblemDifficulty p
        Left  err -> do
            logger $
                "warning: could not parse problem " <> Log.toLogStr problemId <>
                ": " <> Log.toLogStr err <> "\n"
            pure 0

    before <- maybe Time.getCurrentTime pure mbBefore
    teamsSolutionIds <- Db.getSolutionsByProblemBefore conn pid before
    results <- for teamsSolutionIds $ \(tid, sid) -> do
        sol <- traverse (Db.getSolutionById conn tid) sid
        pure (tid, Db.solutionResult <$> sol)
    pure $ case [s | (_, Just (Db.SolutionValid s)) <- results] of
        [] -> Scoreboard
            (HMS.fromList . zip (map fst teamsSolutionIds) $ repeat 0)
            HMS.empty
        dislikes ->
            let tbest = L.minimum dislikes
                byTeam = do
                    (tid, result) <- results
                    case result of
                        Just (Db.SolutionValid tteam) ->
                            pure (tid, score difficulty tbest tteam)
                        _ -> pure (tid, 0) in
            Scoreboard
                (HMS.fromList byTeam)
                (HMS.singleton pid tbest)
  where
    score difficulty tbest tteam = ceiling $ (1000 :: Double) *
        (max 1 $ logBase 2 $ fromIntegral difficulty / 6) *
        sqrt (fromIntegral (tbest + 1) / fromIntegral (tteam + 1))

globalScoreboard
    :: Log.FastLogger -> Db.Database -> Prosecutor s p -> Maybe Time.UTCTime
    -> IO Scoreboard
globalScoreboard logger conn prosecutor mbBefore = do
    problems <- Db.getEnabledProblemIds conn
    foldM
        (\acc pid -> do
            sb <- problemScoreboard logger conn prosecutor pid mbBefore
            let acc' = acc <> sb
            acc' `seq` pure acc')
        mempty
        problems

refreshScoreboard :: Log.FastLogger -> Db.Database -> Prosecutor s p -> IO ()
refreshScoreboard logger conn prosecutor = do
    Scoreboard byTeam bestDislikes <- globalScoreboard
        logger conn prosecutor Nothing
    Db.setScoreboard conn (HMS.toList byTeam) (HMS.toList bestDislikes)

daemon :: Options -> Log.FastLogger -> Db.Config -> IO ()
daemon Options {..} logger dbConfig = do
    numJudgeWorkers <- case optionsJobs of
        Just n  -> pure n
        Nothing -> max 1 . pred <$> getNumCapabilities
    logger $ "Using " <> Log.toLogStr numJudgeWorkers <> " judge workers...\n"
    Async.concurrently_
        scoreBoardWorker
        (Async.replicateConcurrently_ numJudgeWorkers judgeWorker)
  where
    judgeWorker = withDelay 300 $ Db.withDatabase dbConfig $ \conn -> do
        prosecutor <- defaultProsecutor <$> Db.getFeatures conn
        mbSolution <- Db.popAndStartJudgement conn
        case mbSolution of
            Nothing              -> pure ()
            Just (jid, solution) -> prosecuteByJudgementId
                logger conn prosecutor jid solution

    scoreBoardWorker = withDelay 60000 $ Db.withDatabase dbConfig $ \conn -> do
        prosecutor <- defaultProsecutor <$> Db.getFeatures conn
        refreshScoreboard logger conn prosecutor

    withDelay ms mx = do
        _ <- mx
        threadDelay $ ms * 1000
        withDelay ms mx

main :: IO ()
main = do
    options <- OA.simpleRunParser parseOptions
    dbConfig <- Db.configFromEnv
    Log.withFastLogger (Log.LogStderr Log.defaultBufSize) $ \logger ->
        case optionsSolutionId options of
            Just sid ->
                Db.withDatabase dbConfig $ \conn -> do
                prosecutor <- defaultProsecutor <$> Db.getFeatures conn
                prosecuteBySolutionId logger conn prosecutor sid
            Nothing -> daemon options logger dbConfig
