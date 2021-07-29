{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module BrainWall.Main.Prosecutor.Tests
    ( tests
    ) where

import           BrainWall.Database
import           BrainWall.Database.Tests   hiding (tests)
import           BrainWall.Json             (encodeText)
import           BrainWall.Main.Prosecutor
import qualified Data.Aeson                 as Aeson
import           Data.Foldable              (for_)
import           Data.Int                   (Int64)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TL
import qualified Data.Time                  as Time
import           Data.Traversable           (for)
import qualified Database.PostgreSQL.Simple as Pg
import           GHC.Generics               (Generic)
import           System.Log.FastLogger      as Log
import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.HUnit           (Assertion, testCase, (@?=))

type MockProblem = [(Int, T.Text)]

data MockSolution = MockSolution
    { claims :: [(Int, T.Text)]
    , grab   :: [Int]
    } deriving (Generic)

instance Aeson.ToJSON MockSolution
instance Aeson.FromJSON MockSolution

withLogger :: (Log.FastLogger -> IO a) -> IO a
withLogger = Log.withFastLogger (Log.LogStderr Log.defaultBufSize)

-- | Utility to submit a solution and wait for it to be judged.
submitMockSolution
    :: Log.FastLogger -> Database -> TeamId -> ProblemId -> MockSolution
    -> IO Solution
submitMockSolution logger db tid pid sol = do
    Right sid1 <- submitSolution db pid tid .
        TL.toStrict . TL.decodeUtf8 $ Aeson.encode sol
    (jid1, sol1) <- startJudgement db sid1
    prosecuteByJudgementId logger db mockProsecutor jid1 sol1
    getSolutionById db tid sid1

mockProsecutor :: Prosecutor MockProblem MockSolution
mockProsecutor = Prosecutor
    { prosecutorParseProblem      = Aeson.eitherDecodeStrict
    , prosecutorParseSolution     = Aeson.eitherDecodeStrict
    , prosecutorProblemBonuses    = id
    , prosecutorProblemDifficulty = const 1
    , prosecutorSolutionBonuses   = claims
    , prosecutorJudge             = \problem solution ->
        if all (`elem` map fst problem) (grab solution)
            then Valid
                    (fromIntegral . length $ grab solution)
                    (filter ((`elem` grab solution) . fst) problem)
            else Invalid "not in there"
    }

-- | The silliest problem where a participant just names their score,
-- which must be part of the given scores.
data SillyProblem = SillyProblem
    { sillyOptions    :: [Int64]
    , sillyDifficulty :: Int
    } deriving (Generic)

instance Aeson.ToJSON SillyProblem
instance Aeson.FromJSON SillyProblem

type SillySolution = Int64

sillyProsecutor :: Prosecutor SillyProblem SillySolution
sillyProsecutor = Prosecutor
    { prosecutorParseProblem      = Aeson.eitherDecodeStrict
    , prosecutorParseSolution     = Aeson.eitherDecodeStrict
    , prosecutorProblemBonuses    = const []
    , prosecutorProblemDifficulty = sillyDifficulty
    , prosecutorSolutionBonuses   = const []
    , prosecutorJudge             = \(SillyProblem options _) score ->
        if score `elem` options
            then Valid score []
            else Invalid "invalid score"
    }

assertScoreboard
    :: Log.FastLogger -> Database -> Prosecutor p s
    -> [(TeamId, T.Text, Int64)] -> Assertion
assertScoreboard logger conn prosecutor expected = do
    refreshScoreboard logger conn prosecutor
    actual <- getPublicScoreboard conn
    actual @?= expected

-- | This is just extra data that we don't want to hardcode in tests.
removeContestMinDislikes :: ProblemSummary -> ProblemSummary
removeContestMinDislikes ps = ps {problemSummaryContestMinDislikes = Nothing}

tests :: TestTree
tests = testGroup "BrainWall.Main.Prosecutor"
    [ testCase "submit solution" $ withLogger $ \logger -> withTmpDatabase $ \db -> do
        team <- createExampleTeam1 db
        insertProblems db
            [ Problem (ProblemId 0) True $ encodeText ([(1, "green")] :: MockProblem)
            , Problem (ProblemId 1) True $ encodeText ([(0, "yellow")] :: MockProblem)
            ]

        Right sid1 <- submitSolution db (ProblemId 0) (teamId team) "wat"
        (jid1, sol1) <- startJudgement db sid1
        prosecuteByJudgementId logger db mockProsecutor jid1 sol1
        judged1 <- getSolutionById db (teamId team) sid1
        solutionStatus judged1 @?= Just JudgementError
        solutionBonuses judged1 @?= [(ProblemId 1, "green", False)]

        sol2 <- submitMockSolution logger db (teamId team) (ProblemId 0) $
            MockSolution [] [3]
        solutionStatus sol2 @?= Just JudgementInvalid
        solutionBonuses sol2  @?= [(ProblemId 1, "green", False)]

        sol3 <- submitMockSolution logger db (teamId team) (ProblemId 0) $
            MockSolution [] [1]
        solutionStatus sol3 @?= Just JudgementValid
        solutionBonuses sol3 @?= [(ProblemId 1, "green", True)]

    , testCase "simple metagame" $ withLogger $ \logger -> withTmpDatabase $ \db -> do
        team <- createExampleTeam1 db
        insertProblems db
            [ Problem (ProblemId 0) True $ encodeText ([(1, "green")] :: MockProblem)
            , Problem (ProblemId 1) True $ encodeText ([(0, "yellow")] :: MockProblem)
            ]

        -- Valid submission but we use a bonus from problem 1 that isn't
        -- available yet.
        sol1 <- submitMockSolution logger db (teamId team) (ProblemId 0) $
            MockSolution [(1, "yellow")] [1]
        solutionStatus sol1 @?= Just JudgementValid
        solutionBonusOk sol1 @?= Just False

        -- Valid solution that unlocks the bonus we were missing before (and
        -- uses a bonus from problem 0)
        sol2 <- submitMockSolution logger db (teamId team) (ProblemId 1) $
            MockSolution [(0, "green")] [0]
        solutionStatus sol2 @?= Just JudgementValid
        solutionBonusOk sol2 @?= Just True

        -- The metagame bonus for problem 0 should now be marked as OK.
        sol1' <- getSolutionById db (teamId team) (solutionId sol1)
        solutionBonusOk sol1' @?= Just True

        -- Now we submit another solution for problem 0 that doesn't unlock
        -- the bonus in problem 1.
        sol3 <- submitMockSolution logger db (teamId team) (ProblemId 0) $
            MockSolution [] []
        solutionStatus sol3 @?= Just JudgementValid
        solutionBonusOk sol3 @?= Just True

        -- The metagame bonus for problem 1 should now be marked as not OK.
        sol2' <- getSolutionById db (teamId team) (solutionId sol2)
        solutionBonusOk sol2' @?= Just False

    , testCase "metagame chain" $ withLogger $ \logger -> withTmpDatabase $ \db -> do
        -- Sort of a chain where each puzzle unlocks a bonus in the next puzzle.
        team <- createExampleTeam1 db
        let num = 4
        insertProblems db
            [ Problem (ProblemId n) True $
                encodeText ([(n + 1, "blue") | n + 1 <= num] :: MockProblem)
            | n <- [0 .. num]
            ]

        sols <- for [0 .. num] $ \n ->
            submitMockSolution logger db (teamId team) (ProblemId n) $
                MockSolution [(n - 1, "blue") | n >= 1] [n + 1 | n + 1 <= num]

        for_ sols $ (@?= Just JudgementValid) . solutionStatus
        for_ sols $ (@?= Just True) . solutionBonusOk

        -- Break the chain.
        _ <- submitMockSolution logger db (teamId team) (ProblemId 0) $
            MockSolution [] []

        sols' <- for (drop 1 sols) $
            getSolutionById db (teamId team) . solutionId
        for_ sols' $ (@?= Just JudgementValid) . solutionStatus
        for_ sols' $ (@?= Just False) . solutionBonusOk

        -- Restore the chain.
        _ <- submitMockSolution logger db (teamId team) (ProblemId 0) $
            MockSolution [] [1]

        sols'' <- for sols $ getSolutionById db (teamId team) . solutionId
        for_ sols'' $ (@?= Just JudgementValid) . solutionStatus
        for_ sols'' $ (@?= Just True) . solutionBonusOk

    , testCase "scoreboard" $ withLogger $ \logger -> withTmpDatabase $ \db -> do
        sb0 <- getPublicScoreboard db
        [] @?= sb0

        team1 <- createExampleTeam1 db
        team2 <- createExampleTeam2 db

        insertProblems db
            [ Problem (ProblemId n) True . encodeText $
                SillyProblem [0, 2, 4, 6] 1
            | n <- [0 .. 2]
            ]

        refreshScoreboard logger db sillyProsecutor
        assertScoreboard logger db sillyProsecutor []

        Right sid1 <- submitSolution db (ProblemId 0) (teamId team1) $ encodeText $
            (6 :: SillySolution)
        prosecuteBySolutionId logger db sillyProsecutor sid1
        refreshScoreboard logger db sillyProsecutor
        assertScoreboard logger db sillyProsecutor
            [(teamId team1, teamName team1, 1000)]

        lsb <- getLightningScoreboard db
        lsb @?= [(teamId team1, teamName team1, 1000)]

        Right sid2 <- submitSolution db (ProblemId 0) (teamId team2) $ encodeText $
            (2 :: SillySolution)
        prosecuteBySolutionId logger db sillyProsecutor sid2
        assertScoreboard logger db sillyProsecutor
            [ (teamId team2, teamName team2, 1000)
            , (teamId team1, teamName team1, 655)
            ]

    , testCase "scoreboard frozen" $ withLogger $ \logger -> withTmpDatabase $ \db -> do
        now <- Time.getCurrentTime
        _ <- Pg.execute (dbConnection db)
            "UPDATE features SET scoreboard_freeze_at = ?" [now]
        insertProblems db
            [ Problem (ProblemId n) True . encodeText $
                SillyProblem [0, 2, 4, 6] 1
            | n <- [0 .. 2]
            ]
        team1 <- createExampleTeam1 db
        Right _ <- submitSolution db (ProblemId 0) (teamId team1) $
            encodeText (6 :: SillySolution)
        refreshScoreboard logger db sillyProsecutor
        assertScoreboard logger db sillyProsecutor []

    , testCase "lightning round ended" $ withLogger $ \logger -> withTmpDatabase $ \db -> do
        insertProblems db
            [ Problem (ProblemId n) True . encodeText $
                SillyProblem [0, 2, 4, 6] 1
            | n <- [0 .. 2]
            ]
        now <- Time.getCurrentTime
        _ <- Pg.execute (dbConnection db)
            "UPDATE features SET lightning_end_at = ?" [now]
        team1 <- createExampleTeam1 db
        Right _ <- submitSolution db (ProblemId 0) (teamId team1) $
            encodeText (6 :: SillySolution)
        refreshScoreboard logger db sillyProsecutor
        lsb <- getLightningScoreboard db
        lsb @?= []

    , testCase "puzzle difficulty" $ withLogger $ \logger -> withTmpDatabase $ \db -> do
        team1 <- createExampleTeam1 db
        insertProblems db
            [ Problem (ProblemId n) True . encodeText $
                SillyProblem [0, 2, 4, 6] $ 2 ^ n
            | n <- [2 .. 4]
            ]

        Right sid1 <- submitSolution db (ProblemId 2) (teamId team1) $ encodeText $
            (6 :: SillySolution)
        prosecuteBySolutionId logger db sillyProsecutor sid1
        refreshScoreboard logger db sillyProsecutor
        assertScoreboard logger db sillyProsecutor
            [ (teamId team1, teamName team1, 1000)
            ]

        Right sid2 <- submitSolution db (ProblemId 4) (teamId team1) $ encodeText $
            (6 :: SillySolution)
        prosecuteBySolutionId logger db sillyProsecutor sid2
        refreshScoreboard logger db sillyProsecutor
        assertScoreboard logger db sillyProsecutor
            [ (teamId team1, teamName team1, 2416)
            ]

    , testCase "summaries, minimal dislikes" $ withLogger $ \logger -> withTmpDatabase $ \db -> do
        team1 <- createExampleTeam1 db
        team2 <- createExampleTeam2 db
        insertProblems db
            [ Problem (ProblemId n) True . encodeText $
                SillyProblem [0, 2, 4, 6] $ 2 ^ n
            | n <- [2 .. 4]
            ]

        Right sid1 <- submitSolution db (ProblemId 2) (teamId team1) $ encodeText $
            (6 :: SillySolution)
        prosecuteBySolutionId logger db sillyProsecutor sid1
        Right sid2 <- submitSolution db (ProblemId 2) (teamId team2) $ encodeText $
            (4 :: SillySolution)
        prosecuteBySolutionId logger db sillyProsecutor sid2
        refreshScoreboard logger db sillyProsecutor

        summaries1 <- map removeContestMinDislikes <$>
            getEnabledProblemSummaries db (Just $ teamId team1)
        summaries1 @?=
            [ ProblemSummary (ProblemId 2) (Just (SolutionValid 6)) (Just 4) Nothing
            , ProblemSummary (ProblemId 3) Nothing Nothing Nothing
            , ProblemSummary (ProblemId 4) Nothing Nothing Nothing
            ]

        summaries2 <- map removeContestMinDislikes <$>
            getEnabledProblemSummaries db (Just $ teamId team2)
        summaries2 @?=
            [ ProblemSummary (ProblemId 2) (Just (SolutionValid 4)) (Just 4) Nothing
            , ProblemSummary (ProblemId 3) Nothing Nothing Nothing
            , ProblemSummary (ProblemId 4) Nothing Nothing Nothing
            ]
    ]
