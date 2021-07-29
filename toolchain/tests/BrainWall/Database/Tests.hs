{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BrainWall.Database.Tests
    ( tests

    , withTmpDatabase
    , createExampleTeam1
    , createExampleTeam2
    ) where

import           BrainWall.Database
import           Control.Exception          (Exception, SomeException, bracket,
                                             try)
import           Data.Foldable              (for_)
import           Data.IORef                 (newIORef)
import           Data.List                  (sort)
import           Data.Proxy                 (Proxy (..))
import           Data.String                (IsString (..))
import qualified Data.Text                  as T
import qualified Data.Time                  as Time
import qualified Database.Postgres.Temp     as Tmp
import qualified Database.PostgreSQL.Simple as Pg
import           System.Directory           (listDirectory)
import           System.FilePath            ((</>))
import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.HUnit           (Assertion, assertBool,
                                             assertFailure, testCase, (@?=))
import qualified Web.Users.Types            as Users

assertThrows
    :: forall e a. Exception e => Proxy e -> String -> IO a -> Assertion
assertThrows _ msg mx = do
    errOrX <- try mx
    case (errOrX :: Either e a) of
        Right _ -> assertFailure $ "Expected exception but succeeded: " <> msg
        Left _  -> pure ()

withTmpDatabase :: (Database -> IO a) -> IO a
withTmpDatabase f = do
    errOrX <- Tmp.with $ \tmp -> bracket
        (Pg.connectPostgreSQL (Tmp.toConnectionString tmp))
        Pg.close $ \conn -> do
            ref <- newIORef NoCachedFeatures
            let cfg = Config (Tmp.toConnectionString tmp) 0
                db = Database cfg conn ref
            initialize db
            migrations <- sort <$> listDirectory "db"
            for_ migrations $ \file -> do
                contents <- readFile $ "db" </> file
                Pg.execute_ conn $ fromString contents
            -- Clear submission rate in tests
            _ <- Pg.execute_ conn
                "UPDATE features SET submission_rate_seconds = NULL"
            f db
    either (fail . show) pure errOrX

createExampleTeam1 :: Database -> IO Team
createExampleTeam1 db = do
    tid <- createTeam db Users.User
        { Users.u_name     = "jasper"
        , Users.u_email    = "jasper@example.com"
        , Users.u_password = Users.makePassword $
            Users.PasswordPlain "hunter2"
        , Users.u_active   = True
        } >>= either (fail . show) pure
    getTeam db tid

createExampleTeam2 :: Database -> IO Team
createExampleTeam2 db = do
    tid <- createTeam db Users.User
        { Users.u_name     = "alex"
        , Users.u_email    = "alex@example.com"
        , Users.u_password = Users.makePassword $
            Users.PasswordPlain "hunter2"
        , Users.u_active   = True
        } >>= either (fail . show) pure
    getTeam db tid

tests :: TestTree
tests = testGroup "BrainWall.Database"
    [ testCase "create team" $ withTmpDatabase $ \db -> do
        assertBool "no teams" . null =<< getTeams db
        _ <- createExampleTeam1 db
        assertBool "one team" . (== 1) . length =<< getTeams db

    , testCase "features" $ withTmpDatabase $ \db -> do
        -- Test is reversed since we now enable all bonuses by default after
        -- the contest.
        features0 <- getFeatures db
        featuresAnyBonuses features0 @?= True

        mapM_ (Pg.execute_ (dbConnection db))
            [ "UPDATE features SET bonus_breakaleg_enable_at = NULL"
            , "UPDATE features SET bonus_globalist_enable_at = NULL"
            , "UPDATE features SET bonus_superflex_enable_at = NULL"
            , "UPDATE features SET bonus_wallhack_enable_at  = NULL"
            ]

        features1 <- getFeatures db
        featuresAnyBonuses features1 @?= False

    , testCase "submit and get solution" $ withTmpDatabase $ \db -> do
        team <- createExampleTeam1 db
        insertProblems db [Problem (ProblemId 0) True "2 + 2"]
        Right sid <- submitSolution db (ProblemId 0) (teamId team) "4"
        solution <- getSolutionByProblemId db (teamId team) (ProblemId 0)
        fmap solutionId solution @?= Just sid

    , testCase "enabling and disabling problems" $ withTmpDatabase $ \db -> do
        insertProblems db
            [ Problem (ProblemId 0) True  "2 + 2"
            , Problem (ProblemId 1) False "hidden"
            ]

        (@?= [ProblemId 0]) =<< getEnabledProblemIds db
        assertThrows (Proxy :: Proxy SomeException) "get problem 1" $
            getEnabledProblem db (ProblemId 1)

        team1 <- createExampleTeam1 db
        ((@?= 1) . length) =<< getEnabledProblemSummaries db
            (Just $ teamId team1)

        _ <- Pg.execute_ (dbConnection db)
            "UPDATE problems SET enabled = TRUE WHERE id = 1"

        (@?= [ProblemId 0, ProblemId 1]) =<< getEnabledProblemIds db
        (@?= Problem (ProblemId 1) True "hidden") =<<
            getEnabledProblem db (ProblemId 1)

    , testCase "submission end" $ withTmpDatabase $ \db -> do
        insertProblems db [Problem (ProblemId 0) True  "2 + 2"]
        Team {..} <- createExampleTeam1 db
        Right _ <- submitSolution db (ProblemId 0) teamId "4"
        now <- Time.getCurrentTime
        _ <- Pg.execute (dbConnection db)
            "UPDATE features SET submission_end_at = ?" [now]
        Left SubmissionEnded <- submitSolution db (ProblemId 0) teamId "4"
        pure ()

    , testCase "submission size limit" $ withTmpDatabase $ \db -> do
        Features {..} <- getFeatures db
        insertProblems db [Problem (ProblemId 0) True  "2 + 2"]
        Team {..} <- createExampleTeam1 db
        Right _ <- submitSolution db (ProblemId 0) teamId $
            T.replicate (featuresSubmissionLimitKb * 1024) "a"
        Left (SizeLimited _) <- submitSolution db (ProblemId 0) teamId $
            T.replicate (featuresSubmissionLimitKb * 1024 + 1) "b"
        pure ()
    ]
